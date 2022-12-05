namespace FSharp.Avro.Codegen.Generators

open Avro
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open Microsoft.FSharp.Core
open FSharp.Avro.Codegen
open FSharp.Avro.Codegen.Schema

type RecordField =
    { field : Field
      privateFieldId : Ident
      propertyId : Ident
      typ : SynType }

module RecordField =
    let ofField (parameters : GenParams) (fld : Field) =
        let typ = schemaType parameters fld.Schema
        let fid = Ident.Prefixed("__", Ident.Create(fld.Name))
        let pid = Ident.Create(fld.Name)

        { field = fld
          privateFieldId = fid
          propertyId = pid
          typ = typ }

[<RequireQualifiedAccess>]
module ISpecificRecord =
    let private choice (n : int) (ofN : int) (expr : SynExpr) = SynExpr.Choice(n, ofN, expr)

    let private typedAs (typ : SynType) (expr : SynExpr) =
        SynExpr.CreateTyped(expr, typ) |> SynExpr.EnsureParen

    let private wildPat fld =
        SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); SynPat.CreateWild ]

    let private castPat fld typ =
        let isType = SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst typ, SynPat.CreateNamed "x"))
        SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]

    let reflectionPropertySetter (target : RecordField) (value : SynExpr) =
        let propName = SynExpr.CreateConst(SynConst.CreateString target.propertyId.idText)
        let thisType = SynExpr.MethodCall(thisExpr, Ident.Create "GetType", SynExpr.CreateUnit)
        let propExpr = SynExpr.MethodCall(thisType, Ident.Create "GetProperty", SynExpr.CreateParen propName)
        let invokeArgs = SynExpr.CreateParen(SynExpr.CreateTuple [ thisExpr; SynExpr.ArrayOrListComputed(true, value, range0) ])
        SynExpr.MethodCall(propExpr, SynLongIdent.Create("SetMethod.Invoke"), invokeArgs) |> SynExpr.Ignore

    let private getterMatchCase (fld : RecordField) transform =
        let pattern = SynPat.CreateConst(SynConst.Int32 fld.field.Pos)
        let boxedValue = transform (SynExpr.Create [ thisIdent; fld.propertyId ])
        SynMatchClause.Create(pattern, boxedValue)

    let private setMatchUnionCase setter fld typ mkValue =
        let lambdaVar = Ident.Create "x"
        let isType = SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst typ, SynPat.CreateNamed lambdaVar))
        let pattern = SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]
        SynMatchClause.Create(pattern, (setter fld (mkValue (SynExpr.Create lambdaVar))))

    let private setWildUnionCase setter fld value =
        SynMatchClause.Create(wildPat fld, (setter fld value))

    let private setCastedValue setter fld = setMatchUnionCase setter fld fld.typ id

    let implementInterface (parameters : GenParams) (thisIdent : Ident) (typeName : Ident) (fields : RecordField list) (setter : RecordField -> SynExpr -> SynExpr) =
        let methodAttrs = [ SynAttributeList.Create [ SynAttribute.NotForFSharp() ] ]
        let posIdent = Ident.Create "pos"

        let methodGet =
            let methodParameters = [ SynPat.CreateTyped(posIdent, SynType.Int) ]

            let mkCase fld =
                match fld.field.Schema with
                | :? MapSchema ->
                    let convert value =
                        value
                        |-> SynExpr.MethodCall(SynLongIdent.Create "Map.toSeq")
                        |-> SynExpr.Create "dict"
                        |-> SynExpr.Create "System.Collections.Generic.Dictionary"
                        |-> SynExpr.Box

                    [ SynMatchClause.Create(
                          SynPat.Int32 fld.field.Pos,
                          SynExpr.CreateNull,
                          whenExp = SynExpr.IsNull(SynExpr.Create [ thisIdent; fld.propertyId ])
                      )
                      getterMatchCase fld convert ]
                | :? UnionSchema as s ->
                    match s with
                    | UnionEmpty -> [ getterMatchCase fld (fun _ -> SynExpr.CreateNull) ]
                    | UnionSingle _ -> [ getterMatchCase fld SynExpr.Boxed ]
                    | UnionSingleOptional _ ->
                        let getBoxedValue value =
                            value |-> SynExpr.OptionMap SynExpr.Box |-> SynExpr.OptionDefault SynExpr.CreateNull

                        [ getterMatchCase fld getBoxedValue ]
                    | UnionCases xs ->
                        let count = List.length xs

                        let cases =
                            xs
                            |> Seq.mapi (fun num _ ->
                                SynMatchClause.Create(SynPat.Choice(num + 1, count, valueIdent), SynExpr.Boxed valueExpr))
                            |> Seq.append [ SynMatchClause.Create(valuePat, SynExpr.CreateNull, whenExp = SynExpr.IsNull valueExpr) ]
                            |> List.ofSeq

                        [ getterMatchCase fld (fun x -> SynExpr.CreateMatch(x, cases)) ]
                    | UnionOptionalCases xs ->
                        let count = List.length xs

                        let cases =
                            xs
                            |> Seq.mapi (fun num _ ->
                                SynMatchClause.Create(SynPat.Some(SynPat.Choice(num + 1, count, valueIdent)), SynExpr.Boxed valueExpr))
                            |> flip Seq.append [ SynMatchClause.OtherwiseNull ]
                            |> List.ofSeq

                        [ getterMatchCase fld (fun x -> SynExpr.CreateMatch(x, cases)) ]
                | _ -> [ getterMatchCase fld SynExpr.Boxed ]

            let cases =
                fields
                |> Seq.collect mkCase
                |> flip Seq.append [ SynMatchClause.OtherwiseRaise(SynExpr.AvroRuntimeException(posIdent, "Get()")) ]

            let matchExpr = SynExpr.CreateMatch(SynExpr.Create posIdent, List.ofSeq cases)
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Get", matchExpr, methodParameters, attributes = methodAttrs)

        let methodPut =
            let methodParameters =
                [ SynPat.CreateTyped(posIdent, SynType.Int)
                  SynPat.CreateTyped(valueIdent, SynType.Object) ]

            let mkCase fld =
                let setMatching = setMatchUnionCase setter fld
                let setOtherwise = setWildUnionCase setter fld

                let packValue (schema : Schema) =
                    match schema with
                    | :? EnumSchema as s ->
                        [ schemaType parameters schema, id
                          SynType.Int, (fun x -> SynExpr.EnsureParen(SynExpr.CreateApp(SynExpr.Create "enum", [ x ]))) ]
                    | _ -> [ schemaType parameters schema, id ]

                match fld.field.Schema with
                | :? PrimitiveSchema -> [ setCastedValue setter fld ]
                | :? FixedSchema -> [ setCastedValue setter fld ]
                | :? LogicalSchema -> [ setCastedValue setter fld ]
                | :? RecordSchema -> [ setCastedValue setter fld ]
                | :? EnumSchema -> packValue fld.field.Schema |> List.map (fun (typ, transform) -> setMatching typ transform)
                | :? ArraySchema as s ->
                    let typ = SynType.Paren(SynType.Seq(schemaType parameters s.ItemSchema, isPostfix = true), range0)
                    [ setMatching typ (fun x -> x |-> SynExpr.MethodCall(SynLongIdent.Create "Array.ofSeq")) ]
                | :? MapSchema as s ->
                    let typ = SynType.IDictionary(SynType.String, schemaType parameters s.ValueSchema)
                    let unwrapKV = SynExpr.SeqMap(SynExpr.CreateParen(SynExpr.Create "|KeyValue|"))

                    let convert x =
                        SynExpr.MethodCall(SynLongIdent.Create "Map.ofSeq", SynExpr.CreateParen(SynExpr.CreateApp(unwrapKV, [ x ])))

                    [ setMatching typ convert ]
                | :? UnionSchema as union ->
                    match union with
                    | UnionEmpty -> [ SynMatchClause.Create(wildPat fld, setter fld SynExpr.CreateUnit) ]
                    | UnionSingle(_, x) -> [ setMatching (schemaType parameters x) SynExpr.Some ]
                    | UnionSingleOptional(_, x) -> [ setMatching (schemaType parameters x) SynExpr.Some; setOtherwise SynExpr.None ]
                    | UnionCases xs ->
                        let count = List.length xs

                        xs
                        |> Seq.mapi (fun ix (_, x) ->
                            packValue x
                            |> Seq.map (fun (matchType, transformer) ->
                                (setMatching matchType (transformer >> choice (ix + 1) count >> typedAs fld.typ))))
                        |> Seq.collect id
                        |> List.ofSeq
                    | UnionOptionalCases xs ->
                        let count = List.length xs

                        xs
                        |> Seq.mapi (fun ix (_, x) ->
                            packValue x
                            |> Seq.map (fun (matchType, transformer) ->
                                (setMatching matchType (transformer >> choice (ix + 1) count >> SynExpr.Some >> typedAs fld.typ))))
                        |> Seq.collect id
                        |> flip Seq.append [ setOtherwise SynExpr.None ]
                        |> List.ofSeq
                | _ -> [ SynMatchClause.Create(wildPat fld, SynExpr.Failwith $"Not implemented: {fld.field.Name}: {fld.field.Schema.Tag}") ]

            let cases =
                fields
                |> Seq.collect mkCase
                |> flip Seq.append [ SynMatchClause.OtherwiseRaise(SynExpr.AvroRuntimeException(posIdent, "Put()")) ]

            let matchExpr = SynExpr.CreateMatch(SynExpr.CreateTuple [ SynExpr.Create posIdent; valueExpr ], List.ofSeq cases)
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Put", matchExpr, methodParameters, attributes = methodAttrs)

        let propSchema =
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Schema", SynExpr.Create [ typeName; schemaStaticMemberIdent ])

        SynMemberDefn.Interface(
            SynType.Create("Avro.Specific.ISpecificRecord"),
            Some range0,
            Some [ methodGet; methodPut; propSchema ],
            range0
        )

module SpecificRecord =
    let createClass (parameters : GenParams) (typeName : Ident) (schema : RecordSchema) (fields : RecordField list) =
        let defaultCtor = SynMemberDefn.DefaultCtor(fields |> List.map (fun x -> (x.propertyId, x.typ)))
        let unsafeCtor = SynMemberDefn.UnsafeCtor(typeName, fields |> List.map (fun x -> x.typ))

        let directFieldSetter (target : RecordField) (value : SynExpr) =
            SynExpr.Set(SynExpr.Create target.privateFieldId, value, range0)

        let privateFields, props =
            fields
            |> List.map (fun x ->
                SynMemberDefn.GetterForField(
                    thisIdent,
                    x.typ,
                    x.privateFieldId,
                    x.propertyId,
                    xmldoc = PreXmlDoc.Create x.field.Documentation
                ))
            |> List.unzip

        let specRec = ISpecificRecord.implementInterface parameters thisIdent typeName fields directFieldSetter

        let equatable =
            IEquatable.implementInterface (thisIdent, SynType.Create typeName, fields |> List.map (fun x -> x.propertyId))

        let recordType =
            SynModuleDecl.CreateType(
                SynTypeDefn.CreateClass(
                    typeName,
                    members =
                        [ yield defaultCtor
                          yield! privateFields
                          yield unsafeCtor
                          yield! props
                          yield schemaStaticMember schema
                          yield specRec
                          yield! equatable ],
                    attributes = [ SynAttributeList.Create(SynAttribute.Sealed) ],
                    xmldoc = PreXmlDoc.Create schema.Documentation
                )
            )

        [ recordType ]

    let createRecord (parameters : GenParams) (typeName : Ident) (schema : RecordSchema) (fields : RecordField list) =
        let recordFields =
            fields |> Seq.map (fun x -> SynField.Create(x.typ, x.propertyId, xmldoc = PreXmlDoc.Create x.field.Documentation))

        let reflectionSetter (target : RecordField) (value : SynExpr) =
            let setterName = SynLongIdent.Create [ typeName; Ident.Prefixed("set_", target.propertyId) ]
            SynExpr.MethodCall(SynExpr.Create setterName, Ident.Create "Invoke", SynExpr.CreateTuple [ thisExpr; value ])

        let specRec = ISpecificRecord.implementInterface parameters thisIdent typeName fields reflectionSetter

        let companionModule =
            let mkSetter =
                let typeParam = SynTypar.Create "T"
                let typarDecls = SynTyparDecls.CreatePostfix [ typeParam ]

                let pat =
                    SynPat.Create(
                        "mkSetter",
                        [ SynPat.CreateParen(SynPat.CreateTyped("name", SynType.String)) ],
                        typarDecls = SynValTyparDecls(Some typarDecls, false)
                    )

                let expr =
                    let fullTypeName = parameters |> GenParams.fullTypeName schema.SchemaName |> SynType.Create
                    let actionTyp = SynType.CreateApp(SynType.Create "System.Action", [ fullTypeName; SynType.CreateVar typeParam ])
                    let prop = SynExpr.MethodCall(SynExpr.TypeOf fullTypeName, SynLongIdent.Create "GetProperty", SynExpr.Create "name")
                    let meth = SynExpr.MethodCall(prop, Ident.Create "GetSetMethod", SynExpr.CreateUnit)

                    let del =
                        SynExpr.MethodCall(
                            SynLongIdent.Create "System.Delegate.CreateDelegate",
                            SynExpr.CreateTuple [ SynExpr.TypeOf actionTyp; meth ]
                        )

                    SynExpr.CreateDowncast(del, actionTyp)

                SynModuleDecl.CreateLet(pattern = pat, expr = expr, access = SynAccess.Private(range0))

            let setters =
                fields
                |> List.map (fun x ->
                    let propNameExpr = SynExpr.String x.propertyId.idText
                    let pat = SynPat.Create(Ident.Prefixed("set_", x.propertyId), [])
                    let expr = SynExpr.CreateApp(SynExpr.CreateTypeApp(SynExpr.Create "mkSetter", [ x.typ ]), [ propNameExpr ])
                    SynModuleDecl.CreateLet(pattern = pat, expr = expr))

            SynModuleDecl.CreateNestedModule(
                SynComponentInfo.Create(id = [ typeName ], access = SynAccess.Internal(range0)),
                mkSetter :: setters
            )

        let recordType =
            SynModuleDecl.CreateType(
                SynTypeDefn.CreateRecord(
                    typeName,
                    recordFields,
                    attributes = [ SynAttributeList.Create [ SynAttribute.CLIMutable ] ],
                    members = [ specRec; schemaStaticMember schema ],
                    xmldoc = PreXmlDoc.Create schema.Documentation
                )
            )

        [ companionModule; recordType ]

    let genSpecificRecord (parameters : GenParams) (schema : RecordSchema) =
        let typeName = Ident.Create schema.Name

        let generator =
            match parameters.RecordRepr with
            | Record -> createRecord
            | Class -> createClass

        let decls = schema.Fields |> Seq.map (RecordField.ofField parameters) |> List.ofSeq |> generator parameters typeName schema

        { Namespace = parameters |> GenParams.mappedNamespace schema.Namespace
          Declarations = decls }
