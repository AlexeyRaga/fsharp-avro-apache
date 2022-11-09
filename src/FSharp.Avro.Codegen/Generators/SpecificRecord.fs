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
    let ofField (fld : Field) =
        let typ = schemaType fld.Schema
        let fid = Ident.Prefixed("__", Ident.Create(fld.Name))
        let pid = Ident.Create(fld.Name)

        { field = fld
          privateFieldId = fid
          propertyId = pid
          typ = typ }

type RecordRepresentation =
    | Record
    | Class

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

    let private setFixedField setter fld =
        let isType =
            SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst(SynType.CreateArray(SynType.Byte)), SynPat.CreateNamed "x"))

        let pattern = SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]
        let expr = setter fld (SynExpr.CreateApp(SynExpr.Create fld.field.Schema.Fullname, SynExpr.Create "x"))
        SynMatchClause.Create(pattern, expr)

    let implementInterface (thisIdent : Ident) (typeName : Ident) (fields : RecordField list) (setter : RecordField -> SynExpr -> SynExpr) =
        let methodAttrs = [ SynAttributeList.Create [ SynAttribute.NotForFSharp() ] ]

        let methodGet =
            let parameters = [ SynPat.CreateTyped("pos", SynType.Int) ]

            let mkCase fld =
                match fld.field.Schema with
                | :? EnumSchema as s ->
                    let convert value =
                        SynExpr.MethodCall(SynExpr.Create s.Fullname, SpecificEnum.toAvroIdent, args = value) |-> SynExpr.Box

                    [ getterMatchCase fld convert ]
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
                            value |-> SynExpr.OptionMap(SynExpr.Create "box") |-> SynExpr.OptionDefault SynExpr.CreateNull

                        [ getterMatchCase fld getBoxedValue ]
                    | UnionCases xs ->
                        let count = List.length xs

                        let cases =
                            xs
                            |> Seq.mapi (fun num _ ->
                                SynMatchClause.Create(SynPat.Choice(num + 1, count, valueIdent), SynExpr.Boxed(valueExpr)))
                            |> flip Seq.append [ SynMatchClause.OtherwiseNull ]
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

            let cases = SynExpr.CreateMatch(SynExpr.Create "pos", List.collect mkCase fields)
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Get", cases, parameters, attributes = methodAttrs)

        let methodPut =
            let parameters =
                [ SynPat.CreateTyped("pos", SynType.Int)
                  SynPat.CreateTyped("value", SynType.Object) ]

            let mkCase fld =
                let setMatching = setMatchUnionCase setter fld
                let setOtherwise = setWildUnionCase setter fld

                match fld.field.Schema with
                | :? PrimitiveSchema -> [ setCastedValue setter fld ]
                | :? FixedSchema -> [ setCastedValue setter fld ]
                | :? LogicalSchema -> [ setCastedValue setter fld ]
                | :? RecordSchema -> [ setCastedValue setter fld ]
                | :? EnumSchema as s ->
                    let convert value =
                        SynExpr.MethodCall(SynExpr.Create s.Fullname, SpecificEnum.fromAvroIdent, args = value)

                    [ setMatching SynType.Int convert ]
                | :? ArraySchema as s ->
                    let typ = SynType.Paren(SynType.Seq(schemaType s.ItemSchema, isPostfix = true), range0)
                    [ setMatching typ (fun x -> x |-> SynExpr.MethodCall(SynLongIdent.Create "Array.ofSeq")) ]
                | :? MapSchema as s ->
                    let typ = SynType.IDictionary(SynType.String, schemaType s.ValueSchema)
                    let unwrapKV = SynExpr.SeqMap(SynExpr.CreateParen(SynExpr.Create "|KeyValue|"))

                    let convert x =
                        SynExpr.MethodCall(SynLongIdent.Create "Map.ofSeq", SynExpr.CreateParen(SynExpr.CreateApp(unwrapKV, [ x ])))

                    [ setMatching typ convert ]
                | :? UnionSchema as union ->
                    match union with
                    | UnionEmpty -> [ SynMatchClause.Create(wildPat fld, setter fld SynExpr.CreateUnit) ]
                    | UnionSingle (_, x) -> [ setMatching (schemaType x) SynExpr.Some ]
                    | UnionSingleOptional (_, x) -> [ setMatching (schemaType x) SynExpr.Some; setOtherwise SynExpr.None ]
                    | UnionCases xs ->
                        let count = List.length xs

                        xs
                        |> Seq.mapi (fun ix (_, x) -> setMatching (schemaType x) (choice (ix + 1) count >> typedAs fld.typ))
                        |> List.ofSeq
                    | UnionOptionalCases xs ->
                        let count = List.length xs

                        xs
                        |> Seq.mapi (fun ix (_, x) -> setMatching (schemaType x) (choice (ix + 1) count >> SynExpr.Some >> typedAs fld.typ))
                        |> flip Seq.append [ setOtherwise SynExpr.None ]
                        |> List.ofSeq
                | _ -> [ SynMatchClause.Create(wildPat fld, SynExpr.Failwith $"Not implemented: {fld.field.Name}: {fld.field.Schema.Tag}") ]

            let cases =
                SynExpr.CreateMatch(SynExpr.CreateTuple [ SynExpr.Create "pos"; SynExpr.Create "value" ], List.collect mkCase fields)

            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Put", cases, parameters, attributes = methodAttrs)

        let propSchema =
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Schema", SynExpr.Create [ typeName; schemaStaticMemberIdent ])

        SynMemberDefn.Interface(
            SynType.Create("Avro.Specific.ISpecificRecord"),
            Some range0,
            Some [ methodGet; methodPut; propSchema ],
            range0
        )

module SpecificRecord =
    let createClass (typeName : Ident) (schema : RecordSchema) (fields : RecordField list) =
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

        let specRec = ISpecificRecord.implementInterface thisIdent typeName fields directFieldSetter

        let equatable =
            IEquatable.implementInterface (thisIdent, SynType.Create typeName, fields |> List.map (fun x -> x.propertyId))

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

    let createRecord (typeName : Ident) (schema : RecordSchema) (fields : RecordField list) =
        let recordFields = fields |> Seq.map (fun x -> SynField.Create(x.typ, x.propertyId, xmldoc = PreXmlDoc.Create x.field.Documentation))
        let specRec = ISpecificRecord.implementInterface thisIdent typeName fields ISpecificRecord.reflectionPropertySetter

        SynTypeDefn.CreateRecord(
            typeName,
            recordFields,
            attributes = [ SynAttributeList.Create [ SynAttribute.CLIMutable ] ],
            members = [ specRec; schemaStaticMember schema ],
            xmldoc = PreXmlDoc.Create schema.Documentation
        )

    let genSpecificRecord (repr : RecordRepresentation) (schema : RecordSchema) =
        let typeName = Ident.Create schema.Name

        let generator =
            match repr with
            | Record -> createRecord
            | Class -> createClass

        let typ = schema.Fields |> Seq.map RecordField.ofField |> List.ofSeq |> generator typeName schema
        let decl = SynModuleDecl.Types([ typ ], range0)

        { Namespace = schema.Namespace
          Declarations = [ decl ] }
