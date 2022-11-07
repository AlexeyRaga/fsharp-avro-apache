namespace FSharp.Avro.Codegen.Generators

open Avro
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open Microsoft.FSharp.Core
open FSharp.Avro.Codegen
open FSharp.Avro.Codegen.Schema

type RecordField =
    { field : Field
      privateFieldId : Ident
      propertyId : Ident
      typ : SynType }

[<RequireQualifiedAccess>]
module ISpecificRecord =
    let private choice (n : int) (ofN : int) (expr : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdent $"Choice{n}Of{ofN}", expr)

    let private typedAs (typ: SynType) (expr: SynExpr) =
        SynExpr.CreateTyped(expr, typ) |> SynExpr.EnsureParen

    let private wildPat fld =
        SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); SynPat.CreateWild ]

    let private castPat fld typ =
        let isType = SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst typ, SynPat.CreateNamed "x"))
        SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]

    let directFieldSetter (target : RecordField) (value : SynExpr) =
        let target = SynExpr.CreateIdent target.privateFieldId
        SynExpr.Set(target, value, range0)

    let reflectionPropertySetter (target : RecordField) (value : SynExpr) =
        let propName = SynExpr.CreateConst(SynConst.CreateString target.propertyId.idText)
        let thisType = SynExpr.CreateInstanceMethodCall(thisExpr, Ident.Create "GetType", SynExpr.CreateUnit)
        let propExpr = SynExpr.CreateInstanceMethodCall(thisType, Ident.Create "GetProperty", SynExpr.CreateParen propName)
        let invokeArgs = SynExpr.CreateParen(SynExpr.CreateTuple [ thisExpr; SynExpr.ArrayOrListComputed(true, value, range0) ])
        SynExpr.CreateInstanceMethodCall(propExpr, SynLongIdent.Create("SetMethod.Invoke"), invokeArgs) |> SynExpr.Ignore

    let private setMatchUnionCase setter fld typ mkValue =
        let lambdaVar = Ident.Create "x"
        let isType = SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst typ, SynPat.CreateNamed lambdaVar))
        let pattern = SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]
        SynMatchClause.Create(pattern, (setter fld (mkValue (SynExpr.CreateIdent lambdaVar))))

    let private setWildUnionCase setter fld value =
        SynMatchClause.Create(wildPat fld, (setter fld value))

    let private setCastedValue setter fld = setMatchUnionCase setter fld fld.typ id

    let private setFixedField setter fld =
        let isType =
            SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst(SynType.CreateArray(SynType.Byte)), SynPat.CreateNamed "x"))

        let pattern = SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]
        let expr = setter fld (SynExpr.CreateApp(SynExpr.CreateLongIdent fld.field.Schema.Fullname, SynExpr.CreateIdent "x"))
        SynMatchClause.Create(pattern, expr)

    let implementInterface (thisIdent : Ident) (typeName : Ident) (fields : RecordField list) (setter : RecordField -> SynExpr -> SynExpr) =
        let methodAttrs = [ SynAttributeList.Create [ SynAttribute.NotForFSharp ] ]

        let methodGet =
            let parameters = [ SynPat.CreateTyped("pos", SynType.Int) ]
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Get", SynExpr.CreateUnit, parameters, attributes = methodAttrs)

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
                | :? EnumSchema as s ->
                    let convert value =
                        SynExpr.CreateInstanceMethodCall(SynExpr.CreateLongIdent s.Fullname, Ident.Create "FromInt", args = value)

                    [ setMatching SynType.Int convert ]
                | :? ArraySchema as s ->
                    let typ = SynType.Paren(SynType.CreateArray(schemaType s.ItemSchema), range0)
                    [ setMatching typ id ]
                | :? MapSchema as s ->
                    let typ = SynType.IDictionary(SynType.String, schemaType s.ValueSchema)
                    let unwrapKV = SynExpr.CreateSeqMap(SynExpr.CreateParen(SynExpr.CreateIdent "|KeyValue|"))

                    let convert x =
                        SynExpr.CreateInstanceMethodCall(
                            SynLongIdent.Create "Map.ofSeq",
                            SynExpr.CreateParen(SynExpr.CreateApp(unwrapKV, [ x ]))
                        )

                    [ setMatching typ convert ]
                | :? UnionSchema as union ->
                    match union with
                    | UnionEmpty -> [ SynMatchClause.Create(wildPat fld, setter fld SynExpr.CreateUnit) ]
                    | UnionSingle (_, x) -> [ setMatching (schemaType x) SynExpr.CreateSome ]
                    | UnionSingleOptional (_, x) ->
                        [ setMatching (schemaType x) SynExpr.CreateSome
                          setOtherwise SynExpr.CreateNone ]
                    | UnionCases xs ->
                        let count = List.length xs
                        xs |> Seq.mapi (fun ix (_, x) -> setMatching (schemaType x) (choice (ix + 1) count >> typedAs fld.typ)) |> List.ofSeq
                    | UnionOptionalCases xs ->
                        let count = List.length xs

                        xs
                        |> Seq.mapi (fun ix (_, x) -> setMatching (schemaType x) (choice (ix + 1) count >> SynExpr.CreateSome >> typedAs fld.typ))
                        |> flip Seq.append [ setOtherwise SynExpr.CreateNone ]
                        |> List.ofSeq
                | _ ->
                    [ SynMatchClause.Create(
                          wildPat fld,
                          SynExpr.CreateFailwith $"Not implemented: {fld.field.Name}: {fld.field.Schema.Tag}"
                      ) ]

            let cases =
                SynExpr.CreateMatch(
                    SynExpr.CreateTuple [ SynExpr.CreateIdent "pos"; SynExpr.CreateIdent "value" ],
                    List.collect mkCase fields
                )

            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Put", cases, parameters, attributes = methodAttrs)

        let propSchema =
            SynMemberDefn.InstanceMember(
                thisIdent,
                Ident.Create "Schema",
                SynExpr.CreateApp(
                    SynExpr.CreateLongIdent "Avro.Schema.Parse",
                    SynExpr.CreateParen(SynExpr.CreateLongIdent($"{typeName.idText}.SCHEMA"))
                )
            )

        SynMemberDefn.Interface(
            SynType.Create("Avro.Specific.ISpecificRecord"),
            Some range0,
            Some [ methodGet; methodPut; propSchema ],
            range0
        )

module SpecificRecord =
    let createClass (typeName : Ident) (schema : Schema) (fields : RecordField list) =
        let defaultCtor = SynMemberDefn.DefaultCtor(fields |> List.map (fun x -> (x.propertyId, x.typ)))
        let unsafeCtor = SynMemberDefn.CreateUnsafeCtor(typeName, fields |> List.map (fun x -> x.typ))

        let privateFields, props =
            fields
            |> List.map (fun x -> SynMemberDefn.CreateGetterForField(thisIdent, x.typ, x.privateFieldId, x.propertyId))
            |> List.unzip

        let specRec = ISpecificRecord.implementInterface thisIdent typeName fields ISpecificRecord.directFieldSetter

        let equatable =
            IEquatable.implementInterface (thisIdent, SynType.Create typeName, fields |> List.map (fun x -> x.propertyId))

        SynTypeDefn.CreateClass(
            typeName,
            members =
                [ yield defaultCtor
                  yield! privateFields
                  yield unsafeCtor
                  yield! props
                  yield schemaMember schema
                  yield specRec
                  yield! equatable ],
            attributes = [ SynAttributeList.Create(SynAttribute.Sealed) ]
        )

    let createRecord (typeName : Ident) (schema : Schema) (fields : RecordField list) =
        let recordFields = fields |> Seq.map (fun x -> SynField.Create(x.typ, x.propertyId))
        let specRec = ISpecificRecord.implementInterface thisIdent typeName fields ISpecificRecord.reflectionPropertySetter

        SynTypeDefn.CreateRecord(
            typeName,
            recordFields,
            attributes = [ SynAttributeList.Create [ SynAttribute.CLIMutable ] ],
            members = [ specRec; schemaMember schema ]
        )
