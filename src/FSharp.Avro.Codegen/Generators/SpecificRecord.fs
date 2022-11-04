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
    let directFieldSetter (target : RecordField) (value : SynExpr) =
        let target = SynExpr.CreateIdent target.privateFieldId
        SynExpr.Set(target, value, range0)

    let reflectionPropertySetter (target : RecordField) (value : SynExpr) =
        let propName = SynExpr.CreateConst(SynConst.CreateString target.propertyId.idText)
        let thisType = SynExpr.CreateInstanceMethodCall(thisExpr, Ident.Create "GetType", SynExpr.CreateUnit)
        let propExpr = SynExpr.CreateInstanceMethodCall(thisType, Ident.Create "GetProperty", SynExpr.CreateParen propName)
        SynExpr.CreateInstanceMethodCall(propExpr, SynLongIdent.Create("SetMethod.Invoke"), SynExpr.EnsureParen value)

    let private setCastedValue setter fld =
        let isType = SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst(fld.typ), SynPat.CreateNamed "x"))
        let pattern = SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]
        let expr = setter fld (SynExpr.CreateIdent "x")
        SynMatchClause.Create(pattern, None, expr)

    let private setFixedField setter fld =
        let isType =
            SynPat.CreateParen(SynPat.CreateAs(SynPat.CreateIsInst(SynType.CreateArray(SynType.Byte)), SynPat.CreateNamed "x"))

        let pattern = SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); isType ]
        let expr = setter fld (SynExpr.CreateApp(SynExpr.CreateLongIdent fld.field.Schema.Fullname, SynExpr.CreateIdent "x"))
        SynMatchClause.Create(pattern, None, expr)

    let implementInterface (thisIdent : Ident) (typeName : Ident) (fields : RecordField list) (setter : RecordField -> SynExpr -> SynExpr) =
        let methodAttrs = [ SynAttributeList.Create [ SynAttribute.NotForFSharp ] ]

        let methodGet =
            let parameters = [ SynPat.CreateTyped("pos", SynType.Int) ]
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "Get", SynExpr.CreateUnit, parameters, attributes = methodAttrs)

        let methodPut =
            let parameters = [ SynPat.CreateTyped("pos", SynType.Int); SynPat.CreateTyped("value", SynType.Object) ]

            let mkCase fld =
                match fld.field.Schema with
                | :? PrimitiveSchema -> setCastedValue setter fld
                | :? FixedSchema -> setCastedValue setter fld
                | _ ->
                    let pat = SynPat.CreateTuple [ SynPat.CreateConst(SynConst.Int32 fld.field.Pos); SynPat.CreateWild ]
                    SynMatchClause.Create(pat, None, SynExpr.CreateFailwith "Not implemented")

            let cases =
                SynExpr.CreateMatch(SynExpr.CreateTuple [ SynExpr.CreateIdent "pos"; SynExpr.CreateIdent "value" ], List.map mkCase fields)

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
