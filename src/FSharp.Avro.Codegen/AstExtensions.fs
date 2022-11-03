[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Avro.Codegen.AstExtensions

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type SynExpr with
    static member CreateDowncastLambda(typ : SynType) =
        SynExpr.CreateLambda(Ident.Create "x", SynExpr.Downcast(SynExpr.CreateIdent "x", typ, range0))

    static member CreateDowncastOptionPipeline(typ : SynType) =
        SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))

    static member CreateDowncastOption(expr : SynExpr, typ : SynType) =
        SynExpr.CreatePipeRight(
            expr,
            SynExpr.CreatePipeRight(SynExpr.CreateOptionOfObj(), SynExpr.CreateOptionMap(SynExpr.CreateDowncastLambda typ))
        )

    static member CreateFailwith(err : string) =
        SynExpr.CreateApp(SynExpr.CreateIdent "failwith", SynExpr.CreateConst(SynConst.CreateString err))

    static member UncheckedDefault(typ : SynType) =
        SynExpr.CreateInstanceMethodCall(SynLongIdent.Create("Unchecked.defaultof"), [ typ ])


type SynMatchClause with
    static member CreateOtherwiseFailwith(errorMessage : string) =
        SynMatchClause.Create(SynPat.CreateWild, None, SynExpr.CreateFailwith errorMessage)

type SynMemberDefn with

    static member DefaultCtor(parameters : (Ident * SynType) list) =
        let ctorPats = parameters |> List.map SynSimplePat.CreateTyped
        SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, SynSimplePats.Create ctorPats, None, PreXmlDoc.Empty, range0)

    static member CreateUnsafeCtor(typeName : Ident, types : SynType list) =
        let unsafeArgs = types |> List.map SynExpr.UncheckedDefault

        let callCtor = SynExpr.CreateApp(SynExpr.Ident typeName, unsafeArgs)

        SynMemberDefn.Member(
            SynBinding.Create(
                SynMemberFlags.Create(SynMemberKind.Constructor),
                SynPat.CreateLongIdent("new", [ SynPat.CreateUnit ]),
                callCtor,
                attributes = [ SynAttributeList.Create SynAttribute.NotForFSharp ]
            ),
            range0
        )

    static member CreatePropertyWithPrivateSetter(thisIdent : Ident, typ : SynType, fieldIdent : Ident, propIdent : Ident) =
        let fld = SynMemberDefn.Let(isMutable = true, pattern = SynPat.CreateNamed fieldIdent, expr = SynExpr.CreateIdent propIdent)
        let prop = SynMemberDefn.CreateGetSetProperty(thisIdent, propIdent, fieldIdent, setterAccess = SynAccess.Private(range0))
        fld, prop

    static member CreateEqualityMembers(thisIdent : Ident) = ()

    static member CreateIEquatable(thisIdent : Ident, thisType : SynType, props : Ident list) =
        let interfaceMember = { SynMemberFlags.InstanceMember with IsOverrideOrExplicitImpl = true }
        let thisMember name = SynLongIdent.Create [ thisIdent; name ]

        let equalsMethodIdent = Ident.Create "Equals"
        let equatableType = SynType.CreateApp(SynType.Create("System.IEquatable"), [thisType])

        let equalExps =
            let other = Ident.Create "other"
            let otherProp prop = SynExpr.CreateLongIdent (SynLongIdent.Create [other; prop; ])
            let thisProp prop = SynExpr.CreateLongIdent (thisMember prop)

            props
            |> List.map (fun x -> SynExpr.Condition(otherProp x, SynExpr.OpEquality, thisProp x))
            |> SynExpr.CreateAndAll

        let equalsMember =
            SynMemberDefn.Member(
                SynBinding.Create(
                    interfaceMember,
                    SynPat.CreateLongIdent(thisMember equalsMethodIdent, [SynPat.CreateNamed "other"]),
                    equalExps
                ),
                range0
            )


        let objectEquals =
            let okCase =
                let equatable = SynExpr.Upcast(SynExpr.CreateIdent thisIdent, equatableType, range0)
                let call = SynExpr.CreateInstanceMethodCall(SynExpr.CreateParen equatable, equalsMethodIdent, SynExpr.CreateIdent "x")
                let p = SynPat.As(SynPat.IsInst(thisType, range0), SynPat.CreateNamed "x", range0)
                SynMatchClause.Create(p, None, call)

            let nokCase = SynMatchClause.CreateOtherwise(SynExpr.CreateConst(SynConst.Bool false))
            let matchExpr = SynExpr.CreateMatch(SynExpr.CreateIdent "other", [okCase; nokCase])

            SynMemberDefn.InstanceMember(thisIdent, equalsMethodIdent, matchExpr, [ SynPat.CreateNamed "other" ], isOverride = true)

        let equatable =
            SynMemberDefn.Interface(
                equatableType,
                Some range0,
                Some [ equalsMember ],
                range0
            )

        let getHashCode =
            let vals = props |> List.map (thisMember >> SynExpr.CreateLongIdent) |> SynExpr.CreateTuple |> SynExpr.CreateParen
            let expr = SynExpr.CreateInstanceMethodCall(SynLongIdent.Create "hash", vals)
            SynMemberDefn.InstanceMember(thisIdent, Ident.Create "GetHashCode", expr, [], isOverride = true)

        [equatable; objectEquals; getHashCode]

    static member CreateAvroRecordInterface(thisIdent : Ident, typeName : Ident) =
        let interfaceMember = { SynMemberFlags.InstanceMember with IsOverrideOrExplicitImpl = true }
        let thisMember name = SynLongIdent.Create [ thisIdent.idText; name ]

        let specRecGet =
            SynMemberDefn.Member(
                SynBinding.Create(
                    interfaceMember,
                    SynPat.CreateLongIdent(thisMember "Get", [ SynPat.CreateParen(SynPat.CreateTyped("pos", SynType.Int)) ]),
                    SynExpr.CreateUnit,
                    attributes = [ SynAttributeList.Create SynAttribute.NotForFSharp ]
                ),
                range0
            )

        let specRecSet =
            SynMemberDefn.Member(
                SynBinding.Create(
                    interfaceMember,
                    SynPat.CreateLongIdent(
                        thisMember "Put",
                        [ SynPat.CreateParenTuple [ SynPat.CreateTyped("pos", SynType.Int)
                                                    SynPat.CreateTyped("value", SynType.Object) ] ]
                    ),
                    SynExpr.CreateUnit,
                    attributes = [ SynAttributeList.Create SynAttribute.NotForFSharp ]
                ),
                range0
            )

        let specRecSchema =
            let bnd =
                SynBinding.Create(
                    { interfaceMember with MemberKind = SynMemberKind.PropertyGet },
                    SynPat.CreateLongIdent(thisMember "Schema", []),
                    SynExpr.CreateApp(
                        SynExpr.CreateLongIdent "Avro.Schema.Parse",
                        SynExpr.CreateParen(SynExpr.CreateLongIdent($"{typeName.idText}.SCHEMA"))
                    )
                )

            SynMemberDefn.Member(bnd, range0)


        SynMemberDefn.Interface(
            SynType.Create("Avro.Specific.ISpecificRecord"),
            Some range0,
            Some [ specRecGet
                   specRecSet
                   specRecSchema ],
            range0
        )
