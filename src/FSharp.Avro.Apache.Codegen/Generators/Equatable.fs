[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module FSharp.Avro.Codegen.Generators.IEquatable

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

let implementInterface (thisIdent : Ident, thisType : SynType, props : Ident list) =
    let thisMember name = SynLongIdent.Create [ thisIdent; name ]
    let equalsMethodIdent = Ident.Create "Equals"
    let equatableType = SynType.CreateApp(SynType.Create("System.IEquatable"), [ thisType ])

    let equalExps =
        let other = Ident.Create "other"

        let otherProp prop =
            SynExpr.Create(SynLongIdent.Create [ other; prop ])

        let thisProp prop = SynExpr.Create(thisMember prop)
        props |> List.map (fun x -> SynExpr.Condition(otherProp x, SynExpr.OpEquality, thisProp x)) |> SynExpr.CreateAndAll

    let equalsMember =
        SynMemberDefn.InstanceMember(thisIdent, equalsMethodIdent, equalExps, args = [ SynPat.CreateNamed "other" ])

    let objectEquals =
        let okCase =
            let equatable = SynExpr.Upcast(SynExpr.Create thisIdent, equatableType, range0)
            let call = SynExpr.MethodCall(SynExpr.CreateParen equatable, equalsMethodIdent, SynExpr.Create "x")
            let p = SynPat.As(SynPat.IsInst(thisType, range0), SynPat.CreateNamed "x", range0)
            SynMatchClause.Create(p, call)

        let nokCase = SynMatchClause.Otherwise(SynExpr.CreateConst(SynConst.Bool false))
        let matchExpr = SynExpr.CreateMatch(SynExpr.Create "other", [ okCase; nokCase ])
        SynMemberDefn.InstanceMember(thisIdent, equalsMethodIdent, matchExpr, [ SynPat.CreateNamed "other" ], isOverride = true)

    let equatable = SynMemberDefn.Interface(equatableType, Some range0, Some [ equalsMember ], range0)

    let getHashCode =
        let values = props |> List.map (thisMember >> SynExpr.Create) |> SynExpr.CreateTuple |> SynExpr.CreateParen
        let expr = SynExpr.MethodCall(SynLongIdent.Create "hash", values)
        SynMemberDefn.InstanceMember(thisIdent, Ident.Create "GetHashCode", expr, [], isOverride = true)

    [ equatable; objectEquals; getHashCode ]
