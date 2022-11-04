[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynPat

open FSharp.Compiler.Text.Range

type SynArgPats with
    static member Empty = SynArgPats.Pats []

type SynSimplePat with
    static member CreateId(name : Ident, ?isThisVal : bool, ?isOptional : bool) =
        SynSimplePat.Id(name, None, false, defaultArg isOptional false, defaultArg isThisVal false, range0)

    static member CreateTyped(name : Ident, typ : SynType, ?isThisVal : bool, ?isOptional : bool) =
        SynSimplePat.Typed(SynSimplePat.CreateId(name, ?isThisVal = isThisVal, ?isOptional = isOptional), typ, range0)

type SynSimplePats with
    static member Create(patterns) =
        SynSimplePats.SimplePats(patterns, range0)

type SynPat with
    static member CreateLongIdent(ident : SynLongIdent, args, ?typarDecls, ?extraId, ?access) =
        let args = SynArgPats.Pats(args)
        SynPat.LongIdent(ident, extraId, typarDecls, args, access, range0)

    static member CreateLongIdent(name : string, args : SynPat list) =
        SynPat.CreateLongIdent(SynLongIdent.Create name, args)

    static member CreateLongIdent(ident : Ident, args : SynPat list) =
        SynPat.CreateLongIdent(SynLongIdent.Create [ ident ], args)

    static member CreateNamed(ident : SynIdent, ?isThisVal, ?access) =
        let isThisVal = defaultArg isThisVal false
        SynPat.Named(ident, isThisVal, access, range0)

    static member CreateNamed(ident : Ident, ?isThisVal, ?access) =
        let isThisVal = defaultArg isThisVal false
        SynPat.Named(SynIdent(ident, None), isThisVal, access, range0)

    static member CreateNamed(name : string) = SynPat.CreateNamed(Ident.Create name)

    static member CreateTyped(pat, typ) =
        SynPat.Typed(pat, typ, range0)

    static member CreateTyped(name, typ) =
        SynPat.Typed(SynPat.CreateNamed name, typ, range0)

    static member CreateParen(exp) = SynPat.Paren(exp, range0)
    static member CreateTuple(pats : SynPat list) = SynPat.Tuple(false, pats, range0)
    static member CreateTupleParen(pats : SynPat list) = SynPat.CreateParen (SynPat.CreateTuple pats)

    static member CreateWild = SynPat.Wild(range0)
    static member CreateNull = SynPat.Null(range0)
    static member CreateUnit = SynPat.Const(SynConst.Unit, range0)
    static member CreateConst(expr) = SynPat.Const(expr, range0)

    static member CreateIsInst(typ : SynType) = SynPat.IsInst(typ, range0)
    static member CreateAs(lhs : SynPat, rhs : SynPat) = SynPat.As(lhs, rhs, range0)

