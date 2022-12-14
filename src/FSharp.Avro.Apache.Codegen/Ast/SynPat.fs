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
    static member Create(ident : SynLongIdent, args, ?typarDecls, ?extraId, ?access) =
        let args = SynArgPats.Pats(args)
        SynPat.LongIdent(ident, extraId, typarDecls, args, access, range0)

    static member Create(name : string, args : SynPat list, ?typarDecls, ?extraId, ?access) =
        SynPat.Create(SynLongIdent.Create name, args, ?typarDecls = typarDecls, ?extraId = extraId, ?access = access)

    static member Create(ident : Ident, args : SynPat list, ?typarDecls, ?extraId, ?access) =
        SynPat.Create(SynLongIdent.Create [ ident ], args, ?typarDecls = typarDecls, ?extraId = extraId, ?access = access)

    static member CreateNamed(ident : SynIdent, ?isThisVal, ?access) =
        let isThisVal = defaultArg isThisVal false
        SynPat.Named(ident, isThisVal, access, range0)

    static member CreateNamed(ident : Ident, ?isThisVal, ?access) =
        let isThisVal = defaultArg isThisVal false
        SynPat.Named(SynIdent(ident, None), isThisVal, access, range0)

    static member CreateNamed(name : string) = SynPat.CreateNamed(Ident.Create name)
    static member CreateTyped(pat, typ) = SynPat.Typed(pat, typ, range0)

    static member CreateTyped(name : Ident, typ) =
        SynPat.CreateTyped(SynPat.CreateNamed name, typ)

    static member CreateTyped(name : string, typ) =
        SynPat.CreateTyped(SynPat.CreateNamed name, typ)

    static member CreateParen(exp) = SynPat.Paren(exp, range0)
    static member CreateTuple(pats : SynPat list) = SynPat.Tuple(false, pats, range0)
    static member CreateWild = SynPat.Wild(range0)
    static member CreateNull = SynPat.Null(range0)
    static member CreateUnit = SynPat.Const(SynConst.Unit, range0)
    static member CreateConst(expr) = SynPat.Const(expr, range0)
    static member CreateIsInst(typ : SynType) = SynPat.IsInst(typ, range0)
    static member CreateAs(lhs : SynPat, rhs : SynPat) = SynPat.As(lhs, rhs, range0)
