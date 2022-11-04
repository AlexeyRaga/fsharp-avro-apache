[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynAttribute

open System
open FSharp.Compiler.Text.Range

type SynAttribute with
    static member Create(name : string) : SynAttribute =
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.Const(SynConst.Unit, range0)
          Range = range0
          Target = None
          TypeName = SynLongIdent.Create name }

    static member Create(name : string, argument : string) : SynAttribute =
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.Const(SynConst.String(argument, SynStringKind.Regular, range0), range0)
          Range = range0
          Target = None
          TypeName = SynLongIdent.Create name }

    static member Create(name : string, argument : bool) : SynAttribute =
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.Const(SynConst.Bool argument, range0)
          Range = range0
          Target = None
          TypeName = SynLongIdent.Create name }

    static member Create(name : string, argument : int) : SynAttribute =
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.Const(SynConst.Int32 argument, range0)
          Range = range0
          Target = None
          TypeName = SynLongIdent.Create name }

    static member Create(name : string, argument : SynConst) : SynAttribute =
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.Const(argument, range0)
          Range = range0
          Target = None
          TypeName = SynLongIdent.Create name }

    static member Create(name : Ident, argument : SynConst) : SynAttribute =
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.Const(argument, range0)
          Range = range0
          Target = None
          TypeName = SynLongIdent([ name ], [], []) }

    static member Create(name : Ident list, argument : SynConst) : SynAttribute =
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.Const(argument, range0)
          Range = range0
          Target = None
          TypeName = SynLongIdent(name, [], []) }

    static member RequireQualifiedAccess = SynAttribute.Create("RequireQualifiedAccess")
    static member Sealed = SynAttribute.Create("Sealed")
    static member CLIMutable = SynAttribute.Create("CLIMutable")

    static member NotForFSharp : SynAttribute =
        let args = [
            SynExpr.CreateConst (SynConst.CreateString("This method is not intended for use from F#."))
            SynExpr.CreateConst(SynConst.Int32(10001))
            SynExpr.Condition(SynExpr.CreateIdent "IsError", SynExpr.OpEquality, SynExpr.CreateConst (SynConst.Bool true))
            SynExpr.Condition(SynExpr.CreateIdent "IsHidden", SynExpr.OpEquality, SynExpr.CreateConst (SynConst.Bool true))
        ]
        { AppliesToGetterAndSetter = false
          ArgExpr = SynExpr.CreateParen (SynExpr.CreateTuple args)
          Range = range0
          Target = None
          TypeName = SynLongIdent.Create "CompilerMessage" }


    static member CompiledName(valueArg : string) =
        SynAttribute.Create("CompiledName", valueArg)


type SynAttributeList with
    static member Create(attrs) : SynAttributeList = { Attributes = attrs; Range = range0 }

    static member Create(attr) : SynAttributeList =
        { Attributes = [ attr ]
          Range = range0 }

    static member Create([<ParamArray>] attrs) : SynAttributeList =
        { Attributes = List.ofArray attrs
          Range = range0 }
