[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynExpr

open System
open FSharp.Compiler.Text.Range
open FSharp.Compiler.SyntaxTrivia

type SynExpr with
    static member CreateConst cnst = SynExpr.Const(cnst, range0)

    static member CreateParen expr =
        SynExpr.Paren(expr, range0, Some range0, range0)

    static member CreateTyped(expr, typ) = SynExpr.Typed(expr, typ, range0)

    static member CreateApp(funcExpr, argExpr, ?isInfix) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, defaultArg isInfix false, funcExpr, argExpr, range0)

    static member CreateIdent ident = SynExpr.Ident(ident)
    static member CreateIdent ident = SynExpr.Ident(Ident.Create ident)

    static member CreateLongIdent(ident, ?isOptional) =
        SynExpr.LongIdent(defaultArg isOptional false, ident, None, range0)

    static member CreateLongIdent(ident : string, ?isOptional) =
        SynExpr.CreateLongIdent(SynLongIdent.Create ident, ?isOptional = isOptional)

    static member CreateLongIdent(ident : LongIdent, ?isOptional) =
        SynExpr.CreateLongIdent(SynLongIdent.Create ident, ?isOptional = isOptional)

    static member CreateTuple list = SynExpr.Tuple(false, list, [], range0)
    static member CreateUnit = SynExpr.CreateConst SynConst.Unit
    static member CreateNull = SynExpr.Null(range0)

    static member EnsureParen(value : SynExpr) =
        match value with
        | SynExpr.Paren _ -> value
        | _ -> SynExpr.CreateParen value

    static member CreateApp(func : SynExpr, args : SynExpr list) =

        match args with
        | [] -> SynExpr.CreateApp(func, SynExpr.CreateConst(SynConst.Unit))
        | [ x ] -> SynExpr.CreateApp(func, SynExpr.EnsureParen x)
        | _ -> SynExpr.CreateApp(func, SynExpr.CreateParen(SynExpr.CreateTuple args))

    static member CreateApp(func : Ident, [<ParamArray>] args : Ident array) =
        let func = SynExpr.CreateIdent func
        let args = args |> Seq.map SynExpr.CreateIdent |> List.ofSeq
        SynExpr.CreateApp(func, args)

    static member CreateConst(value : int) =
        SynExpr.CreateConst(SynConst.Int32 value)

    static member OpEquality =
        let ident = SynLongIdent([Ident.Create "op_Equality"], [], [Some (IdentTrivia.OriginalNotation("="))])
        SynExpr.CreateLongIdent(ident)

    static member OpBooleanAnd =
        let ident = SynLongIdent([Ident.Create "op_BooleanAnd"], [], [Some (IdentTrivia.OriginalNotation("&&"))])
        SynExpr.CreateLongIdent(ident)

    static member CreateAndAll(exprs : SynExpr list) =
        match exprs with
        | [] -> SynExpr.CreateConst(SynConst.Bool true)
        | [x] -> x
        | x :: xs -> (xs |> Seq.fold (fun s e -> SynExpr.Condition(s, SynExpr.OpBooleanAnd, e)) x)

    static member Condition(lhs : SynExpr, comp : SynExpr, rhs : SynExpr) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(comp, rhs))

    static member CreateOk(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdent "Ok", value)

    static member CreateError(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdent "Error", value)

    static member CreateStringError(value : string) =
        SynExpr.CreateApp(
            SynExpr.CreateIdent "Error",
            SynExpr.CreateConst(SynConst.String(value, SynStringKind.Regular, range0))
        )

    static member CreateFailwith(err : string) =
        SynExpr.CreateApp(SynExpr.CreateIdent "failwith", SynExpr.CreateConst(SynConst.CreateString err))

    static member CreatePipeRightOp = SynExpr.CreateIdent(Ident.Create "op_PipeRight")

    static member CreatePipeRight(lhs : SynExpr, rhs : SynExpr) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(SynExpr.CreatePipeRightOp, rhs))

    static member CreateRecord(fields : list<RecordFieldName * option<SynExpr>>) =
        let fields = fields |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, None, synExpr, None))
        SynExpr.Record(None, None, fields, range0)

    static member CreateRecordUpdate(copyInfo : SynExpr, fieldUpdates) =
        let blockSep : BlockSeparator = (range0, None)

        let fields =
            fieldUpdates
            |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, Some range0, synExpr, Some blockSep))

        let copyInfo = Some(copyInfo, blockSep)
        SynExpr.Record(None, copyInfo, fields, range0)

    static member CreateRecordUpdate(copyInfo : SynExpr, fieldUpdates) =
        let blockSep : BlockSeparator = (range0, None)
        let copyInfo = Some(copyInfo, blockSep)
        SynExpr.Record(None, copyInfo, fieldUpdates, range0)

    static member CreateMatch(matchExpr, clauses) =
        SynExpr.Match(DebugPointAtBinding.Yes range0, matchExpr, clauses, range0, SynExprMatchTrivia.Zero)

    static member CreateInstanceMethodCall(instance : SynExpr, method : SynLongIdent, ?args : SynExpr) =
        let valueExpr = SynExpr.DotGet(instance, range0, method, range0)
        match args with
        | Some xs -> SynExpr.CreateApp(valueExpr, xs)
        | None -> valueExpr

    static member CreateInstanceMethodCall(instance : SynExpr, method : Ident, ?args : SynExpr) =
        SynExpr.CreateInstanceMethodCall(instance, SynLongIdent.Create method, ?args = args)

    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, ?args : SynExpr) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        match args with
        | Some xs -> SynExpr.CreateApp(valueExpr, xs)
        | None -> valueExpr

    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, typeArgs : SynType list, ?args : SynExpr) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, typeArgs, [], Some range0, range0, range0)
        match args with
        | Some xs -> SynExpr.CreateApp(valueExprWithType, xs)
        | None -> valueExprWithType

    static member CreateLambda(argName : Ident, body : SynExpr) =
        let args = [ SynPat.CreateNamed argName ]

        let dummyExpr = SynExpr.CreateConst(SynConst.Unit)

        SynExpr.Lambda(
            false,
            false,
            SynSimplePats.SimplePats([], range0), // not used
            dummyExpr, // not used
            Some(args, body), // The good stuff is in here!
            range0,
            SynExprLambdaTrivia.Zero
        )

    static member CreateOptionOfObj(?expr : SynExpr) =
        let func = SynLongIdent.Create "Option.ofObj"

        match expr with
        | None -> SynExpr.CreateLongIdent(func)
        | Some (SynExpr.Paren _ as e) -> SynExpr.CreateInstanceMethodCall(func, e)
        | Some e -> SynExpr.CreateInstanceMethodCall(func, e |> SynExpr.CreateParen)

    static member CreateOptionMap(expr : SynExpr) =
        SynExpr.CreateInstanceMethodCall(SynLongIdent.Create "Option.map", SynExpr.EnsureParen expr)

    static member CreateDowncast(expr : SynExpr, typ : SynType) = SynExpr.Downcast(expr, typ, range0)
