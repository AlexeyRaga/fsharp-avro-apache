[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynExpr

open System
open FSharp.Compiler.Text.Range
open FSharp.Compiler.SyntaxTrivia

type SynExpr with
    static member CreateConst value = SynExpr.Const(value, range0)

    static member CreateParen expr =
        SynExpr.Paren(expr, range0, Some range0, range0)

    static member CreateTyped(expr, typ) = SynExpr.Typed(expr, typ, range0)

    static member CreateApp(funcExpr, argExpr, ?isInfix) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, defaultArg isInfix false, funcExpr, argExpr, range0)

    static member Create(ident : SynLongIdent, ?isOptional) =
        SynExpr.LongIdent(defaultArg isOptional false, ident, None, range0)

    static member Create(ident : LongIdent, ?isOptional) =
        SynExpr.Create(SynLongIdent.Create ident, ?isOptional = isOptional)

    static member Create (ident : Ident) = SynExpr.Ident(ident)
    static member Create (ident : String) =
        if ident.Contains('.')
            then SynExpr.Create(SynLongIdent.Create ident)
            else SynExpr.Create(Ident.Create ident)

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
        | [ x ] -> SynExpr.CreateApp(func, x)
        | _ -> SynExpr.CreateApp(func, SynExpr.CreateParen(SynExpr.CreateTuple args))

    static member CreateApp(func : Ident, [<ParamArray>] args : Ident array) =
        let func = SynExpr.Create func
        let args = args |> Seq.map SynExpr.Create |> List.ofSeq
        SynExpr.CreateApp(func, args)

    static member CreateConst(value : int) =
        SynExpr.CreateConst(SynConst.Int32 value)

    static member OpEquality =
        let ident = SynLongIdent([ Ident.Create "op_Equality" ], [], [ Some(IdentTrivia.OriginalNotation("=")) ])
        SynExpr.Create(ident)

    static member OpInequality =
        let ident = SynLongIdent([ Ident.Create "op_Inequality" ], [], [ Some(IdentTrivia.OriginalNotation("<>")) ])
        SynExpr.Create(ident)

    static member OpBooleanAnd =
        let ident = SynLongIdent([ Ident.Create "op_BooleanAnd" ], [], [ Some(IdentTrivia.OriginalNotation("&&")) ])
        SynExpr.Create(ident)

    static member CreateIfThenElse(ifExpr : SynExpr, thenExpr : SynExpr, ?elseExpr : SynExpr ) =
        let trivia = { IfKeyword = range0
                       ElseKeyword = Some range0
                       IsElif = false
                       ThenKeyword = range0
                       IfToThenRange = range0 }
        SynExpr.IfThenElse(ifExpr, thenExpr, elseExpr, DebugPointAtBinding.Yes(range0), false, range0, trivia)

    static member CreateAndAll(exprs : SynExpr list) =
        match exprs with
        | [] -> SynExpr.CreateConst(SynConst.Bool true)
        | [ x ] -> x
        | x :: xs -> (xs |> Seq.fold (fun s e -> SynExpr.Condition(s, SynExpr.OpBooleanAnd, e)) x)

    static member Condition(lhs : SynExpr, comp : SynExpr, rhs : SynExpr) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(comp, rhs))

    static member PipeRightOp =
        let ident = SynLongIdent([ Ident.Create "op_PipeRight" ], [], [ Some(IdentTrivia.OriginalNotation("|>")) ])
        SynExpr.Create ident

    static member PipeRight(lhs : SynExpr, rhs : SynExpr) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(SynExpr.PipeRightOp, rhs))

    static member Ignore(expr : SynExpr) =
        SynExpr.PipeRight(expr, SynExpr.Create "ignore")

    static member CreateRecord(fields : list<RecordFieldName * option<SynExpr>>) =
        let fields = fields |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, None, synExpr, None))
        SynExpr.Record(None, None, fields, range0)

    static member CreateRecordUpdate(copyInfo : SynExpr, fieldUpdates) =
        let blockSep : BlockSeparator = (range0, None)
        let fields = fieldUpdates |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, Some range0, synExpr, Some blockSep))
        let copyInfo = Some(copyInfo, blockSep)
        SynExpr.Record(None, copyInfo, fields, range0)

    static member CreateRecordUpdate(copyInfo : SynExpr, fieldUpdates) =
        let blockSep : BlockSeparator = (range0, None)
        let copyInfo = Some(copyInfo, blockSep)
        SynExpr.Record(None, copyInfo, fieldUpdates, range0)

    static member CreateMatch(matchExpr, clauses) =
        SynExpr.Match(DebugPointAtBinding.Yes range0, matchExpr, clauses, range0, SynExprMatchTrivia.Zero)

    static member MethodCall(instance : SynExpr, method : SynLongIdent, ?args : SynExpr) =
        let valueExpr = SynExpr.DotGet(instance, range0, method, range0)

        match args with
        | Some xs -> SynExpr.CreateApp(valueExpr, xs)
        | None -> valueExpr

    static member MethodCall(instance : SynExpr, method : Ident, ?args : SynExpr) =
        SynExpr.MethodCall(instance, SynLongIdent.Create method, ?args = args)

    static member MethodCall(instanceAndMethod : SynLongIdent, ?args : SynExpr) =
        let valueExpr = SynExpr.Create instanceAndMethod

        match args with
        | Some xs -> SynExpr.CreateApp(valueExpr, xs)
        | None -> valueExpr

    static member MethodCall(instanceAndMethod : SynLongIdent, typeArgs : SynType list, ?args : SynExpr) =
        let valueExpr = SynExpr.Create instanceAndMethod
        let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, typeArgs, [], Some range0, range0, range0)

        match args with
        | Some xs -> SynExpr.CreateApp(valueExprWithType, xs)
        | None -> valueExprWithType

    static member CreateLambda(argName : Ident, body : SynExpr) =
        let args = [ SynPat.CreateNamed argName ]
        let dummyExpr = SynExpr.CreateConst(SynConst.Unit)
        SynExpr.Lambda(false, false, SynSimplePats.SimplePats([], range0), dummyExpr, Some(args, body), range0, SynExprLambdaTrivia.Zero)

    static member CreateDowncast(expr : SynExpr, typ : SynType) = SynExpr.Downcast(expr, typ, range0)
