[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.AstExtensions

open System
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml

type Ident with
    static member Create text = Ident(text, range0)

    static member CreateLong(text : string) =
        text.Split([| '.' |]) |> List.ofArray |> List.map Ident.Create

type SynConst with
    static member CreateString s =
        SynConst.String(s, SynStringKind.Regular, range0)

type SynLongIdent with
    static member Create(longIdent : LongIdent) =
        SynLongIdent(longIdent, [], List.replicate longIdent.Length None)

    static member Create(texts) =
        SynLongIdent.Create(texts |> List.map Ident.Create)

    static member Create(text : string) =
        SynLongIdent.Create(Ident.CreateLong text)

type SynAttribute with
    static member Create(name: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Unit, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent.Create name
        }

    static member Create(name: string, argument: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.String(argument, SynStringKind.Regular, range0), range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent.Create name
        }

    static member Create(name: string, argument: bool) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Bool argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent.Create name
        }

    static member Create(name: string, argument: int) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Int32 argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent.Create name
        }

    static member Create(name: string, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent.Create name
        }

    static member Create(name: Ident, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent([name], [ ], [])
        }

    static member Create(name: Ident list, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = SynLongIdent(name, [ ], [])
        }

    static member RequireQualifiedAccess() =
        SynAttribute.Create("RequireQualifiedAccess")

    static member CompiledName(valueArg: string) =
        SynAttribute.Create("CompiledName", valueArg)

type SynAttributeList with
    static member Create(attrs) : SynAttributeList = { Attributes = attrs; Range = range0 }

    static member Create(attr) : SynAttributeList =
        { Attributes = [ attr ]
          Range = range0 }

    static member Create([<ParamArray>] attrs) : SynAttributeList =
        { Attributes = List.ofArray attrs
          Range = range0 }

type SynModuleOrNamespace with
    static member private Create
        (
            ident,
            kind : SynModuleOrNamespaceKind,
            ?isRecursive : bool,
            ?decls : SynModuleDecl list,
            ?docs : PreXmlDoc,
            ?attribs : SynAttributeList list,
            ?access : SynAccess
        ) =
        let range = range0
        let isRecursive = defaultArg isRecursive false
        let decls = defaultArg decls []
        let docs = defaultArg docs PreXmlDoc.Empty
        let attribs = defaultArg attribs SynAttributes.Empty
        SynModuleOrNamespace(ident, isRecursive, kind, decls, docs, attribs, access, range, SynModuleOrNamespaceTrivia.Zero)

    static member CreateNamespace
        (
            ident : LongIdent,
            ?isRecursive : bool,
            ?decls : SynModuleDecl list,
            ?docs : PreXmlDoc,
            ?attribs : SynAttributeList list,
            ?access : SynAccess
        ) =
        SynModuleOrNamespace.Create(
            ident,
            SynModuleOrNamespaceKind.DeclaredNamespace,
            ?isRecursive = isRecursive,
            ?decls = decls,
            ?docs = docs,
            ?attribs = attribs,
            ?access = access
        )

    static member CreateModule(ident, ?isRecursive, ?decls, ?docs, ?attribs, ?access) =
        SynModuleOrNamespace.Create(
            ident,
            SynModuleOrNamespaceKind.NamedModule,
            ?isRecursive = isRecursive,
            ?decls = decls,
            ?docs = docs,
            ?attribs = attribs,
            ?access = access
        )

    static member CreateAnonModule(?ident, ?isRecursive, ?decls, ?docs, ?attribs, ?access) =
        let ident = defaultArg ident (Ident.CreateLong "Tmp")

        SynModuleOrNamespace.Create(
            ident,
            SynModuleOrNamespaceKind.AnonModule,
            ?isRecursive = isRecursive,
            ?decls = decls,
            ?docs = docs,
            ?attribs = attribs,
            ?access = access
        )


type SynComponentInfo with
    static member Create(id : LongIdent, ?attributes, ?parameters, ?constraints, ?xmldoc, ?preferPostfix, ?access) =
        let attributes = defaultArg attributes SynAttributes.Empty
        let constraints = defaultArg constraints []
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
        let preferPostfix = defaultArg preferPostfix false
        let access = defaultArg access None
        let range = range0
        SynComponentInfo(attributes, parameters, constraints, id, xmldoc, preferPostfix, access, range)

type SynArgPats with
    static member Empty = SynArgPats.Pats []

type SynMemberFlags with
    static member InstanceMember : SynMemberFlags =
        { IsInstance = true
          MemberKind = SynMemberKind.Member
          IsDispatchSlot = false
          IsOverrideOrExplicitImpl = false
          IsFinal = false
          GetterOrSetterIsCompilerGenerated = false
          Trivia = SynMemberFlagsTrivia.Zero }

    static member StaticMember = { SynMemberFlags.InstanceMember with IsInstance = false }

type SynTypeDefn with
    static member CreateFromRepr
        (
            name : Ident,
            repr : SynTypeDefnRepr,
            ?members : SynMemberDefns,
            ?attributes : SynAttributeList list,
            ?xmldoc : PreXmlDoc
        ) =
        let name =
            SynComponentInfo.Create([ name ], attributes = defaultArg attributes [], xmldoc = defaultArg xmldoc PreXmlDoc.Empty)

        let extraMembers, trivia =
            match members with
            | None -> SynMemberDefns.Empty, SynTypeDefnTrivia.Zero
            | Some defns -> defns, { SynTypeDefnTrivia.Zero with WithKeyword = Some range0 }

        SynTypeDefn(name, repr, extraMembers, None, range0, trivia)

    static member CreateUnion
        (
            name : Ident,
            cases : SynUnionCase list,
            ?members : SynMemberDefns,
            ?access : SynAccess,
            ?attributes : SynAttributeList list,
            ?xmldoc : PreXmlDoc
        ) =
        let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(access, cases, range0), range0)

        SynTypeDefn.CreateFromRepr(
            name,
            repr,
            defaultArg members SynMemberDefns.Empty,
            defaultArg attributes [],
            defaultArg xmldoc PreXmlDoc.Empty
        )

    static member CreateRecord
        (
            name : Ident,
            fields : SynField seq,
            ?members : SynMemberDefns,
            ?attributes : SynAttributeList list,
            ?xmldoc : PreXmlDoc
        ) =
        let repr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(None, Seq.toList fields, range0), range0)

        SynTypeDefn.CreateFromRepr(
            name,
            repr,
            defaultArg members SynMemberDefns.Empty,
            defaultArg attributes [],
            defaultArg xmldoc PreXmlDoc.Empty
        )

type SynField with
    static member Create(fieldType : SynType, ?name : Ident, ?attributes : SynAttributes, ?access : SynAccess, ?xmldoc : PreXmlDoc) =
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
        let attributes = defaultArg attributes SynAttributes.Empty
        SynField(attributes, false, name, fieldType, false, xmldoc, access, range0)

    static member CreateArray(elementType : SynType, ?rank : int) =
        SynField.Create(SynType.Array(defaultArg rank 1, elementType, range0))

type SynUnionCase with
    static member Create(name : Ident, ?fields : SynField list, ?attributes : SynAttributes, ?access : SynAccess, ?xmldoc : PreXmlDoc) =
        let trivia : SynUnionCaseTrivia = { BarRange = Some range0 }
        let attributes = defaultArg attributes SynAttributes.Empty
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
        SynUnionCase(attributes, SynIdent(name, None), SynUnionCaseKind.Fields(defaultArg fields []), xmldoc, access, range0, trivia)

type SynType with
    static member Create(name : string) =
        SynType.LongIdent(SynLongIdent.Create name)

    static member CreateFun(fieldTypeIn, fieldTypeOut) =
        SynType.Fun(fieldTypeIn, fieldTypeOut, range0, { ArrowRange = range0 })

    static member Unit = SynType.Create "unit"
    static member Int = SynType.Create "int"
    static member Int64 = SynType.Create "int64"
    static member Float = SynType.Create "float"
    static member Float32 = SynType.Create "float32"
    static member Double = SynType.Create "double"
    static member String = SynType.Create "string"
    static member Bool = SynType.Create "bool"
    static member Byte = SynType.Create "byte"
    static member Char = SynType.Create "char"

    static member Guid = SynType.LongIdent(SynLongIdent.Create [ "System"; "Guid" ])

    static member DateTimeOffset =
        SynType.LongIdent(
            SynLongIdent.Create [ "System"
                                  "DateTimeOffset" ]
        )

    static member DateTime =
        SynType.LongIdent(
            SynLongIdent.Create [ "System"
                                  "DateTime" ]
        )

    static member CreateArray(inner : SynType, ?rank : int) =
        SynType.Array(defaultArg rank 1, inner, range0)

    static member List(inner : SynType, ?isPostfix : bool) =
        let isPostfix = defaultArg isPostfix false

        SynType.App(
            typeName = SynType.Create(if isPostfix then "list" else "List"),
            typeArgs = [ inner ],
            commaRanges = [],
            isPostfix = isPostfix,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member Dictionary(key, value) =
        SynType.App(
            typeName = SynType.Create "System.Collections.Generic.Dictionary",
            typeArgs = [ key; value ],
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member Map(key, value) =
        SynType.App(
            typeName = SynType.Create "Map",
            typeArgs = [ key; value ],
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member Option(inner : SynType, ?isPostfix : bool) =
        let isPostfix = defaultArg isPostfix false

        SynType.App(
            typeName = SynType.Create(if isPostfix then "option" else "Option"),
            typeArgs = [ inner ],
            commaRanges = [],
            isPostfix = isPostfix,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member Set(inner : SynType) =
        SynType.App(
            typeName = SynType.Create "Set",
            typeArgs = [ inner ],
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member ResizeArray(inner : SynType) =
        SynType.App(
            typeName = SynType.Create "ResizeArray",
            typeArgs = [ inner ],
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

    static member Choice(choices : SynType list) =
        SynType.App(
            typeName = SynType.Create "Choice",
            typeArgs = choices,
            commaRanges = [],
            isPostfix = false,
            range = range0,
            greaterRange = Some range0,
            lessRange = Some range0
        )

type SynSimplePat with
    static member CreateId(name : Ident, ?isThisVal : bool, ?isOptional : bool) =
        SynSimplePat.Id(name, None, false, defaultArg isOptional false, defaultArg isThisVal false, range0)

type SynSimplePats with
    static member Create(patterns) =
        SynSimplePats.SimplePats(patterns, range0)

type SynPat with
    static member CreateLongIdent(id, args, ?typarDecls, ?extraId, ?access) =
        let args = SynArgPats.Pats(args)
        SynPat.LongIdent(id, extraId, typarDecls, args, access, range0)

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
    static member CreateTyped(pat, typ) = SynPat.Typed(pat, typ, range0)
    static member CreateParen(exp) = SynPat.Paren(exp, range0)
    static member CreateWild = SynPat.Wild(range0)
    static member CreateNull = SynPat.Null(range0)
    static member CreateConst(expr) = SynPat.Const(expr, range0)

type SynExpr with
    static member CreateConst cnst = SynExpr.Const(cnst, range0)

    static member CreateParen expr =
        SynExpr.Paren(expr, range0, Some range0, range0)

    static member CreateTyped(expr, typ) = SynExpr.Typed(expr, typ, range0)

    static member CreateApp(funcExpr, argExpr, ?isInfix) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, defaultArg isInfix false, funcExpr, argExpr, range0)

    static member CreateIdent id = SynExpr.Ident(id)
    static member CreateIdentString id = SynExpr.Ident(Ident.Create id)

    static member CreateLongIdent(id, ?altNameRefCell, ?isOptional) =
        SynExpr.LongIdent(defaultArg isOptional false, id, defaultArg altNameRefCell None, range0)

    static member CreateTuple list = SynExpr.Tuple(false, list, [], range0)
    static member CreateUnit = SynExpr.CreateConst SynConst.Unit
    static member CreateNull = SynExpr.Null(range0)

    static member EnsureParen(value : SynExpr) =
        match value with
        | SynExpr.Paren _ -> value
        | _ -> SynExpr.CreateParen value

    static member CreateApp(func : Ident, [<ParamArray>] args : Ident array) =
        let funcExpr = SynExpr.CreateIdent func
        let argExprs = args |> Seq.map SynExpr.CreateIdent |> List.ofSeq

        match argExprs with
        | [] -> SynExpr.CreateApp(funcExpr, SynExpr.CreateConst(SynConst.Unit))
        | [ x ] -> SynExpr.CreateApp(funcExpr, SynExpr.EnsureParen x)
        | _ -> SynExpr.CreateApp(funcExpr, SynExpr.CreateTuple argExprs)

    static member CreateConst(value : int) =
        SynExpr.CreateConst(SynConst.Int32 value)

    static member OpEquality = SynExpr.CreateIdent(Ident.Create("op_Equality"))

    static member Condition(lhs, comp, rhs) =
        SynExpr.CreateApp(lhs, SynExpr.CreateApp(comp, rhs))

    static member CreateOk(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Ok", value)

    static member CreateError(value : SynExpr) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Error", value)

    static member CreateStringError(value : string) =
        SynExpr.CreateApp(SynExpr.CreateIdentString "Error", SynExpr.CreateConst(SynConst.String(value, SynStringKind.Regular, range0)))

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

    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, ?args : SynExpr) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        SynExpr.CreateApp(valueExpr, defaultArg args SynExpr.CreateUnit)

    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, instanceMethodsGenericTypes, args) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, instanceMethodsGenericTypes, [], None, range0, range0)
        SynExpr.CreateApp(valueExprWithType, args)

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
            SynExprLambdaTrivia.Zero)

    static member CreateOptionOfObj(?expr : SynExpr) =
        let func = SynLongIdent.Create "Option.ofObj"

        match expr with
        | None -> SynExpr.CreateLongIdent(func)
        | Some (SynExpr.Paren _ as e) -> SynExpr.CreateInstanceMethodCall(func, e)
        | Some e -> SynExpr.CreateInstanceMethodCall(func, e |> SynExpr.CreateParen)

    static member CreateOptionMap(expr : SynExpr) =
        SynExpr.CreateInstanceMethodCall(SynLongIdent.Create "Option.map", SynExpr.EnsureParen expr)

    static member CreateDowncast(expr : SynExpr, typ : SynType) = SynExpr.Downcast(expr, typ, range0)

type SynArgInfo with
    static member Empty = SynArgInfo(SynAttributes.Empty, false, None)

    static member CreateId id =
        SynArgInfo(SynAttributes.Empty, false, Some id)

    static member CreateIdString id = SynArgInfo.CreateId(Ident.Create id)

type SynValInfo with
    static member Empty = SynValInfo([], SynArgInfo.Empty)

type SynBinding with
    static member Let(?access, ?isInline, ?isMutable, ?attributes, ?xmldoc, ?valData, ?pattern, ?returnInfo, ?expr) =
        let isInline = defaultArg isInline false
        let isMutable = defaultArg isMutable false
        let attributes = defaultArg attributes SynAttributes.Empty
        let xmldoc = defaultArg xmldoc PreXmlDoc.Empty
        let valData = defaultArg valData (SynValData(None, SynValInfo([], SynArgInfo.Empty), None))
        let headPat = defaultArg pattern SynPat.CreateNull
        let expr = defaultArg expr (SynExpr.CreateTyped(SynExpr.CreateNull, SynType.Unit))
        let bind = DebugPointAtBinding.NoneAtLet
        let trivia = { LetKeyword = Some range0
                       EqualsRange = Some range0 }
        SynBinding.SynBinding(access, SynBindingKind.Normal, isInline, isMutable, attributes, xmldoc, valData, headPat, returnInfo, expr, range0, bind, trivia)

type SynMemberDefn with
    static member StaticMember(name : Ident, body : SynExpr, ?args : SynPat list) =
        let flags =
            { SynMemberFlags.StaticMember with
                Trivia =
                    { SynMemberFlags.StaticMember.Trivia with
                        MemberRange = Some range0
                        StaticRange = Some range0 } }

        let valData = SynValData(Some flags, SynValInfo.Empty, None)

        let memberArgs =
            match args with
            | Some xs -> [ SynPat.Tuple(false, xs, range0) |> SynPat.CreateParen ]
            | None -> []

        let pat = SynPat.CreateLongIdent(SynLongIdent.Create [ name ], memberArgs)

        let bnd =
            SynBinding.SynBinding(
                None,
                SynBindingKind.Normal,
                false,
                false,
                SynAttributes.Empty,
                PreXmlDoc.Empty,
                valData,
                pat,
                None,
                body,
                range0,
                DebugPointAtBinding.NoneAtLet,
                SynBindingTrivia.Zero
            )

        SynMemberDefn.Member(bnd, range0)

type SynMatchClause with
    static member Create(pat, whenExp, result) =
        let trivia =
            { SynMatchClauseTrivia.ArrowRange = Some range0
              BarRange = Some range0 }

        SynMatchClause.SynMatchClause(pat, whenExp, result, range0, DebugPointAtTarget.No, trivia)

    static member Create(ident, whenExp, result) =
        SynMatchClause.Create(SynPat.CreateNamed ident, whenExp, result)

    static member CreateOtherwiseError(errorMessage : string) =
        SynMatchClause.Create(SynPat.CreateWild, None, SynExpr.CreateStringError errorMessage)

    static member CreateWild(expr : SynExpr) =
        SynMatchClause.Create(SynPat.CreateWild, None, expr)

type SynModuleDecl with
    static member CreateTypes(typeDefs : SynTypeDefn list) = SynModuleDecl.Types(typeDefs, range0)

    static member CreateType(typeDef : SynTypeDefn) =
        SynModuleDecl.Types([ typeDef ], range0)

    static member CreateLet(bindings, ?isRecursive) =
        let isRecursive = defaultArg isRecursive false
        SynModuleDecl.Let(isRecursive, bindings, range0)

    static member CreateNestedModule(ci, decls, ?isRec, ?isCont) =
        let isRec = defaultArg isRec false
        let isCont = defaultArg isCont false
        let trivia = {SynModuleDeclNestedModuleTrivia.EqualsRange = Some range0; ModuleKeyword = Some range0 }
        SynModuleDecl.NestedModule(ci, isRec, decls, isCont, range0, trivia)

