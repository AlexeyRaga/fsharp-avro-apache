[<Microsoft.FSharp.Core.AutoOpen>]
module FSharp.Compiler.Syntax.SynType

open FSharp.Compiler.Text.Range

type SynType with
    static member Create(name : Ident) =
        SynType.LongIdent(SynLongIdent.Create name)

    static member Create(name : string) =
        SynType.LongIdent(SynLongIdent.Create name)

    static member CreateFun(fieldTypeIn, fieldTypeOut) =
        SynType.Fun(fieldTypeIn, fieldTypeOut, range0, { ArrowRange = range0 })

    static member CreateApp(typeName : SynType, typeArgs : SynType list) =
        SynType.App(typeName, Some range0, typeArgs, [], Some range0, false, range0)

    static member Object = SynType.Create "obj"
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
    static member Wild = SynType.Create "_"

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
