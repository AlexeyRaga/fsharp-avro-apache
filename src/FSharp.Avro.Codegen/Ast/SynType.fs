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

    static member CreateApp(typeName : SynType, typeArgs : SynType list, ?isPostfix : bool) =
        SynType.App(typeName, Some range0, typeArgs, [], Some range0, defaultArg isPostfix false, range0)

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
    static member DateTimeOffset = SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTimeOffset" ])
    static member DateTime = SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTime" ])

    static member CreateArray(inner : SynType, ?rank : int) =
        SynType.Array(defaultArg rank 1, inner, range0)

    static member List(itemType : SynType, ?isPostfix : bool) =
        let isPostfix = defaultArg isPostfix false
        let typeName = if isPostfix then "list" else "List"
        SynType.CreateApp(SynType.Create typeName, [ itemType ], isPostfix = isPostfix)

    static member Seq(itemType : SynType, ?isPostfix : bool) =
        SynType.CreateApp(SynType.Create "seq", [ itemType ], ?isPostfix = isPostfix)


    static member Dictionary(key, value) =
        SynType.CreateApp(SynType.Create "System.Collections.Generic.Dictionary", [ key; value ])

    static member IDictionary(key, value) =
        SynType.CreateApp(SynType.Create "System.Collections.Generic.IDictionary", [ key; value ])

    static member Map(key, value) =
        SynType.CreateApp(SynType.Create "Map", [ key; value ])

    static member Option(valueType : SynType, ?isPostfix : bool) =
        let isPostfix = defaultArg isPostfix false
        let typeName = if isPostfix then "option" else "Option"
        SynType.CreateApp(SynType.Create typeName, [ valueType ], isPostfix = isPostfix)

    static member Set(elementType : SynType) =
        SynType.CreateApp(SynType.Create "Set", [ elementType ])

    static member ResizeArray(itemType : SynType) =
        SynType.CreateApp(SynType.Create "ResizeArray", [ itemType ])

    static member Choice(choices : SynType list) =
        SynType.CreateApp(SynType.Create "Choice", choices)
