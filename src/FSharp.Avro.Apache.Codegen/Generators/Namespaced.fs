namespace FSharp.Avro.Codegen.Generators

open FSharp.Compiler.Syntax

type Namespaced =
    { Namespace : string
      Declarations : SynModuleDecl list }
