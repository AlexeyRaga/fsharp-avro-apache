namespace FSharp.Avro.Codegen

open Avro

type RecordRepresentation =
    | Record
    | Class

type GenParams =
    { RecordRepr : RecordRepresentation
      NamespaceMapping : (string * string) list }

module GenParams =
    let mappedNamespace (ns : string) (parameters : GenParams) =
        match parameters.NamespaceMapping |> Seq.tryFind(fst >> ns.StartsWith) with
        | None -> ns
        | Some (oldNs, newNs) -> ns.Replace(oldNs, newNs)

    let fullTypeName (name : SchemaName) (parameters : GenParams) =
        match (Option.ofObj name.Namespace, name.Name) with
        | Some "", typ -> typ
        | None, typ -> typ
        | Some ns, typ -> mappedNamespace ns parameters + "." + typ
