namespace Ecos.Experiment

open Ecos

module World =

    let getAtomCount world =
        world.AtomMap.Count

    let getBondCount bondType world =
        world.BondTypeMap[bondType]
            |> Map.toSeq
            |> Seq.collect (fun (idA, ids) ->
                Seq.where (fun idB -> idA < idB) ids)   // count each bond only once
            |> Seq.length
