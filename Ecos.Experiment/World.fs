namespace Ecos.Experiment

open Ecos

module World =

    let getAtomCount world =
        world.AtomMap.Count

    let getBondCount bondType world =
        world.BondTypeMap[bondType].Count
