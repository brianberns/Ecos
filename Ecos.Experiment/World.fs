namespace Ecos.Experiment

open Ecos

module World =

    let getAtomCount world =
        world.AtomMap.Count

    let getBondCount bondType world =
        let count = world.BondTypeMap[bondType].Count
        assert(count % 2 = 0)
        count / 2   // each bond counted twice
