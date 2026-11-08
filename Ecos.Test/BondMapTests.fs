namespace Ecos.Test

open Xunit
open Ecos

/// Tests of the bond map's own behavior: lookups at its edges, and
/// equality, which Elmish needs, ignoring the empty slots that removing
/// bonds leaves behind.
module BondMapTests =

    let private bonded pairs =
        (BondMap.empty, pairs)
            ||> Seq.fold (fun map (idA, idB) -> BondMap.createBond idA idB map)

    [<Fact>]
    let ``ContainsKey is false past the end of the map`` () =
        let map = bonded [ 0, 1 ]
        Assert.True(map.ContainsKey 0)
        Assert.True(map.ContainsKey 1)
        Assert.False(map.ContainsKey 2)
        Assert.False(map.ContainsKey 100)

    [<Fact>]
    let ``Removing every bond leaves an empty map`` () =
        let map = bonded [ 0, 1; 3, 2 ] |> BondMap.removeBond 0 1 |> BondMap.removeBond 2 3
        Assert.True(map.IsEmpty)
        Assert.False(map.ContainsKey 3)
        Assert.True((map = BondMap.empty))
        Assert.Equal(hash BondMap.empty, hash map)

    [<Fact>]
    let ``Maps with the same bonds are equal and hash alike`` () =
        let a = bonded [ 0, 1; 1, 2 ]
        let b = bonded [ 2, 1; 1, 0 ]
        let c = bonded [ 0, 1; 1, 2; 5, 6 ] |> BondMap.removeBond 6 5   // longer array, same bonds
        for other in [ b; c ] do
            Assert.True((a = other))
            Assert.Equal(hash a, hash other)
            Assert.Equal(a.GetHashCode(), other.GetHashCode())

    [<Fact>]
    let ``Maps with different bonds are not equal`` () =
        Assert.False((bonded [ 0, 1 ] = bonded [ 0, 2 ]))
        Assert.False((bonded [ 0, 1 ] = BondMap.empty))
