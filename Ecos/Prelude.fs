namespace Ecos

open System
open System.Collections.Immutable

module Dictionary =

    /// Tries to find the given key in the given dictionary.
    let inline tryFind key (dict : ImmutableSortedDictionary<_, _>) =
        match dict.TryGetValue(key) with
            | true, value -> Some value
            | false, _ -> None

    /// Adds the given key to the given value in the given
    /// dictionary.
    let inline add key value (dict : ImmutableSortedDictionary<_, _>) =
        dict.Add(key, value)

    /// Removes the given key from the given dictionary.
    let inline remove key (dict : ImmutableSortedDictionary<_, _>) =
        dict.Remove(key)
