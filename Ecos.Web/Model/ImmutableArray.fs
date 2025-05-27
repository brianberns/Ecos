namespace Ecos

/// Immutable array.
type ImmutableArray<'t> =
    {
        Items : 't[]
    }

    /// Creates an immutable array.
    static member Create(items) =
        { Items = [| yield! items |] }

    /// Accesses this array at the given index.
    member this.Item
        with get(i) = this.Items[i]

    /// Number of items in this array.
    member this.Length =
        this.Items.Length

    /// Creates a new array with the given item at the given
    // index.
    member this.SetItem(idx, item) =
        {
            Items =
                Array.init this.Length (fun i ->
                    if i = idx then item
                    else this[i])
        }