namespace Ecos

type ImmutableArray<'t> =
    private {
        Items : 't[]
    }

    static member Create(items) =
        { Items = [| yield! items |] }

    member this.Item
        with get(i) = this.Items[i]

    member this.Length =
        this.Items.Length

    member this.SetItem(idx, item) =
        {
            Items =
                Array.init this.Length (fun i ->
                    if i = idx then item
                    else this[i])
        }