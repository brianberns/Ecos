namespace Ecos.Builder

open System
open System.Globalization

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media

/// Editing widgets. A text field reports a new value when it is
/// committed: on Enter, or when it loses focus. A spinner reports each
/// new value. Each field has a key, unique within the window, so that its
/// handler stays with it even if the control is reused for another
/// field. So a handler must not capture any value that can change
/// without its key changing.
module Fields =

    /// Width of a field's label.
    let labelWidth = 170.0

    /// Width of a value box.
    let valueWidth = 90.0

    /// Space around controls.
    let margin = Avalonia.Thickness(4.0, 2.0)

    // Colors that read well in both light and dark themes.

    /// Muted text, for notes.
    let noteBrush : IBrush = SolidColorBrush(Color.Parse("#8A8A8A"))

    /// Text of a warning.
    let warningBrush : IBrush = SolidColorBrush(Color.Parse("#D84040"))

    /// Faint gray behind a panel, lighter or darker than the window.
    let panelBrush : IBrush = SolidColorBrush(Color.Parse("#14808080"))

    /// Lines around panels and cards.
    let lineBrush : IBrush = SolidColorBrush(Color.Parse("#50808080"))

    /// Shows the given float, with infinities as "inf".
    let formatFloat (x : float) =
        if Double.IsPositiveInfinity x then "inf"
        elif Double.IsNegativeInfinity x then "-inf"
        else x.ToString("R", CultureInfo.InvariantCulture)

    /// Parses a float, accepting "inf" for infinity.
    let tryParseFloat (text : string) =
        match text.Trim().ToLowerInvariant() with
            | "inf" | "infinity" | "+inf" | "∞" -> Some Double.PositiveInfinity
            | "-inf" | "-infinity" -> Some Double.NegativeInfinity
            | t ->
                match Double.TryParse(t, NumberStyles.Float, CultureInfo.InvariantCulture) with
                    | true, x -> Some x
                    | _ -> None

    /// Parses an int.
    let tryParseInt (text : string) =
        match Int32.TryParse(text.Trim().Replace(",", "").Replace("_", ""), NumberStyles.Integer, CultureInfo.InvariantCulture) with
            | true, n -> Some n
            | _ -> None

    /// A label.
    let label (text : string) width =
        TextBlock.create [
            TextBlock.text text
            TextBlock.width width
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin margin
        ] :> IView

    /// A note in muted text.
    let note (text : string) =
        TextBlock.create [
            TextBlock.text text
            TextBlock.foreground noteBrush
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin margin
        ] :> IView

    /// A note in red, e.g. for an invalid pattern.
    let warning (text : string) =
        TextBlock.create [
            TextBlock.text text
            TextBlock.foreground warningBrush
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin margin
        ] :> IView

    /// A section heading.
    let heading (text : string) =
        TextBlock.create [
            TextBlock.text text
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.fontSize 14.0
            TextBlock.margin (Avalonia.Thickness(4.0, 12.0, 4.0, 4.0))
        ] :> IView

    /// A text box that reports its text when committed.
    let textBox key width (text : string) (onCommit : string -> unit) =
        let options = SubPatchOptions.OnChangeOf key
        TextBox.create [
            TextBox.text text
            TextBox.width width
            TextBox.margin margin
            TextBox.verticalAlignment VerticalAlignment.Center
            TextBox.onLostFocus ((fun args -> onCommit (args.Source :?> TextBox).Text), options)
            TextBox.onKeyDown ((fun args ->
                if args.Key = Key.Enter then
                    onCommit (args.Source :?> TextBox).Text), options)
        ] :> IView

    /// A row of controls.
    let row (children : IView list) =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children children
        ] :> IView

    /// A column of controls.
    let column (children : IView list) =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children children
        ] :> IView

    /// The given view, with the given tooltip anywhere over it.
    let tipped (tip : string) (view : IView) =
        Border.create [
            Border.tip tip
            Border.background Brushes.Transparent   // so that the tip shows between controls too
            Border.child view
        ] :> IView

    /// A labeled float field.
    let floatField key text (value : float) (onCommit : float -> unit) =
        row [
            label text labelWidth
            textBox key valueWidth (formatFloat value) (fun s ->
                tryParseFloat s |> Option.iter onCommit)
        ]

    /// A labeled float field in which a blank means infinity, e.g. a
    /// barrier that can be absent.
    let infiniteFloatField key text (value : float) infinityText (onCommit : float -> unit) =
        row [
            label text labelWidth
            textBox key valueWidth (if Double.IsPositiveInfinity value then "" else formatFloat value) (fun s ->
                if String.IsNullOrWhiteSpace s then onCommit Double.PositiveInfinity
                else tryParseFloat s |> Option.iter onCommit)
            if Double.IsPositiveInfinity value then note $"∞ ({infinityText})"
        ]

    /// A labeled field for an optional float: blank for none.
    let floatOptionField key text (value : float option) noneText (onCommit : float option -> unit) =
        row [
            label text labelWidth
            textBox key valueWidth (value |> Option.map formatFloat |> Option.defaultValue "") (fun s ->
                if String.IsNullOrWhiteSpace s then onCommit None
                else tryParseFloat s |> Option.iter (Some >> onCommit))
            if value.IsNone then note noneText
        ]

    /// An up/down box for a number, which reports each new value.
    let spinner key width (minimum : decimal) (maximum : decimal) (step : decimal) format (value : decimal) (onChange : decimal -> unit) =
        NumericUpDown.create [
            NumericUpDown.value (Nullable value)
            NumericUpDown.minimum minimum
            NumericUpDown.maximum maximum
            NumericUpDown.increment step
            NumericUpDown.formatString format
            NumericUpDown.width width
            NumericUpDown.margin margin
            NumericUpDown.verticalAlignment VerticalAlignment.Center
            NumericUpDown.horizontalContentAlignment HorizontalAlignment.Right
            NumericUpDown.onValueChanged ((fun v ->
                if v.HasValue then onChange v.Value), SubPatchOptions.OnChangeOf key)
        ] :> IView

    /// An up/down box for an int.
    let intSpinner key width (minimum : int) (maximum : int) (step : int) (value : int) (onChange : int -> unit) =
        spinner key width (decimal minimum) (decimal maximum) (decimal step) "0" (decimal value)
            (fun v -> onChange (int (Math.Round v)))

    /// A labeled up/down box for an int.
    let intField key text minimum maximum step value onChange =
        row [
            label text labelWidth
            intSpinner key 130.0 minimum maximum step value onChange
        ]

    /// A check box.
    let checkBox key (text : string) (value : bool) (onChange : bool -> unit) =
        CheckBox.create [
            CheckBox.content text
            CheckBox.isChecked value
            CheckBox.margin margin
            CheckBox.verticalAlignment VerticalAlignment.Center
            CheckBox.onIsCheckedChanged ((fun args ->
                let box = args.Source :?> CheckBox
                if box.IsChecked.HasValue && box.IsChecked.Value <> value then
                    onChange box.IsChecked.Value), SubPatchOptions.OnChangeOf (box (key, value)))
        ] :> IView

    /// A combo box of the given names. It reports a selection only if it
    /// differs from the given one.
    let comboBox key width (names : string[]) (index : int) (onSelect : int -> unit) =
        ComboBox.create [
            ComboBox.dataItems names
            ComboBox.selectedIndex index
            ComboBox.width width
            ComboBox.margin margin
            ComboBox.verticalAlignment VerticalAlignment.Center
            ComboBox.onSelectedIndexChanged ((fun i ->
                if i >= 0 && i <> index then onSelect i), SubPatchOptions.OnChangeOf (box (key, index)))
        ]
            |> View.withKey (key + "|" + String.Join("|", names))   // new items, new box
            :> IView

    /// A button.
    let button key (text : string) (onClick : unit -> unit) =
        Button.create [
            Button.content text
            Button.margin margin
            Button.verticalAlignment VerticalAlignment.Center
            Button.onClick ((fun _ -> onClick ()), SubPatchOptions.OnChangeOf key)
        ] :> IView

    /// A small button, e.g. to remove a row.
    let smallButton key (text : string) (tip : string) (onClick : unit -> unit) =
        Button.create [
            Button.content text
            Button.margin margin
            Button.padding (Avalonia.Thickness(6.0, 0.0))
            Button.tip tip
            Button.verticalAlignment VerticalAlignment.Center
            Button.onClick ((fun _ -> onClick ()), SubPatchOptions.OnChangeOf key)
        ] :> IView
