namespace Ecos.Desktop

open Avalonia.Media
open Ecos.Engine

module Photon =

    let radius = Atom.radius / 2.0

    /// Draws the given photon.
    let draw (ctx : DrawingContext) (photon : Photon) =
        ctx.DrawEllipse(
            Brushes.Black, null,
            photon.Location.ToAvalonia(),
            radius, radius)
