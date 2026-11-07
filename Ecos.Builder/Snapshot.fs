namespace Ecos.Builder

open System
open System.IO
open System.Runtime.InteropServices

open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform

/// Copies the visible part of the world view to the clipboard as an
/// image, at the screen's own resolution.
module Snapshot =

    /// Win32 clipboard calls. They let us write a classic bottom-up
    /// 24-bit bitmap, which older apps (e.g. Photoshop Elements 10)
    /// read correctly. They show Avalonia's top-down 32-bit bitmap as
    /// black.
    module private Win32 =

        [<DllImport("user32.dll", SetLastError = true)>]
        extern bool OpenClipboard(nativeint hWndNewOwner)

        [<DllImport("user32.dll", SetLastError = true)>]
        extern bool CloseClipboard()

        [<DllImport("user32.dll", SetLastError = true)>]
        extern bool EmptyClipboard()

        [<DllImport("user32.dll", SetLastError = true)>]
        extern nativeint SetClipboardData(uint32 uFormat, nativeint hMem)

        [<DllImport("user32.dll", SetLastError = true, CharSet = CharSet.Unicode)>]
        extern uint32 RegisterClipboardFormat(string lpszFormat)

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern nativeint GlobalAlloc(uint32 uFlags, unativeint dwBytes)

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern nativeint GlobalLock(nativeint hMem)

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern bool GlobalUnlock(nativeint hMem)

        [<DllImport("kernel32.dll", SetLastError = true)>]
        extern nativeint GlobalFree(nativeint hMem)

        let private CF_DIB = 8u
        let private GMEM_MOVEABLE = 0x0002u

        /// Copies the given bytes into a new global memory block.
        let private toGlobal (bytes : byte[]) =
            let handle = GlobalAlloc(GMEM_MOVEABLE, unativeint bytes.Length)
            if handle = 0n then failwith "Out of memory"
            let ptr = GlobalLock(handle)
            Marshal.Copy(bytes, 0, ptr, bytes.Length)
            GlobalUnlock(handle) |> ignore
            handle

        /// Puts the given DIB and PNG on the clipboard.
        let setImage (window : nativeint) (dib : byte[]) (png : byte[]) =
            if not (OpenClipboard window) then failwith "Can't open the clipboard"
            try
                EmptyClipboard() |> ignore
                for format, bytes in [ CF_DIB, dib; RegisterClipboardFormat "PNG", png ] do
                    let handle = toGlobal bytes
                    if SetClipboardData(format, handle) = 0n then   // else the clipboard owns it
                        GlobalFree(handle) |> ignore
            finally
                CloseClipboard() |> ignore

    /// A bottom-up 24-bit DIB (header and pixels) of the given opaque
    /// BGRA pixels.
    let private toDib width height (bgra : byte[]) =
        let stride = (width * 3 + 3) &&& ~~~3
        let dib = Array.zeroCreate<byte> (40 + stride * height)
        use writer = new BinaryWriter(new MemoryStream(dib))
        writer.Write(40)           // header size
        writer.Write(width)
        writer.Write(height)       // positive: bottom-up
        writer.Write(1s)           // planes
        writer.Write(24s)          // bits per pixel
        writer.Write(0)            // BI_RGB
        writer.Write(stride * height)
        writer.Write(3780)         // 96 dpi, in pixels per meter
        writer.Write(3780)
        writer.Write(0)
        writer.Write(0)
        for row in 0 .. height - 1 do
            let src = (height - 1 - row) * width * 4
            let dst = 40 + row * stride
            for col in 0 .. width - 1 do
                dib[dst + col * 3] <- bgra[src + col * 4]
                dib[dst + col * 3 + 1] <- bgra[src + col * 4 + 1]
                dib[dst + col * 3 + 2] <- bgra[src + col * 4 + 2]
        dib

    /// The part of the given frame that shows the world: the world
    /// canvas's corners, through the view's fit and zoom, clipped to
    /// the frame.
    let private getWorldRect (frame : Visual) =
        Avalonia.VisualTree.VisualExtensions.GetVisualDescendants(frame)
            |> Seq.tryPick (function :? Canvas as canvas -> Some canvas | _ -> None)
            |> Option.bind (fun canvas ->
                let corner x y = canvas.TranslatePoint(Point(x, y), frame)
                let a, b = corner 0.0 0.0, corner canvas.Bounds.Width canvas.Bounds.Height
                if a.HasValue && b.HasValue then
                    let world = Rect(topLeft = a.Value, bottomRight = b.Value)
                    Some (world.Intersect(Rect(frame.Bounds.Size)))
                else None)

    /// Copies the visible part of the world shown in the given frame to
    /// the clipboard.
    let copy (frame : Control) =
        let topLevel = TopLevel.GetTopLevel(frame)
        match getWorldRect frame, topLevel.TryGetPlatformHandle() with
            | Some rect, handle when not (isNull handle) && rect.Width > 0.0 && rect.Height > 0.0 ->
                let scale = topLevel.RenderScaling   // screen pixels per view unit
                let dpi = Vector(96.0 * scale, 96.0 * scale)
                let frameSize = PixelSize(int (ceil (frame.Bounds.Width * scale)), int (ceil (frame.Bounds.Height * scale)))
                use view = new RenderTargetBitmap(frameSize, dpi)
                view.Render(frame)

                    // crop to the world, on white in case of any gaps
                let pixelRect =
                    PixelRect(
                        int (round (rect.X * scale)), int (round (rect.Y * scale)),
                        int (round (rect.Width * scale)), int (round (rect.Height * scale)))
                        .Intersect(PixelRect(frameSize))
                let width, height = pixelRect.Width, pixelRect.Height
                let bgra = Array.zeroCreate<byte> (width * height * 4)
                let pinned = GCHandle.Alloc(bgra, GCHandleType.Pinned)
                try
                    view.CopyPixels(pixelRect, pinned.AddrOfPinnedObject(), bgra.Length, width * 4)
                finally
                    pinned.Free()
                for i in 0 .. 4 .. bgra.Length - 4 do   // premultiplied, so over white is just adding the uncovered part
                    let uncovered = 255 - int bgra[i + 3]
                    for j in 0 .. 2 do
                        bgra[i + j] <- byte (min 255 (int bgra[i + j] + uncovered))
                    bgra[i + 3] <- 255uy
                use image = new WriteableBitmap(PixelSize(width, height), dpi, PixelFormat.Bgra8888, AlphaFormat.Premul)
                do
                    use buffer = image.Lock()
                    for row in 0 .. height - 1 do
                        Marshal.Copy(bgra, row * width * 4, buffer.Address + nativeint (row * buffer.RowBytes), width * 4)
                use png = new MemoryStream()
                image.Save(png, PngBitmapEncoderOptions.Default)
                Win32.setImage handle.Handle (toDib width height bgra) (png.ToArray())
            | _ -> ()
