
structure Tigr :> TIGR = struct

type color = word

val op && = Word.andb infix &&
val op || = Word.orb infix ||

fun toW (w:word8) : word = Word.fromInt(Word8.toInt w)
fun toW8 (w:word) : word8 = Word8.fromInt(Word.toInt (w && 0wxff))

fun fromRgba (r:word8, g:word8, b:word8, a:word8) : color =
    toW r || Word.<<(toW g, 0w8) || Word.<<(toW b, 0w16)
        || Word.<<(toW a, 0w24)

fun fromRgb (r,g,b) = fromRgba (r,g,b,0wxff)

fun toRgba (c:color) : word8 * word8 * word8 * word8 =
    (toW8 c,
     toW8 (Word.>>(c,0w8)),
     toW8 (Word.>>(c,0w16)),
     toW8 (Word.>>(c,0w24)))

fun toRgb (c:color) : word8 * word8 * word8 =
    (toW8 c,
     toW8 (Word.>>(c,0w8)),
     toW8 (Word.>>(c,0w16)))

val black    : color = fromRgb(0w0,0w0,0w0)
val white    : color = fromRgb(0w255,0w255,0w255)
val red      : color = fromRgb(0w255,0w0,0w0)
val green    : color = fromRgb(0w0,0w255,0w0)
val blue     : color = fromRgb(0w0,0w0,0w255)

local
  fun c2f c = (real (Word8.toInt c)) / 255.0
  fun f2c f = Word8.fromInt(floor (255.0 * f))
  fun lerpc (x,y,f) = f2c((1.0-f) * c2f x + f * c2f y)  (* lerp channel *)
in
  fun lerp (c1,c2,f) =
      let val (r1,g1,b1,a1) = toRgba c1
          val (r2,g2,b2,a2) = toRgba c2
      in fromRgba(lerpc(r1,r2,f), lerpc(g1,g2,f),
                  lerpc(b1,b2,f), lerpc(a1,a2,f))
      end
end

type tigr = foreignptr     (* Bitmap *)

type flags = word          (* Window flags *)
val TIGR_FIXED = 0w0       (* window's bitmap is a fixed size (default) *)
val TIGR_AUTO = 0w1        (* window's bitmap is scaled with the window *)
val TIGR_2X = 0w2          (* always enforce (at least) 2X pixel scale *)
val TIGR_3X = 0w4          (* always enforce (at least) 3X pixel scale *)
val TIGR_4X = 0w8          (* always enforce (at least) 4X pixel scale *)
val TIGR_RETINA = 0w16     (* enable retina support on OS X *)
val TIGR_NOCURSOR = 0w32   (* hide cursor *)
val TIGR_FULLSCREEN = 0w64 (* start in full-screen mode *)

(*
  Creates a new empty window with a given bitmap size.

  Title is UTF-8.

  In TIGR_FIXED mode, the window is made as large as possible to
  contain an integer-scaled version of the bitmap while still fitting
  on the screen. Resizing the window will adapt the scale in integer
  steps to fit the bitmap.

  In TIGR_AUTO mode, the initial window size is set to the bitmap size
  times the pixel scale. Resizing the window will resize the bitmap
  using the specified scale.  For example, in forced 2X mode, the
  window will be twice as wide (and high) as the bitmap.

  Turning on TIGR_RETINA mode will request full backing resolution on
  OSX, meaning that the effective window size might be integer scaled
  to a larger size. In TIGR_AUTO mode, this means that the Tigr bitmap
  will change size if the window is moved between retina and
  non-retina screens.
*)

fun window {w:int, h:int, title:string, flags:flags} : tigr =
    prim("@tigrWindow", (w,h,title,flags))

(* Creates an empty off-screen bitmap. *)
fun bitmap (w:int, h:int) : tigr =
    prim("@tigrBitmap", (w,h))

(* Deletes a window/bitmap. *)
fun free (bmp:tigr) : unit =
    prim("@tigrFree", bmp)

(* Returns true if the user requested to close a window. *)
fun closed (bmp:tigr) : bool =
    (prim("@tigrClosed", bmp):int) <> 0

(* Displays a window's contents on-screen and updates input. *)
fun update (bmp:tigr) : unit =
    prim("@tigrUpdate", bmp)

(* Called before doing direct OpenGL calls and before tigrUpdate.
   Returns non-zero if OpenGL is available. *)
fun beginOpenGL (bmp:tigr) : int =
    prim("@tigrBeginOpenGL", bmp)

(* Sets post shader for a window.
   This replaces the built-in post-FX shader. *)
fun setPostShader {bmp:tigr, code:string, size:int} : unit =
    prim("@tigrSetPostShader", (bmp, code, size))

(* Sets post-FX properties for a window.

   The built-in post-FX shader uses the following parameters:
   p1: hblur - use bilinear filtering along the x-axis (pixels)
   p2: vblur - use bilinear filtering along the y-axis (pixels)
   p3: scanlines - CRT scanlines effect (0-1)
   p4: contrast - contrast boost (1 = no change, 2 = 2X contrast, etc)
*)

fun setPostFX {bmp:tigr, p1:real, p2:real, p3:real, p4:real} : unit =
    raise Fail "tigrSetPostFX not implemented"
(*    prim("@tigrSetPostFX", (bmp, p1, p2, p3, p4)) *)

fun width (bmp:tigr) : int =
    prim("@tigrWidth", bmp)

fun height (bmp:tigr) : int =
    prim("@tigrHeight", bmp)

(* Drawing *)

(* Helper for reading pixels.
   For high performance, just access bmp->pix directly. *)
fun get (bmp:tigr, x:int, y:int) : color =
    prim("@tigrGet", (bmp,x,y))

(* Plots a pixel.
   Clips and blends.
   For high performance, just access bmp->pix directly. *)
fun plot (bmp:tigr, x:int, y:int, c:color) : unit =
    prim("@tigrPlot", (bmp,x,y,c))

(* Clears a bitmap to a color.
   No blending, no clipping. *)
fun clear (bmp:tigr, color:color) : unit =
    prim("@tigrClear", (bmp,color))

(* Fills a rectangular area.
   No blending, no clipping. *)
fun fill {bmp:tigr, x:int, y:int, w:int, h:int, color:color} : unit =
    prim("@tigrFill", (bmp, x, y, w, h, color))

(* Draws a line.
   Start pixel is drawn, end pixel is not.
   Clips and blends. *)
fun line {bmp:tigr, x0:int, y0:int, x1:int, y1:int, color:color} : unit =
    prim("@tigrLine", (bmp, x0, y0, x1, y1, color))

(* Draws an empty rectangle.
   Drawing a 1x1 rectangle yields the same result as calling tigrPlot.
   Clips and blends. *)
fun rect {bmp:tigr, x:int, y:int, w:int, h:int, color:color} : unit =
    prim("@tigrRect", (bmp, x, y, w, h, color))

(* Fills a rectangle.
   Fills the inside of the specified rectangular area.
   Calling tigrRect followed by tigrFillRect using the same arguments
   causes no overdrawing.
   Clips and blends. *)
fun fillRect {bmp:tigr, x:int, y:int, w:int, h:int, color:color} : unit =
    prim("@tigrFillRect", (bmp, x, y, w, h, color))

(* Draws a circle.
   Drawing a zero radius circle yields the same result as calling tigrPlot.
   Drawing a circle with radius one draws a circle three pixels wide.
   Clips and blends. *)
fun circle {bmp:tigr, x:int, y:int, r:int, color:color} : unit =
    prim("@tigrCircle", (bmp, x, y, r, color))

(* Fills a circle.
   Fills the inside of the specified circle.
   Calling tigrCircle followed by tigrFillCircle using the same arguments
   causes no overdrawing.
   Filling a circle with zero radius has no effect.
   Clips and blends. *)
fun fillCircle {bmp:tigr, x:int, y:int, r:int, color:color} : unit =
    prim("@tigrFillCircle", (bmp, x, y, r, color))

(* Sets clip rect.
   Set to (0, 0, -1, -1) to reset clipping to full bitmap. *)
fun clip {bmp:tigr, cx:int, cy:int, cw:int, ch:int} : unit =
    prim("@tigrClip", (bmp, cx, cy, cw, ch))

(* Copies bitmap data.
   dx/dy = dest co-ordinates
   sx/sy = source co-ordinates
   w/h   = width/height

   RGBAdest = RGBAsrc
   Clips, does not blend. *)
fun blit {dest:tigr, src:tigr, dx:int, dy:int, sx:int, sy:int, w:int, h:int} : unit =
    raise Fail "tigrBlit not implemented"
(*    prim("@tigrBlit", (dest, src, dx, dy, sx, sy, w, h)) *)

(* Same as tigrBlit, but alpha blends the source bitmap with the
   target using per pixel alpha and the specified global alpha.

   Ablend = Asrc * alpha
   RGBdest = RGBsrc * Ablend + RGBdest * (1 - Ablend)

   Blit mode == TIGR_KEEP_ALPHA:
   Adest = Adest

   Blit mode == TIGR_BLEND_ALPHA:
   Adest = Asrc * Ablend + Adest * (1 - Ablend)
   Clips and blends. *)

fun blitAlpha {dest:tigr, src:tigr, dx:int, dy:int, sx:int, sy:int, w:int, h:int, alpha:real} : unit =
    raise Fail "tigrBlitAlpha not implemented"
(* prim("@tigrBlitAlpha", (dest, src, dx, dy, sx, sy, w, h, alpha)) *)

(* Same as tigrBlit, but tints the source bitmap with a color
   and alpha blends the resulting source with the destination.

   Rblend = Rsrc * Rtint
   Gblend = Gsrc * Gtint
   Bblend = Bsrc * Btint
   Ablend = Asrc * Atint

   RGBdest = RGBblend * Ablend + RGBdest * (1 - Ablend)

   Blit mode == TIGR_KEEP_ALPHA:
   Adest = Adest

   Blit mode == TIGR_BLEND_ALPHA:
   Adest = Ablend * Ablend + Adest * (1 - Ablend)
   Clips and blends. *)

fun blitTint {dest:tigr, src:tigr, dx:int, dy:int, sx:int, sy:int, w:int, h:int, tint:color} : unit =
    raise Fail "tigrBlitTint not implemented"
(*    prim("@tigrBlitTint", (dest, src, dx, dy, sx, sy, w, h, tint)) *)

type blitmode = int
val TIGR_KEEP_ALPHA : blitmode = 0   (* Keep destination alpha value *)
val TIGR_BLEND_ALPHA : blitmode = 1  (* Blend destination alpha (default) *)

(* Set destination bitmap blend mode for blit operations. *)
fun blitMode (dest:tigr, mode:blitmode) : unit =
    prim("@tigrBlitMode", (dest, mode))

(* Font printing *)
type codepage = int
val TCP_ASCII : codepage = 0       (* Regular 7-bit ASCII *)
val TCP_1252 : codepage = 1252     (* Windows 1252 *)
val TCP_UTF32 : codepage = 12001   (* Unicode subset *)

type font = foreignptr

(* For ASCII and 1252, the font bitmap should contain all characters
   for the given codepage, excluding the first 32 control codes.

   For UTF32 - the font bitmap contains a subset of Unicode characters
   and must be in the format generated by tigrFont for UTF32. *)

fun loadFont {bitmap:tigr, codepage:codepage} : font =
    prim("@tigrLoadFont", (bitmap, codepage))

(* Frees a font. *)
fun freeFont (font:font) : unit =
    prim("@tigrFreeFont", font)

(* Prints UTF-8 text onto a bitmap.
   Uses the target bitmap blit mode.
   See tigrBlitTint for details. *)
fun print {dest:tigr, font:font, x:int, y:int, color:color, text:string} : unit =
    prim("@tigrPrint", (dest, font, x, y, color, text))

(* Returns the width/height of a string. *)
fun textWidth (font:font, text:string) : int =
    prim("@tigrTextWidth", (font,text))

fun textHeight (font:font, text:string) : int =
    prim("@tigrTextHeight", (font,text))

(* The built-in font. *)
fun defaultFont () : font =
    prim("@tigrFont", ())

(* User Input *)
type key = int
fun ascii (c: char) : key = Char.ord c
val TK_PAD0 = 128
val TK_PAD1 = 129
val TK_PAD2 = 130
val TK_PAD3 = 131
val TK_PAD4 = 132
val TK_PAD5 = 133
val TK_PAD6 = 134
val TK_PAD7 = 135
val TK_PAD8 = 136
val TK_PAD9 = 137
val TK_PADMUL = 138
val TK_PADADD = 139
val TK_PADENTER = 140
val TK_PADSUB = 141
val TK_PADDOT = 142
val TK_PADDIV = 143
val TK_F1 = 144
val TK_F2 = 145
val TK_F3 = 146
val TK_F4 = 147
val TK_F5 = 148
val TK_F6 = 149
val TK_F7 = 150
val TK_F8 = 151
val TK_F9 = 152
val TK_F10 = 153
val TK_F11 = 154
val TK_F12 = 155
val TK_BACKSPACE = 156
val TK_TAB = 157
val TK_RETURN = 158
val TK_SHIFT = 159
val TK_CONTROL = 160
val TK_ALT = 161
val TK_PAUSE = 162
val TK_CAPSLOCK = 163
val TK_ESCAPE = 164
val TK_SPACE = 165
val TK_PAGEUP = 166
val TK_PAGEDN = 167
val TK_END = 168
val TK_HOME = 169
val TK_LEFT = 170
val TK_UP = 171
val TK_RIGHT = 172
val TK_DOWN = 173
val TK_INSERT = 174
val TK_DELETE = 175
val TK_LWIN = 176
val TK_RWIN = 177
val TK_NUMLOCK = 178
val TK_SCROLL = 179
val TK_LSHIFT = 180
val TK_RSHIFT = 181
val TK_LCONTROL = 182
val TK_RCONTROL = 183
val TK_LALT = 184
val TK_RALT = 185
val TK_SEMICOLON = 186
val TK_EQUALS = 187
val TK_COMMA = 188
val TK_MINUS = 189
val TK_DOT = 190
val TK_SLASH = 191
val TK_BACKTICK = 192
val TK_LSQUARE = 193
val TK_BACKSLASH = 194
val TK_RSQUARE = 195
val TK_TICK = 196

(* Returns mouse input for a window. *)
fun mouse (win:tigr) : int * int * int =
    let val xa = Int32Array.array(1,0)
        val ya = Int32Array.array(1,0)
        val ba = Int32Array.array(1,0)
        fun pick a = Int32.toInt(Int32Array.sub(a,0))
    in prim("@tigrMouse", (win, xa, ya, ba))
     ; (pick xa, pick ya, pick ba)
    end

fun mouseX (win:tigr) : int = #1 (mouse win)
fun mouseY (win:tigr) : int = #2 (mouse win)
fun mouseButtons (win:tigr) : int = #3 (mouse win)

(*
typedef struct {
    int x;
    int y;
} TigrTouchPoint;

*)

(* Reads the keyboard for a window.
   Returns true if a key is pressed/held.
   tigrKeyDown tests for the initial press, tigrKeyHeld repeats each frame. *)
fun keyDown (bmp:tigr, key:int) : bool =
    (prim("@tigrKeyDown", (bmp,key)):int) <> 0

fun keyHeld (bmp:tigr, key:int) : bool =
    (prim("@tigrKeyHeld", (bmp,key)):int) <> 0

(* Reads character input for a window.
   Returns the Unicode value of the last key pressed, or 0 if none. *)
fun readChar (bmp:tigr) : key =
    prim("@tigrReadChar", bmp)

(* Bitmap I/O *)

(* Loads a PNG from file (fileName is UTF-8)
   On error, returns NULL and sets errno. *)
fun loadImage (filename:string) : tigr =
    prim("@tigrLoadImage", filename)

(* Saves a PNG to a file (fileName is UTF-8)
   On error, returns zero and sets errno. *)
fun saveImage (filename:string, bmp:tigr) : int =
    prim("@tigrSaveImage", (filename, bmp))

end
