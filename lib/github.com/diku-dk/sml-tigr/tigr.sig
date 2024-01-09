
signature TIGR = sig

type color   (* Color *)
val fromRgb  : word8 * word8 * word8 -> color
val fromRgba : word8 * word8 * word8 * word8 -> color
val toRgba   : color -> word8 * word8 * word8 * word8
val toRgb    : color -> word8 * word8 * word8
val black    : color
val white    : color
val red      : color
val green    : color
val blue     : color
val lerp     : color * color * real -> color

eqtype flags                 (* Window flags *)
val TIGR_FIXED      : flags  (* window's bitmap is a fixed size (default) *)
val TIGR_AUTO       : flags  (* window's bitmap is scaled with the window *)
val TIGR_2X         : flags  (* always enforce (at least) 2X pixel scale *)
val TIGR_3X         : flags  (* always enforce (at least) 3X pixel scale *)
val TIGR_4X         : flags  (* always enforce (at least) 4X pixel scale *)
val TIGR_RETINA     : flags  (* enable retina support on OS X *)
val TIGR_NOCURSOR   : flags  (* hide cursor *)
val TIGR_FULLSCREEN : flags  (* start in full-screen mode *)
val ||              : flags * flags -> flags
val &&              : flags * flags -> flags

type tigr                      (* Bitmap *)
val window        : {w:int, h:int, title:string, flags:flags} -> tigr
val bitmap        : int * int -> tigr
val free          : tigr -> unit
val closed        : tigr -> bool
val update        : tigr -> unit
val beginOpenGL   : tigr -> int
val setPostShader : {bmp:tigr, code:string, size:int} -> unit
val setPostFX     : {bmp:tigr, p1:real, p2:real, p3:real, p4:real} -> unit
val width         : tigr -> int
val height        : tigr -> int

(* Drawing *)
val get        : tigr * int * int -> color
val plot       : tigr * int * int * color -> unit
val clear      : tigr * color -> unit
val fill       : {bmp:tigr, x:int, y:int, w:int, h:int, color:color} -> unit
val line       : {bmp:tigr, x0:int, y0:int, x1:int, y1:int, color:color} -> unit
val rect       : {bmp:tigr, x:int, y:int, w:int, h:int, color:color} -> unit
val fillRect   : {bmp:tigr, x:int, y:int, w:int, h:int, color:color} -> unit
val circle     : {bmp:tigr, x:int, y:int, r:int, color:color} -> unit
val fillCircle : {bmp:tigr, x:int, y:int, r:int, color:color} -> unit
val clip       : {bmp:tigr, cx:int, cy:int, cw:int, ch:int} -> unit

val blit       : {dest:tigr, src:tigr, dx:int, dy:int, sx:int, sy:int, w:int, h:int} -> unit
val blitAlpha  : {dest:tigr, src:tigr, dx:int, dy:int, sx:int, sy:int, w:int, h:int, alpha:real} -> unit
val blitTint   : {dest:tigr, src:tigr, dx:int, dy:int, sx:int, sy:int, w:int, h:int, tint:color} -> unit

type blitmode
val TIGR_KEEP_ALPHA  : blitmode   (* Keep destination alpha value *)
val TIGR_BLEND_ALPHA : blitmode   (* Blend destination alpha (default) *)

val blitMode   : tigr * blitmode -> unit

(* Font printing *)
type codepage
val TCP_ASCII      : codepage           (* Regular 7-bit ASCII *)
val TCP_1252       : codepage           (* Windows 1252 *)
val TCP_UTF32      : codepage           (* Unicode subset *)

type font
val loadFont       : {bitmap:tigr, codepage:codepage} -> font
val freeFont       : font -> unit
val print          : {dest:tigr, font:font, x:int, y:int, color:color, text:string} -> unit

val textWidth      : font * string -> int
val textHeight     : font * string -> int
val defaultFont    : unit -> font (* The built-in font. *)

(* User Input *)
eqtype key
val ascii          : char -> key
val TK_PAD0        : key
val TK_PAD1        : key
val TK_PAD2        : key
val TK_PAD3        : key
val TK_PAD4        : key
val TK_PAD5        : key
val TK_PAD6        : key
val TK_PAD7        : key
val TK_PAD8        : key
val TK_PAD9        : key
val TK_PADMUL      : key
val TK_PADADD      : key
val TK_PADENTER    : key
val TK_PADSUB      : key
val TK_PADDOT      : key
val TK_PADDIV      : key
val TK_F1          : key
val TK_F2          : key
val TK_F3          : key
val TK_F4          : key
val TK_F5          : key
val TK_F6          : key
val TK_F7          : key
val TK_F8          : key
val TK_F9          : key
val TK_F10         : key
val TK_F11         : key
val TK_F12         : key
val TK_BACKSPACE   : key
val TK_TAB         : key
val TK_RETURN      : key
val TK_SHIFT       : key
val TK_CONTROL     : key
val TK_ALT         : key
val TK_PAUSE       : key
val TK_CAPSLOCK    : key
val TK_ESCAPE      : key
val TK_SPACE       : key
val TK_PAGEUP      : key
val TK_PAGEDN      : key
val TK_END         : key
val TK_HOME        : key
val TK_LEFT        : key
val TK_UP          : key
val TK_RIGHT       : key
val TK_DOWN        : key
val TK_INSERT      : key
val TK_DELETE      : key
val TK_LWIN        : key
val TK_RWIN        : key
val TK_NUMLOCK     : key
val TK_SCROLL      : key
val TK_LSHIFT      : key
val TK_RSHIFT      : key
val TK_LCONTROL    : key
val TK_RCONTROL    : key
val TK_LALT        : key
val TK_RALT        : key
val TK_SEMICOLON   : key
val TK_EQUALS      : key
val TK_COMMA       : key
val TK_MINUS       : key
val TK_DOT         : key
val TK_SLASH       : key
val TK_BACKTICK    : key
val TK_LSQUARE     : key
val TK_BACKSLASH   : key
val TK_RSQUARE     : key
val TK_TICK        : key

val keyDown        : tigr * key -> bool
val keyHeld        : tigr * key -> bool
val readChar       : tigr -> key

val mouseX         : tigr -> int
val mouseY         : tigr -> int
val mouseButtons   : tigr -> int
val mouse          : tigr -> int * int * int

(* Bitmap I/O *)
val loadImage      : string -> tigr
val saveImage      : string * tigr -> int

end

(**

[type color] Type of colors.

[fromRgb(r,g,b)] Returns color for the three channels.

[fromRgba(r,g,b,a)] Returns color for the four channels.

[toRgba c] Returns the four channels representing the color c.

[toRgb c] Returns the three channels (r, g, and b) representing the
color c.

[val black] is the color black.

[val white] is the color white.

[val red] is the color red.

[val green] is the color green.

[val blue] is the color blue.

[linterp(c1,c2,f)] Returns the color representing the linear
interpolation (channel-wise) between the colors c1 and c2, using f,
which is assumed to be in the interval [0;1].

[type tigr] Type of bitmaps.

[type winflag] Type of flags for window creation.

[val TIGR_FIXED] Window's bitmap is a fixed size (default).

[val TIGR_AUTO] Window's bitmap is scaled with the window.

[val TIGR_2X] Always enforce (at least) 2X pixel scale.

[val TIGR_3X] Always enforce (at least) 3X pixel scale.

[val TIGR_4X] Always enforce (at least) 4X pixel scale.

[val TIGR_RETINA] Enable retina support on OS X.

[val TIGR_NOCURSOR] Hide cursor.

[val TIGR_FULLSCREEN] Start in full-screen mode.

[window {w:int, h:int, title:string, flags:int}] Creates a new
empty window with a given bitmap size.

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

[bitmap (x,y)] Creates an empty off-screen bitmap.

[free bmp] Deletes a window/bitmap.

[closed bmp] Returns non-zero if the user requested to close a window.

[update bmp] Displays a window's contents on-screen and updates input.

[beginOpenGL bmp] Called before doing direct OpenGL calls and before
update.  Returns non-zero if OpenGL is available.

[setPostShader {bmp:tigr, code:string, size:int}] Sets post shader for
a window.  This replaces the built-in post-FX shader.

[setPostFX {bmp:tigr, p1:real, p2:real, p3:real, p4:real}] Sets
post-FX properties for a window.

  The built-in post-FX shader uses the following parameters:
  p1: hblur - use bilinear filtering along the x-axis (pixels)
  p2: vblur - use bilinear filtering along the y-axis (pixels)
  p3: scanlines - CRT scanlines effect (0-1)
  p4: contrast - contrast boost (1 = no change, 2 = 2X contrast, etc)

[width bmp] Returns the width of bmp.

[height bmp] Returns the height of bmp.

[get (bmp,x,y)] Accesses bmp directly.

[plot (bmp,x,y,color)] Plots a pixel. Clips and blends. For high
performance, just accesses bmp directly.

[clear (bmp,color)] Clears a bitmap to a color.  No blending, no
clipping.

[fill {bmp:tigr, x:int, y:int, w:int, h:int, color:color}] Fills a
rectangular area.  No blending, no clipping.

[line {bmp:tigr, x0:int, y0:int, x1:int, y1:int, color:color}] Draws a
line.  Start pixel is drawn, end pixel is not.  Clips and blends.

[rect {bmp:tigr, x:int, y:int, w:int, h:int, color:color}] Draws an
empty rectangle.  Drawing a 1x1 rectangle yields the same result as
calling tigrPlot.  Clips and blends.

[fillRect {bmp:tigr, x:int, y:int, w:int, h:int, color:color}] Fills a
rectangle.  Fills the inside of the specified rectangular area.
Calling rect followed by fillRect using the same arguments causes no
overdrawing.  Clips and blends.

[circle {bmp:tigr, x:int, y:int, r:int, color:color}] Draws a circle.
Drawing a zero radius circle yields the same result as calling plot.
Drawing a circle with radius one draws a circle three pixels wide.
Clips and blends.

[fillCircle {bmp:tigr, x:int, y:int, r:int, color:color}] Fills a
circle.  Fills the inside of the specified circle.  Calling circle
followed by fillCircle using the same arguments causes no overdrawing.
Filling a circle with zero radius has no effect.  Clips and blends.

[clip {bmp:tigr, cx:int, cy:int, cw:int, ch:int}] Sets clip rect.  Set
to (0, 0, -1, -1) to reset clipping to full bitmap.

[blit {dest, src, dx, dy, sx, sy, w, h}] Copies bitmap data.  dx/dy =
dest co-ordinates, sx/sy = source co-ordinates, w/h = width/height,
RGBAdest = RGBAsrc. Clips, does not blend.

[blitAlpha {dest, src, dx, dy, sx, sy, w, h, alpha}] Same as blit, but
alpha blends the source bitmap with the target using per pixel alpha
and the specified global alpha.

   Ablend = Asrc * alpha
   RGBdest = RGBsrc * Ablend + RGBdest * (1 - Ablend)

   Blit mode == TIGR_KEEP_ALPHA:
   Adest = Adest

   Blit mode == TIGR_BLEND_ALPHA:
   Adest = Asrc * Ablend + Adest * (1 - Ablend)
   Clips and blends.

[blitTint {dest, src, dx, dy, sx, sy, w, h, tint}] Same as blit, but
tints the source bitmap with a color and alpha blends the resulting
source with the destination.

   Rblend = Rsrc * Rtint
   Gblend = Gsrc * Gtint
   Bblend = Bsrc * Btint
   Ablend = Asrc * Atint

   RGBdest = RGBblend * Ablend + RGBdest * (1 - Ablend)

   Blit mode == TIGR_KEEP_ALPHA:
   Adest = Adest

   Blit mode == TIGR_BLEND_ALPHA:
   Adest = Ablend * Ablend + Adest * (1 - Ablend)
   Clips and blends.

[type blitmode] Type of blit modes.

[TIGR_KEEP_ALPHA : blitmode] Keep destination alpha value.

[TIGR_BLEND_ALPHA : blitmode] Blend destination alpha (default).

[blitMode (bmp,blitmode)] Sets destination bitmap blend mode for blit
operations.

[type font] Type of fonts.

[type codepage] Type of code pages.

[TCP_ASCII : codepage] Regular 7-bit ASCII.

[TCP_1252 : codepage] Windows 1252.

[TCP_UTF32 : codepage] Unicode subset.

[loadFont {bitmap:tigr, codepage:codepage}] For ASCII and 1252, the
font bitmap should contain all characters for the given codepage,
excluding the first 32 control codes. For UTF32 - the font bitmap
contains a subset of Unicode characters and must be in the format
generated by tigrFont for UTF32.

[freeFont font] Frees a font.

[print {dest, font, x, y, color, text}] Prints UTF-8 text onto a
bitmap.  Uses the target bitmap blit mode.  See blitTint for details.

[textWidth (font,text)] Returns the width of a string.

[textHeight (font,text)] Returns the height of a string.

[defaultFont()] Returns the built-in font.

[eqtype key] Type of keys (user input).

[ascii char] Returns the key corresponding to char.

[keyDown (win,key)] Reads the keyboard for a window.  Returns non-zero
if a key is pressed.  keyDown tests for the initial press.

[keyHeld (win,key)] Reads the keyboard for a window.  Returns non-zero
if a key is held. keyHeld repeats each frame.

[readChar win] Reads character input for a window.  Returns the
Unicode value of the last key pressed, or 0 if none.

[loadImage filename] Loads a PNG from file (fileName is UTF-8) On
error, returns NULL and sets errno.

[saveImage (filename,bmp)] Saves a PNG to a file (fileName is UTF-8)
On error, returns zero and sets errno.

*)
