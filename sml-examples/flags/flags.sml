val op || = Tigr.|| infix ||
val op && = Tigr.&& infix &&

fun makeDemoWindow (w:int, h:int, flags:Tigr.flags) : Tigr.tigr =
    Tigr.window {w=w, h=h, title="Flag tester", flags=flags}

fun drawDemoWindow (win:Tigr.tigr) =
    let val lineColor = Tigr.fromRgb(0w100, 0w100, 0w100)
        val w = Tigr.width win
        val h = Tigr.height win
    in Tigr.line {bmp=win, x0=0, y0=0, x1=w-1, y1=h-1, color=lineColor}
     ; Tigr.line {bmp=win, x0=0, y0=h-1, x1=w-1, y1=0, color=lineColor}
     ; Tigr.rect {bmp=win, x=0, y=0, w=w, h=h, color=Tigr.fromRgb(0w200, 0w10, 0w10)}
     ; Tigr.print {dest=win, font=Tigr.defaultFont(), x=5, y=5,
                   color=Tigr.fromRgb(0w20, 0w200, 0w0),
                   text=Int.toString w ^ "x" ^ Int.toString h}
    end

type toggle = {text:string, checked:bool ref, value:Tigr.flags,
               key:Tigr.key, color:Tigr.color}

fun drawToggle (bmp:Tigr.tigr, toggle:toggle, x:int, y:int, stride:int) =
    let val font = Tigr.defaultFont()
        val height = Tigr.textHeight(font, #text toggle)
        val width = Tigr.textWidth(font, #text toggle)

        val yOffset = stride div 2
        val xOffset = width div ~2

        val () = Tigr.print {dest=bmp, font=font,
                             x=x+xOffset, y=y+yOffset,
                             color= #color toggle,
                             text= #text toggle}

        val yOffset = yOffset + (if !(#checked toggle) then height else height div 3)

        val lineColor =
            let val (r,g,b) = Tigr.toRgb (#color toggle)
            in Tigr.fromRgba(r,g,b,0w240)
            end
    in Tigr.line {bmp=bmp, x0=x+xOffset, y0=y+yOffset, x1=x+xOffset+width,
                  y1=y+yOffset, color=lineColor}
    end

fun main () =
    let val initialW = 400
        val initialH = 400
        val win = makeDemoWindow(initialW, initialH, Tigr.TIGR_FIXED);
        val white = Tigr.fromRgb(0w255, 0w255, 0w255)
        val yellow = Tigr.fromRgb(0w255, 0w255, 0w0)
        val black = Tigr.fromRgb(0w0, 0w0, 0w0)

        fun mk (t,v,k,c) = {text=t,checked=ref false, value=v,
                            key=Tigr.ascii k, color=c}
        val toggles = [
          mk ("(A)UTO",       Tigr.TIGR_AUTO,       #"A", white),
          mk ("(R)ETINA",     Tigr.TIGR_RETINA,     #"R", white),
          mk ("(F)ULLSCREEN", Tigr.TIGR_FULLSCREEN, #"F", white),
          mk ("(2)X",         Tigr.TIGR_2X,         #"2", yellow),
          mk ("(3)X",         Tigr.TIGR_3X,         #"3", yellow),
          mk ("(4)X",         Tigr.TIGR_4X,         #"4", yellow),
          mk ("(N)OCURSOR",   Tigr.TIGR_NOCURSOR,   #"N", white)
        ]

        fun loop win flags =
            if Tigr.closed win orelse Tigr.keyDown(win,Tigr.TK_ESCAPE)
            then Tigr.free win
            else let val () = Tigr.clear(win, black)
                     val () = drawDemoWindow win

                     val numToggles = length toggles
                     val stepY = Tigr.height win div numToggles
                     val toggleX = Tigr.width win div 2

                     val (newFlags,_) =
                      List.foldr (fn (t,(f,y)) =>
                                     (if Tigr.keyDown(win, #key t) then
                                        #checked t := not (!(#checked t))
                                      else ()
                                     ; drawToggle(win, t,toggleX,y,stepY)
                                     ; ( if !(#checked t) then f || #value t else f
                                       , y+stepY ))) (Tigr.TIGR_FIXED, 0) toggles

                     val (win,flags) =
                         if flags <> newFlags then
                           let val modeFlags = Tigr.TIGR_AUTO || Tigr.TIGR_RETINA
                               val modeChange = (flags && modeFlags) <> (newFlags && modeFlags)
                               val (w,h) = if modeChange then (initialW, initialH)
                                           else (Tigr.width win, Tigr.height win)
                               val () = Tigr.free win
                               val win = makeDemoWindow(w, h, newFlags)
                           in (win, newFlags)
                           end
                         else (win, flags)
                 in Tigr.update win
                  ; loop win flags
                 end
    in loop win Tigr.TIGR_FIXED
    end


val () = main ()
