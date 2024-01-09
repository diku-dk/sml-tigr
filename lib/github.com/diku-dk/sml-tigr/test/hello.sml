
val () = print "Hello...\n"

fun main () =
    let val screen = Tigr.window {w=320,h=230, title="Hello from SML", flags=Tigr.TIGR_FIXED}
        val font = Tigr.defaultFont()
        val grey = Tigr.fromRgb (0wx80,0wx90,0wxa0)
        val white = Tigr.fromRgb (0wxff,0wxff,0wxff)
        fun loop () =
            if Tigr.closed screen orelse Tigr.keyDown(screen,Tigr.TK_ESCAPE)
            then Tigr.free screen
            else ( Tigr.clear(screen, grey)
                 ; Tigr.print {dest=screen, font=font, x=120, y=110, color=white, text="Hello, world."}
                 ; Tigr.update screen
                 ; loop ())
    in loop ()
    end

val () = main ()
val () = print "Good bye...\n"
