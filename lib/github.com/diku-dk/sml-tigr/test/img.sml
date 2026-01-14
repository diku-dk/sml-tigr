
val () = print "Test writing image...\n"

val outfile = "out.png"
fun main () =
    let val img = Tigr.bitmap (320,230)
        val font = Tigr.defaultFont()
        val grey = Tigr.fromRgb (0wx80,0wx90,0wxa0)
        val white = Tigr.fromRgb (0wxff,0wxff,0wxff)
    in Tigr.clear(img, grey)
     ; Tigr.print {dest=img, font=font, x=120, y=110, color=white, text="Hello, world."}
     ; Tigr.saveImage(outfile, img)
     ; Tigr.free img
     ; print ("Wrote file " ^ outfile ^ "\n")
    end

val () = main ()
