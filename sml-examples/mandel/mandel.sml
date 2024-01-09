
structure Color = struct
  type color = Tigr.color
  val grey = Tigr.fromRgb(0w127,0w127,0w127)
  fun relax c = Tigr.lerp (c,grey,0.2)

  fun palette (C:int) (i:int) : color =  (* i > C for using the entire palette *)
      let val f = real (i mod C) / real C
          val c4 = Tigr.lerp(Tigr.red,Tigr.green,f)
          val c5 = Tigr.lerp(Tigr.green,Tigr.blue,f)
      in relax(Tigr.lerp(c4,c5,f))
      end

  fun fromReal (r:real) : color =
      let val C = 1000
          val c1 = palette C (floor r)
          val c2 = palette C (floor r + 1)
          val f = r - real(floor r)
      in Tigr.lerp(c1,c2,f)
      end
end

structure Mandel = struct

  fun lerpf (a,b,f) = (1.0-f)*a + f*b

  fun pow2 0 = 1
    | pow2 n = 2*pow2(n-1)
  val bailout : real = real(pow2 8)
  val M = 100 (* max iteration *)

  fun mandel (h,w) ((x1,x2),(y1,y2)) (Py,Px) : real =
      let val x0 = lerpf(x1, x2, real Px / real w)
          val y0 = lerpf(y1, y2, real Py / real h)
          (* Here N = 2^8 is chosen as a reasonable bailout radius. *)
          val innerM = 50
          fun innerloop (i, x, y) =
              if x*x + y*y <= bailout andalso i < innerM
              then innerloop (i+1, x*x - y*y + x0, 2.0*x*y + y0)
              else (i, x+0.0, y+0.0)
          fun loop (i, x, y) =
              let val (j,x,y) = innerloop (0,x,y)
              in if x*x + y*y <= bailout andalso (i+j) < M
                 then loop (i+j,x,y)
                 else (i+j, x+0.0, y+0.0)
              end
          val (i, x, y) = loop (0, 0.0, 0.0)
      in
        (* Avoid floating point issues with points inside the set. *)
        if i < M then
          (* sqrt of inner term removed using log simplification rules *)
          let val log_zn = Math.ln(x*x + y*y) / 2.0
              val nu = Math.ln(log_zn / Math.ln 2.0) / Math.ln 2.0
          (* Rearranging the potential function.
           * Dividing log_zn by log(2) instead of log(N = 1<<8)
           * because we want the entire palette to range from the
           * center to radius 2, NOT our bailout radius. *)
          in real(i+1)-nu
          end
        else real i
      end

  val d1 = ((~2.5,1.0),(~1.0,1.0))
  val d2 = ((~0.95,~0.85),(0.25,0.35))
  val d3 = ((~0.95,~0.9),(0.25,0.3))

  val ds = [d1,d2,d3]

  fun get ds x = List.nth(ds,x mod length ds)
end

fun for (lo,hi) (f:int->unit) : unit =
    let fun loop i = if i >= hi then ()
                     else (f i; loop (i+1))
    in loop lo
    end

val op || = Tigr.|| infix ||
val op && = Tigr.&& infix &&

fun makeWindow (w:int, h:int, flags:Tigr.flags) : Tigr.tigr =
    Tigr.window {w=w, h=h, title="Mandelbrot", flags=flags}

fun drawWindow (win:Tigr.tigr) (p1,p2) =
    let val w = Tigr.width win
        val h = Tigr.height win
        fun drawMandel () =
            for (0,h-1) (fn y =>
              for (0,w-1) (fn x =>
                let val r = Mandel.mandel (h,w) (p1,p2) (y,x)
                    val c = Color.fromReal (r*20.0)
                in Tigr.plot(win,x,y,c)
                end))
        fun pp (x,y) = Real.toString x ^ "," ^ Real.toString y
    in drawMandel ()
     ; Tigr.print {dest=win, font=Tigr.defaultFont(), x=10, y=h-40, color=Tigr.black,
                   text="p1=" ^ pp p1 ^ " | p2=" ^ pp p2 ^ " | screen=" ^ Int.toString w ^ "x" ^ Int.toString h
                        ^ "\nZ/X: zoom in/out | Arrows: move | 1/2/3: defaults | ESC: exit"}
    end

fun main () =
    let val initialW = 400
        val initialH = 400
        val win = makeWindow(initialW, initialH, Tigr.TIGR_AUTO);
        fun keyd k = Tigr.keyDown(win,k)
        fun key k = Tigr.keyHeld(win,k)
        fun loop d =
            if Tigr.closed win orelse keyd Tigr.TK_ESCAPE
            then Tigr.free win
            else let val () = Tigr.clear(win, Tigr.black)
                     val () = drawWindow win d
                     val d =
                         let val ((x1,x2),(y1,y2)) = d
                             val dx = (x2-x1) / 20.0
                             val dy = (y2-y1) / 20.0
                         in if key Tigr.TK_RIGHT then
                              ((x1+dx,x2+dx),(y1,y2))
                            else if key Tigr.TK_LEFT then
                              ((x1-dx,x2-dx),(y1,y2))
                            else if key Tigr.TK_UP then
                              ((x1,x2),(y1-dy,y2-dy))
                            else if key Tigr.TK_DOWN then
                              ((x1,x2),(y1+dy,y2+dy))
                            else if key (Tigr.ascii #"Z") then
                              ((x1+dx/2.0,x2-dx/2.0), (y1+dy/2.0,y2-dy/2.0))
                            else if key (Tigr.ascii #"X") then
                              ((x1-dx/2.0,x2+dx/2.0), (y1-dy/2.0,y2+dy/2.0))
                            else if keyd (Tigr.ascii #"1") then
                              Mandel.d1
                            else if keyd (Tigr.ascii #"2") then
                              Mandel.d2
                            else if keyd (Tigr.ascii #"3") then
                              Mandel.d3
                            else d
                         end
                 in Tigr.update win
                  ; print "hi3\n"
                  ; loop d
                 end
    in loop Mandel.d1
    end

val () = main ()
