(* Ray-tracer written by Troels Henriksen, DIKU - modified to work
 * with TIGR colors and the TIGR GUI... *)

fun for (lo,hi) (f:int->unit) : unit =
    let fun loop i = if i >= hi then ()
                     else (f i; loop (i+1))
    in loop lo
    end

structure ForkJoin = struct

fun par (f,g) = (f(),g())
fun alloc n v = Array.tabulate(n,fn _ => v)
fun parfor (f:int->unit) X (lo, hi) : unit =
    if lo >= hi then () else (f lo; parfor f X (lo+1, hi))

end
(* A ray tracer that fires one ray per pixel and only supports
colorued, reflective spheres.  It parallelises two things

 0. The construction of a BVH for accelerating ray lookups
    (divide-and-conquer task parallelism)

 1. The parallel loop across all of the pixels to be computed (data
    parallelism, albeit potentially poorly load balanced)

 *)

structure V = struct

  type vec3 = {x: real, y: real, z: real}

  local
    fun vf f (v1: vec3) (v2: vec3) =
        {x= f (#x v1, #x v2),
         y= f (#y v1, #y v2),
         z= f (#z v1, #z v2)}
  in
    val vec_add = vf (op+)
    val vec_sub = vf (op-)
    val vec_mul = vf (op* )
    val vec_div = vf (op/)
  end

  fun scale s {x,y,z} = {x=s*x, y=s*y, z=s*z} : vec3

  fun dot (v1: vec3) (v2: vec3) =
      let val v3 = vec_mul v1 v2
      in #x v3 + #y v3 + #z v3
      end

  fun norm v = Math.sqrt (dot v v)

  fun normalise v = scale (1.0 / norm v) v

  fun cross ({x=x1, y=y1, z=z1}, {x=x2, y=y2, z=z2}) : vec3 =
      {x=y1*z2-z1*y2, y=z1*x2-x1*z2, z=x1*y2-y1*x2}

end

structure Ray = struct
open V

type aabb = { min: vec3, max: vec3 }

fun min x y : real = Real.min(x,y)

fun max x y : real = Real.max(x,y)

fun enclosing (box0: aabb) (box1: aabb) =
    let val small = { x = min (#x (#min box0)) (#x (#min box1))
                    , y = min (#y (#min box0)) (#y (#min box1))
                    , z = min (#z (#min box0)) (#z (#min box1))
                    }
        val big = { x = max (#x (#max box0)) (#x (#max box1))
                  , y = max (#y (#max box0)) (#y (#max box1))
                  , z = max (#z (#max box0)) (#z (#max box1))
                  }
    in {min=small, max=big}
    end

fun centre (aabb: aabb) =
    { x = (#x (#min aabb) + (#x (#max aabb) - #x (#min aabb))),
      y = (#y (#min aabb) + (#y (#max aabb) - #y (#min aabb))),
      z = (#z (#min aabb) + (#z (#max aabb) - #z (#min aabb)))
    }

datatype 'a bvh = bvh_leaf of aabb * 'a
                | bvh_split of aabb * 'a bvh * 'a bvh

fun bvh_aabb (bvh_leaf (box, _)) = box
  | bvh_aabb (bvh_split (box, _, _)) = box

local
  fun merge cmp ([], ys) = ys
    | merge cmp (xs, []) = xs
    | merge cmp (xs as x::xs', ys as y::ys') =
      case cmp (x, y) of
          GREATER => y :: merge cmp (xs, ys')
        | _       => x :: merge cmp (xs', ys)
  fun sort cmp [] = []
    | sort cmp [x] = [x]
    | sort cmp xs =
      let val ys = List.take (xs, length xs div 2)
          val zs = List.drop (xs, length xs div 2)
      in merge cmp (sort cmp ys, sort cmp zs)
      end
in
  fun mk_bvh f all_objs =
      let fun mk _ _ [] = raise Fail "mk_bvh: no nodes"
            | mk _ _ [x] = bvh_leaf(f x, x)
            | mk d n xs =
              let val axis = case d mod 3 of 0 => #x
                                           | 1 => #y
                                           | _ => #z
                  fun cmp (x, y) =
                      Real.compare(axis(centre(f x)),
                                   axis(centre(f y)))
                  val xs_sorted = sort cmp xs
                  val xs_left = List.take(xs_sorted, n div 2)
                  val xs_right = List.drop(xs_sorted, n div 2)
                  fun do_left () = mk (d+1) (n div 2) xs_left
                  fun do_right () = mk (d+1) (n-(n div 2)) xs_right
                  val (left, right) =
                      if n < 100
                      then (do_left(), do_right())
                      else ForkJoin.par (do_left, do_right)
                  val box = enclosing (bvh_aabb left) (bvh_aabb right)
              in bvh_split (box, left, right)
              end
      in mk 0 (length all_objs) all_objs
      end
end

type pos = vec3
type dir = vec3
type color = Tigr.color

val black = Tigr.black
val white = Tigr.white
val red = Tigr.red
val green = Tigr.green
val blue = Tigr.blue
val cyan = Tigr.lerp(blue,green,0.5)
val yellow = Tigr.lerp(red,green,0.5)

type ray = {origin: pos, dir: dir}

fun point_at_param (ray: ray) t =
    vec_add (#origin ray) (scale t (#dir ray))

type hit = { t: real
           , p: pos
           , normal: dir
           , color: color
           }

type sphere = { pos: pos
              , color: color
              , radius: real
              }

fun sphere_aabb {pos, color=_, radius} =
    {min = vec_sub pos {x=radius, y=radius, z=radius},
     max = vec_add pos {x=radius, y=radius, z=radius}}

fun sw {x,y,z} = {x=y,y=z,z=x}

fun sphere_hit {pos, color, radius} r t_min t_max : hit option =
    let fun try temp =
            if temp < t_max andalso temp > t_min
            then SOME { t = temp
                      , p = point_at_param r temp
                      , normal = scale (1.0/radius)
                                       (vec_sub (point_at_param r temp) pos)
                      , color = color
                      }
            else NONE
        val oc = vec_sub (#origin r) pos
        val a = dot (#dir r) (#dir r)
        val b = dot oc (#dir r)
        val c = dot oc oc - radius*radius
        val discriminant = b*b - a*c
    in if discriminant <= 0.0
       then NONE
       else case try ((~b - Math.sqrt(b*b-a*c))/a) of
                SOME hit => SOME hit
              | NONE => try ((~b + Math.sqrt(b*b-a*c))/a)
    end

fun aabb_hit (aabb:aabb) ({origin, dir}: ray) tmin0 tmax0 =
  let fun iter min' max' origin' dir' tmin' tmax' =
          let val invD = 1.0 / dir'
              val t0 = (min' - origin') * invD
              val t1 = (max' - origin') * invD
              val (t0', t1') = if invD < 0.0 then (t1, t0) else (t0, t1)
              val tmin'' = max t0' tmin'
              val tmax'' = min t1' tmax'
          in (tmin'', tmax'')
          end
      val (tmin1, tmax1) =
          iter
          (#x (#min aabb)) (#x (#max aabb))
          (#x origin) (#x dir)
          tmin0 tmax0
  in if tmax1 <= tmin1 then false
     else let val (tmin2, tmax2) =
                  iter (#y (#min aabb)) (#y (#max aabb))
                  (#y origin) (#y dir)
                  tmin1 tmax1
          in if tmax2 <= tmin2 then false
             else let val (tmin3, tmax3) =
                          iter (#z (#min aabb)) (#z (#max aabb))
                          (#z origin) (#z dir)
                          tmin2 tmax2
                  in not (tmax3 <= tmin3)
                  end
          end
  end

type objs = sphere bvh

fun objs_hit (bvh_leaf (v, s)) r t_min t_max =
    sphere_hit s r t_min t_max
  | objs_hit (bvh_split (box, left, right)) r t_min t_max =
    if not (aabb_hit box r t_min t_max)
    then NONE
    else case objs_hit left r t_min t_max of
             SOME h => (case objs_hit right r t_min (#t h) of
                            NONE => SOME h
                          | SOME h' => SOME h')
           | NONE => objs_hit right r t_min t_max

type camera = { origin: pos
              , llc: pos
              , horizontal: dir
              , vertical: dir
              }

fun camera lookfrom lookat vup vfov aspect =
  let val theta = vfov * Math.pi / 180.0
      val half_height = Math.tan (theta / 2.0)
      val half_width = aspect * half_height
      val origin = lookfrom
      val w = normalise (vec_sub lookfrom lookat)
      val u = normalise (cross (vup, w))
      val v = cross (w, u)
  in { origin = lookfrom
     , llc = vec_sub
             (vec_sub (vec_sub origin (scale half_width u))
                     (scale half_height v)) w
     , horizontal = scale (2.0*half_width) u
     , vertical = scale (2.0*half_height) v
     }
  end

fun get_ray (cam: camera) s t : ray =
    { origin = #origin cam
    , dir = vec_sub (vec_add (vec_add (#llc cam) (scale s (#horizontal cam)))
                             (scale t (#vertical cam)))
                    (#origin cam)
    }

fun reflect v n =
    vec_sub v (scale (2.0 * dot v n) n)

fun scatter (r: ray) (hit: hit) =
    let val reflected =
            reflect (normalise (#dir r)) (#normal hit)
        val scattered = {origin = #p hit, dir = reflected}
    in if dot (#dir scattered) (#normal hit) > 0.0
       then SOME (scattered, #color hit)
       else NONE
    end

fun attenuate (a:color) (c:color) : color =
    let fun mix (a:Word8.word) (c:Word8.word) : Word8.word =
            Word8.fromInt((Word8.toInt a * Word8.toInt c) div 255)
        val (ar,ag,ab) = Tigr.toRgb a
        val (r,g,b) = Tigr.toRgb c
    in Tigr.fromRgb(mix ar r, mix ag g, mix ab b)
    end

fun ray_color objs r D depth =
    case objs_hit objs r 0.001 1000000000.0 of
        SOME hit =>
        (case scatter r hit of
             SOME (scattered, a) =>
             if depth < D
             then attenuate a (ray_color objs scattered D (depth+1))
             else black
           | NONE => black)
      | NONE => let val unit_dir = normalise (#dir r)
                    val t = 0.5 * (#y unit_dir + 1.0)
                    val bg = Tigr.fromRgb(0w127,0w190,0w255)
                in Tigr.lerp (white,bg,t)
                end

fun trace_ray objs width height cam D j i : color =
    let val u = real i / real width
        val v = real j / real height
        val ray = get_ray cam u v
    in ray_color objs ray D 0
    end

val op || = Tigr.|| infix ||
val op && = Tigr.&& infix &&

fun render objs width height cam D (win:Tigr.tigr) : unit =
    let fun pixel j i = trace_ray objs width height cam D j i
    in for (0,height) (fn j => for (0,width) (fn i =>
         Tigr.plot(win,i,j,pixel j i)))
    end

type scene = { camLookFrom: pos
             , camLookAt: pos
             , camFov: real
             , spheres: sphere list
             }

fun from_scene width height (scene: scene) : objs * camera =
  (mk_bvh sphere_aabb (#spheres scene),
   camera (#camLookFrom scene) (#camLookAt scene) {x=0.0, y=1.0, z=0.0}
   (#camFov scene) (real width/real height))

fun tabulate_2d m n f =
    List.concat (List.tabulate (m, fn j => List.tabulate (n, fn i => f (j, i))))

fun rgbbox () : scene =
    let val n = 10
        val k = 60.0

        val leftwall =
            tabulate_2d n n (fn (y, z) =>
                                { pos={x=(~k/2.0),
                                       y=(~k/2.0 + (k/real n) * real y),
                                       z=(~k/2.0 + (k/real n) * real z)}
                                , color=red
                                , radius = (k/(real n*2.0))
                                })

        val midwall =
            tabulate_2d n n (fn (x,y) =>
                                { pos={x=(~k/2.0 + (k/real n) * real x),
                                       y=(~k/2.0 + (k/real n) * real y),
                                       z=(~k/2.0)}
                                , color=yellow
                                , radius = (k/(real n*2.0))})

        val rightwall =
            tabulate_2d n n (fn (y,z) =>
                                { pos={x=(k/2.0),
                                       y=(~k/2.0 + (k/real n) * real y),
                                       z=(~k/2.0 + (k/real n) * real z)}
                                , color=Tigr.blue
                                , radius = (k/(real n*2.0))
                                })


        val bottom =
            tabulate_2d n n (fn (x,z) =>
                                { pos={x=(~k/2.0 + (k/real n) * real x),
                                       y=(~k/2.0),
                                       z=(~k/2.0 + (k/real n) * real z)}
                                , color=white
                                , radius = (k/(real n*2.0))
                                })


    in { spheres = leftwall @ midwall @ rightwall @ bottom
       , camLookFrom = {x=0.0, y=30.0, z=30.0}
       , camLookAt = {x=0.0, y= ~1.0, z= ~1.0}
       , camFov = 75.0
       }
    end

fun irreg () : scene =
    let val n = 100
        val k = 600.0
        val bottom =
            tabulate_2d n n (fn (x,z) =>
                                { pos={x=(~k/2.0 + (k/real n) * real x),
                                       y=0.0,
                                       z=(~k/2.0 + (k/real n) * real z)}
                                , color = white
                                , radius = k/(real n * 2.0)
                                })
    in { spheres = bottom
       , camLookFrom = {x=0.0, y=12.0, z=30.0}
       , camLookAt = {x=0.0, y=10.0, z= ~1.0}
       , camFov = 75.0 }
    end

fun balls () : scene =
    let val sp1 = {pos={x= ~100.0, y=50.0, z= ~30.0},color=white,radius=20.0}
        val sp2 = {pos={x= ~10.0, y= ~50.0, z= ~20.0},color=yellow,radius=45.0}
        val sp3 = {pos={x= ~30.0, y=50.0, z= ~30.0},color=cyan,radius=30.0}
    in { spheres = [sp1,sp2,sp3]
       , camLookFrom = {x=0.0, y=12.0, z=130.0}
       , camLookAt = {x=0.0, y=10.0, z= ~1.0}
       , camFov = 100.0 }
    end

type state = {height:int, width:int, scene:scene, max_depth:int, n:int}

fun withMaxDepth (s:state) md : state =
    {height= #height s,
     width= #width s,
     scene= #scene s,
     n= #n s,
     max_depth= md}

fun init h w scene_name =
    let val scene = case scene_name of
                        "rgbbox" => rgbbox()
                      | "irreg" => irreg()
                      | "balls" => balls()
                      | s => raise Fail ("No such scene: " ^ s)
    in {height=h, width=w, scene=scene, max_depth=5, n=0}
    end

fun makeWindow (w:int, h:int, flags:Tigr.flags) : Tigr.tigr =
    Tigr.window {w=w, h=h, title="Ray", flags=flags}

fun drawWindow win ({width,height,scene,max_depth=D,...}:state) : unit =
    let val (objs, cam) = from_scene width height scene
    in render objs width height cam D win
    end

fun step {scene,width,height,max_depth,n} : state =
    let val {spheres, camLookFrom, camLookAt={x,y,z}, camFov} = scene
        val dx = 5.0 * Math.sin (2.0 * Math.pi * real n / 100.0)
        val dz = 5.0 * Math.cos (2.0 * Math.pi * real n / 100.0)
        val camFov = camFov
        val camLookAt = {x= x, y= y, z= z}
        val spheres =
            case spheres of
                nil => nil
              | ({pos,color,radius}:sphere) :: sps =>
                let val sp' = {pos={x= #x pos + dx,
                                    y= #y pos,
                                    z= #z pos + dz},
                               color=color,
                               radius=radius}
                in sp' :: sps
                end
        val scene :scene = {spheres=spheres,camLookFrom=camLookFrom,camLookAt=camLookAt, camFov=camFov}
    in {scene=scene,height=height,width=width,max_depth=max_depth,n=n+1}
    end

fun main () =
    let val height = CommandLineArgs.parseInt "h" 300
        val width = CommandLineArgs.parseInt "w" 300
        val scene_name = CommandLineArgs.parseString "s" "balls"
        val _ = print ("Using scene '" ^ scene_name ^ "' (-s to switch).\n")

        val win = makeWindow(width, height, Tigr.TIGR_AUTO);
        fun keyd k = Tigr.keyDown(win,k)
        fun key k = Tigr.keyHeld(win,k)
        fun loop (s:state) : state =
              if Tigr.closed win orelse keyd Tigr.TK_ESCAPE
              then (Tigr.free win; s)
              else let val () = Tigr.clear(win, Tigr.black)
                       val s = if #height s <> Tigr.height win orelse #width s <> Tigr.width win
                               then init (Tigr.height win) (Tigr.width win) scene_name
                               else s
                       val () = drawWindow win s
                       val s = if key (Tigr.ascii #"R") then
                                 init (Tigr.height win) (Tigr.width win) scene_name
                               else if keyd (Tigr.ascii #"F") then
                                 withMaxDepth s (Int.max(0, #max_depth s - 1))
                               else if keyd (Tigr.ascii #"D") then
                                 withMaxDepth s (#max_depth s + 1)
                               else s
                       val s = step s
                   in Tigr.update win
                    ; loop s
                   end
    in loop (init height width scene_name); ()
    end
end

val () = Ray.main()
