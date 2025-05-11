open Raylib
let window_w = 800
let window_h = 600
let h = 5
let sizecell = 20
let dt = 1.0 /. 60.0
let big_g = 9.81e-10
let alpha = 0.6

type vec = float*float
let iof = int_of_float

(*indicatrice de simulation*)
let s = Grid.make 0.0 (window_w/sizecell) (window_h/sizecell) 1.0
let in_boundaries (x,y) = x >= 0 && x < window_w/sizecell && y >= 0 && y < window_h/sizecell 

type status = {
    u : float Grid.t ;
    v : float Grid.t ;
    time : float
}

let dist (x1,y1) (x2,y2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
let norme (x,y) = sqrt (x*.x +. y*.y)
let zero = (0.0,0.0)

let draw st =
    begin_drawing ();
    let gu = Grid.get st.u in
    let gv = Grid.get st.v in
    Grid.iteri (fun pos _  ->
      let x,y = pos in
      let x,y = x*sizecell, y*sizecell in
      draw_line x y (x + (pos |> gu |> iof)  ) (y + (pos |> gv |> iof) ) Color.raywhite;
      let v = norme (gu pos, gv pos) /. 100.0 in
      draw_rectangle x y sizecell sizecell (
          Color.create (int_of_float (255.0*.v)) 0 (int_of_float (255.*.(1. -. v))) 255
          )
        

      ) s ;
  end_drawing ()

let gravity dt v = Grid.map (fun v -> v -. big_g*.dt) v

let projection (u : float Grid.t) (v : float Grid.t) =
    let s_tot = Grid.mapi (fun (i,j) _ -> 
        Grid.get s (i+1,j) 
        +. Grid.get s (i-1,j)  
        +. Grid.get s (i,j+1) 
        +. Grid.get s (i,j-1) 
    ) s in
    let uh = u.height in
    let uw = u.width in
    let u = ref u in   
    let v = ref v in
    let gu = Grid.get !u in
    let gv = Grid.get !v in 
    let gs = Grid.get s_tot in
    (* on répète le processus N = 40 à 100 fois pour que sa stabilise *)
    for _=0 to 40 do
        let u' = Grid.make 0.0 (window_w/sizecell) (window_h/sizecell) 0.0 in
        let v' = Grid.make 0.0 (window_w/sizecell) (window_h/sizecell) 0.0 in
        let su p = if in_boundaries p then Grid.set u' p else (fun _ -> ()) in 
        let sv p = if in_boundaries p then Grid.set v' p else (fun _ -> ()) in
        for j=0 to uh - 1 do
            for i=0 to uw - 1 do
                let pos = (i,j) in
                let d = gu (i+1,j) -. gu pos +. gv (i,j+1) -. gv pos in
                let d' = alpha *. d /. (Grid.get s_tot pos) in
                if gs pos > 0.0 then begin
                    su pos (gu pos +. d' *. gs (i-1,j)) ;
                    su (i+1,j) (gu (i+1,j) -. d' *. gs (i+1,j));
                    sv pos (gv pos +. d' *. gs (i,j-1)) ;
                    sv (i,j+1) (gv (i,j+1) -. d' *. gs (i,j+1));
                end
            done
        done;
        u := u';
        v := v';
    done;
    !u, !v

let advection u v = u,v 

let update {u;v;time} =
    let time' = time +. dt in
    let v = gravity dt v in
    let u,v = projection u v in
    let u,v = advection u v in
        {u;v;time=time'}


let rec loop st =
  if Raylib.window_should_close () then begin
      Raylib.close_window ();
      print_newline ();
      st
    end
  else begin
      draw st;
      loop (update st)
    end

let setup () =
  Raylib.init_window window_w window_h "Blob";
  if is_window_ready () then {
      u = Grid.make 0.5 (window_w/sizecell) (window_h/sizecell) 0.0;
      v = Grid.make 0.5 (window_w/sizecell) (window_h/sizecell) 0.0;
      time = 0.0;
    }
  else failwith "window not ready"

let () =
    let _ = setup () |> loop in
    Printf.printf "Tatie kimono\n"

