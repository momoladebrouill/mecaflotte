open Raylib
let w = 800
let h = 600
let h = 5
let sizecell = 5
let dt = 1.0 /. 60.0
let big_g = 9.81
let alpha = 1.9

type vec = float*float

(*indicatrice de simulation*)
let s = Grid.make 0.0 (w/sizecell) (h/sizecell) 1.0
let in_boundaries (x,y) = x >= 0 && x < w/sizecell && y >= 0 && y < h/sizecell 

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
      let v = norme (gu pos, gv pos) /. 100.0 in
      let x,y = pos in
      draw_rectangle (x*sizecell) (y*sizecell) sizecell sizecell (
          Color.create (int_of_float (255.0*.v)) 0 (int_of_float (255.*.(1. -. v))) 255
          )
      ) s ;
  end_drawing ()

let gravity dt v = Grid.map (fun v -> v -. big_g*.dt) v

let projection u v =
    let s_tot = Grid.mapi (fun (i,j) _ -> 
        Grid.get s (i+1,j) 
        +. Grid.get s (i-1,j)  
        +. Grid.get s (i,j+1) 
        +. Grid.get s (i,j-1) 
    ) s in
    let gu = Grid.get u in
    let gv = Grid.get v in 
    let gs = Grid.get s in
    let su p = if in_boundaries p then Grid.set u p else (fun _ -> ()) in 
    let sv p = if in_boundaries p then Grid.set v p else (fun _ -> ()) in
    (* on répète le processus N = 40 à 100 fois pour que sa stabilise *)
    for _=0 to 50 do
        for j=0 to u.height - 1 do
            for i=0 to u.width - 1 do
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
        done
    done

let advection u v = 
let update {u;v;time} =
    let time' = time +. dt in
    let v = gravity dt v in
    projection u v;
    advection u v;
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
  Raylib.init_window w h "Blob";
  if is_window_ready () then {
      u = Grid.make 0.0 (w/sizecell) (h/sizecell) 0.0;
      v = Grid.make 0.0 (w/sizecell) (h/sizecell) 0.0;
      time = 0.0;
    }
  else failwith "window not ready"

let () =
    let _ = setup () |> loop in
    Printf.printf "Tatie kimono\n"

