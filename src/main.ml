open Raylib
let w = 800
let h = 600
let h = 5
let sizecell = 5
let dt = 1.0 /. 60.0

type vec = float*float


type status = {
    t : vec Grid.t ;
    time : float
}

let dist (x1,y1) (x2,y2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
let norme (x,y) = sqrt (x*.x +. y*.y)
let zero = (0.0,0.0)

let draw st =
    let t = st.t in
      begin_drawing ();
      Grid.iteri (fun (x,y) v  ->
          let v = norme v in
          draw_rectangle (x*sizecell) (y*sizecell) sizecell sizecell (
              Color.create (int_of_float (255.0*.v)) 0 (int_of_float (255.*.(1. -. v))) 255
          )
        ) t ;
  end_drawing ()



let update {t;time} =
    let time' = time in
    let t' = t in
    {t = t'; time = time'}


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
      t = Grid.make (w/sizecell) (h/sizecell) (fun _ -> (0.0,0.0));
      time = 0.0;
    }
  else failwith "window not ready"

let () =
    let st_final = setup () |> loop in
      Printf.printf "Tatie kimono\n"

