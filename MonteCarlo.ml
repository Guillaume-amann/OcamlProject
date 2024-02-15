open Plplot

let ps = 60.0;;
let d = 2.0;;
let r = 0.045;;

let forwards ps d r t =
  (ps -. (d *. exp(-.r *. t))) *. (exp(r *. t));;

  let derivatives x type_ =
    if type_ = "forwards" then
      forwards ps d r x
    else if type_ = "world" then
      x
    else
      -.x  

let graph () =
  (* Sample at 20 points, ranging from -10.0 to 10.0 *)
  let xs = Array.init 21 (fun xi -> float xi -. 10.0) in
  let ys = Array.map (fun x -> derivatives x "forwards") xs in

  (* Initialize PLplot *)
  plinit ();

  (* Set the color for the lines to red *)
  (* Add a "if P/L>0 green else red"*)
  plcol0 3;  (* Color index 0 is black on black
                            1 is red on black
                            2 is yellow on black 
                            3 is green *)

  (* Draw a filled rectangle as the background *)
  pladv 1;  (* Advance the page before drawing the background *)
  plvpor 0.1 0.9 0.1 0.9;  (* Set the viewport to leave some margin *)
  plwind 0.0 10.0 0.0 100.0;  (* Xmin Xmax Ymin Ymax *)

  (* Draw the plot window axes *)
  plbox "bcnst" 0.0 0 "bcnstv" 0.0 0;

  (* Draw the parabola points as a series of line segments *)
  plline xs ys;

  (* End the plotting session *)
  plend ();
  ()

let () = graph ()
