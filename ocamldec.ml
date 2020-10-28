(** ocamldec.ml is generated automatically *) 

open Toolkit 
open Ode 
open Symbolic 
open Lts 
open Safety
open Control
open Reachability
open Plot 



let dim = 2 

let mds = [|"m1"; "m2"|] 

let r0 = 1.
let vs = 1.
let rl = 0.05
let rc = rl /. 10.
let xl = 3.
let xc = 70.


let a1_val_0_0 = -. (rl) /. xl
let a1_0_0 (t:float) = a1_val_0_0

let a1_val_0_1 = 0.
let a1_0_1 (t:float) = a1_val_0_1

let a1_val_1_0 = 0.
let a1_1_0 (t:float) = a1_val_1_0

let a1_val_1_1 = (-1. /. xc) *. (1. /. (r0 +. rc))
let a1_1_1 (t:float) = a1_val_1_1

let a1 = [|[|a1_0_0 ; a1_0_1|]; [|a1_1_0 ; a1_1_1|]|]

let a2_val_0_0 = (-1. /. xl) *. (rl +. ((r0 *. rc) /. (r0 +. rc)))
let a2_0_0 (t:float) = a2_val_0_0

let a2_val_0_1 = ((-1. /. xl) *. (r0 /. (r0 +. rc))) /. 5.
let a2_0_1 (t:float) = a2_val_0_1

let a2_val_1_0 = 5. *. (r0 /. (r0 +. rc)) *. (1. /. xc)
let a2_1_0 (t:float) = a2_val_1_0

let a2_val_1_1 = (-1. /. xc) *. (1. /. (r0 +. rc))
let a2_1_1 (t:float) = a2_val_1_1

let a2 = [|[|a2_0_0 ; a2_0_1|]; [|a2_1_0 ; a2_1_1|]|]

let b_val_0 = (vs /. xl)
let b_0 (t:float) = b_val_0

let b_val_1 = 0.
let b_1 (t:float) = b_val_1

let b = [|b_0 ; b_1|]

let aa1_val_0_0 = -0.25
let aa1_0_0 (t:float) = aa1_val_0_0

let aa1_val_0_1 = 1.
let aa1_0_1 (t:float) = aa1_val_0_1

let aa1_val_1_0 = -2.
let aa1_1_0 (t:float) = aa1_val_1_0

let aa1_val_1_1 = -0.25
let aa1_1_1 (t:float) = aa1_val_1_1

let aa1 = [|[|aa1_0_0 ; aa1_0_1|]; [|aa1_1_0 ; aa1_1_1|]|]

let aa2_val_0_0 = -0.25
let aa2_0_0 (t:float) = aa2_val_0_0

let aa2_val_0_1 = 2.
let aa2_0_1 (t:float) = aa2_val_0_1

let aa2_val_1_0 = -1.
let aa2_1_0 (t:float) = aa2_val_1_0

let aa2_val_1_1 = -0.25
let aa2_1_1 (t:float) = aa2_val_1_1

let aa2 = [|[|aa2_0_0 ; aa2_0_1|]; [|aa2_1_0 ; aa2_1_1|]|]

let bb1_val_0 = -0.25
let bb1_0 (t:float) = bb1_val_0

let bb1_val_1 = -2.
let bb1_1 (t:float) = bb1_val_1

let bb1 = [|bb1_0 ; bb1_1|]

let bb2_val_0 = 0.25
let bb2_0 (t:float) = bb2_val_0

let bb2_val_1 = 1.
let bb2_1 (t:float) = bb2_val_1

let bb2 = [|bb2_0 ; bb2_1|]

let init3 = 
  Sp ([|{lower = 1.3000 ; upper = 1.7000}; {lower = 5.7750 ; upper = 5.8000}|])

let safe1 = 
  SpMinusSp ([|{lower = 1.1500 ; upper = 1.5500}; {lower = 5.4500 ; upper = 5.8500}|], [|{lower = 1.2500 ; upper = 1.3500}; {lower = 5.6500 ; upper = 5.7000}|])

let safe2 = 
  Sp ([|{lower = 0.6500 ; upper = 1.6500}; {lower = 4.9500 ; upper = 5.9500}|])

let safe3 = 
  Sp ([|{lower = 1.3000 ; upper = 1.7000}; {lower = 5.7000 ; upper = 5.8000}|])

let safe4 = 
  SpMinusSp ([|{lower = -6.0000 ; upper = 6.0000}; {lower = -4.0000 ; upper = 4.0000}|], [|{lower = -1.5000 ; upper = 1.5000}; {lower = -1.0000 ; upper = 1.0000}|])

let safe4_bad_1 = 
  Sp ([|{lower = -6.0000 ; upper = 6.0000}; {lower = -4.0000 ; upper = 4.0000}|])

let safe4_bad_2 = 
  SpMinusSp ([|{lower = -12.0000 ; upper = 12.0000}; {lower = -8.0000 ; upper = 8.0000}|], [|{lower = -1.5000 ; upper = 1.5000}; {lower = -1.0000 ; upper = 1.0000}|])

let safe5 = 
  Sp ([|{lower = 1.1500 ; upper = 1.5500}; {lower = 5.4500 ; upper = 5.8500}|])

let target2 = 
  Sp ([|{lower = 1.1000 ; upper = 1.6000}; {lower = 5.4000 ; upper = 5.9000}|])

let target3 = 
  Sp ([|{lower = 1.5000 ; upper = 1.6000}; {lower = 5.7000 ; upper = 5.7250}|])

let target4 = 
  Sp ([|{lower = 1.5000 ; upper = 1.6000}; {lower = 5.7750 ; upper = 5.8000}|])

(* the vector fields for x *)

let f_x_0 (t:float) (x:float_vector) = 
 (vector_float_plus (matrix_vector_float_prod (get_float_matrix_from_tfunmat t a1) x) (get_float_vector_from_tfunvec t b))

let f_x_1 (t:float) (x:float_vector) = 
 (vector_float_plus (matrix_vector_float_prod (get_float_matrix_from_tfunmat t a2) x) (get_float_vector_from_tfunvec t b))

let vfs_x () = 
 let res = H.create 0 in
 H.add res 0 f_x_0;
 H.add res 1 f_x_1;
 res

let vfs () = 
 let res = H.create 0 in 
 H.add res "x" (vfs_x ());
 res

let tau = 64. *. 0.5

let eta = 64. *. 3. *. (10. ** -4.)

let fscale = 6

let sdwell = -1

let lat = {
 dim = dim;
 eta = eta;
 scales = fscale;
 safe = safe5;
 initial = safe5;
 target = Obj.magic ()
}

let sys = H.find (vfs ()) "x" 

let synthesis () = 
 let symbo = initialize_symbolic tau lat mds (sdwell = -1) in
 let stime = Sys.time () in
 lazy_safety_synthesis symbo sys 0.000000 sdwell 2;
 let etime = Sys.time () in
 Printf.printf "Ended in %f seconds\n" (etime -. stime);
 Printf.printf "The abstraction size is %d states\n" (Lts.size symbo.lts);
 Printf.printf "The controllability ratio is %.2f\n" (Lts.control_ratio symbo.lts symbo.lat symbo.finer (sdwell = -1));
 Printf.printf "Proportion of transitions of duration %.3f is %.2f\n" ((2.0 ** (-0.0)) *. tau) (Lts.proportion symbo.lts 0);
 Printf.printf "Proportion of transitions of duration %.3f is %.2f\n" ((2.0 ** (-1.0)) *. tau) (Lts.proportion symbo.lts 1);
 Printf.printf "Proportion of transitions of duration %.3f is %.2f\n" ((2.0 ** (-2.0)) *. tau) (Lts.proportion symbo.lts 2);
 Printf.printf "Proportion of transitions of duration %.3f is %.2f\n" ((2.0 ** (-3.0)) *. tau) (Lts.proportion symbo.lts 3);
 Printf.printf "Proportion of transitions of duration %.3f is %.2f\n" ((2.0 ** (-4.0)) *. tau) (Lts.proportion symbo.lts 4);
 Printf.printf "Proportion of transitions of duration %.3f is %.2f\n" ((2.0 ** (-5.0)) *. tau) (Lts.proportion symbo.lts 5);
 Printf.printf "Proportion of transitions of duration %.3f is %.2f\n" ((2.0 ** (-6.0)) *. tau) (Lts.proportion symbo.lts 6);
 plot_trajectory symbo (safety_control symbo sys [|1.35; 5.5|] 200.000000 0.050000 6) 20.000 20.000 true true
