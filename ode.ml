(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Sebti Mouelhi                                                           *)
(**                                                                                  *)
(** Copyright (c) 2011-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)

open Toolkit  


(* The float vector data type (array of floats) representing continuous states *)
type float_vector = float array

(* Matrices represented by float double arrays *)
type float_matrix = float array array


let string_of_float_vector (vec:float_vector) =  
  string_of_float_array vec

let string_of_float_vector_array (lvec:float_vector array) =
  let buff = Buffer.create 100 in
  let add = Buffer.add_string buff in 
  add @@ va "[" ;
  if (Array.length lvec) > 0 then 
    (for i = 0 to (Array.length lvec) - 2 do 
      add @@ va "%s, " (string_of_float_vector lvec.(i))
    done ; 
    add @@ va "%s ]" (string_of_float_vector lvec.((Array.length lvec) - 1))) 
  else
    add @@ va "]" ;
  Buffer.contents buff  

let vector_float_copy (v:float_vector) =
  let r = Array.length v in
  let vv = Array.make r 0. in
  for i = 0 to r - 1 do
    vv.(i) <- v.(i)
  done;
  vv


let matrix_vector_float_prod (m:float_matrix) (v:float_vector) =
  let w = Array.make (Array.length m) 0. in
  for i = 0 to (Array.length m) - 1 do
    let row_input = m.(i) and output = ref 0. in
    for j = 0 to (Array.length v) - 1 do
      output := row_input.(j) *. v.(j) +. !output
    done ;
    w.(i) <- !output ;
  done;
  w
  

let vector_float_plus (v:float_vector) (vv:float_vector) =
  let r = Array.length v in
  for i = 0 to r - 1 do
    vv.(i) <- v.(i) +. vv.(i)
  done;
  vv

let vector_float_scal_mult (x:float) (v:float_vector) =
  let r = Array.length v in
  for i = 0 to r - 1 do
    v.(i) <- x *. v.(i)
  done; 
  v
 
(* The common fourth-order Runge-Kutta method *)
let rk4 (f:float -> float_vector -> float_vector) (nsteps:int) (y0:float_vector) (beginning:float) (ending:float) =
  let step = ( ending -. beginning ) /. ( float nsteps )  
  and x = ref beginning  
  and l = Array.length y0 in
  let y = vector_float_copy y0  
  and ll = l - 1 
  and halfstep = step *. 0.5 in
  for i = 1 to nsteps do
    let k1 = f !x y in
    let kk1 = vector_float_copy k1 in
    let xx = !x +. halfstep in
    let k2 = f xx (vector_float_plus y (vector_float_scal_mult halfstep k1)) in
    let kk2 = vector_float_copy k2 in
    let k3 = f xx (vector_float_plus y (vector_float_scal_mult halfstep k2)) in
    let kk3 = vector_float_copy k3 in
    x := !x +. step ;
    let k4 = f !x (vector_float_plus y (vector_float_scal_mult step k3)) in
    for j = 0 to ll do
      y.(j) <- y.(j) +. ( halfstep *. (kk1.(j) +. k4.(j) ) +. step *. ( kk2.(j) +. kk3.(j) ) ) /. 3. ;
    done ;
  done ;
  y

let rounded digits fl =
  let rfl = fl *. (10.0 ** (float digits)) in
  if (floor ((rfl -. (floor rfl)) *. 10.0)) < 5.0  then
   (floor rfl) /. (10.0 ** (float digits))
  else
   (ceil rfl) /. (10.0 ** (float digits))


 
(* The function returning the nearest .(10 digits) float of a given float *)
let nearest = rounded 10

let nearest_int fl = 
  if fl >= 0.0 then 
    truncate (fl +. 0.5)
  else 
    truncate (fl -. 0.5)
    
let int_of_floor fl = truncate (floor fl)
let int_of_ceil fl = truncate (ceil fl)
  
(* The time dependent functions type *)
type tfun = float -> float

(* The time dependent function of given constant *)
let cst_tfun (cte:float) =
  let f (t:float) = cte in f
  

(* A vector of time dependent functions *)
type vector_tfun = (tfun) array

(* A matrix of time dependent functions *)
type matrix_tfun = (vector_tfun) array


(* The differential equation system type *)
type deqs = float -> float_vector -> float_vector

(* The correspondent float matrix of a given matrix of time dependent *)
(* functions "a" applied to a time parameter "t"                      *)
let get_float_matrix_from_tfunmat (t:float) (a:matrix_tfun) =
  let nr = Array.length a in
  let nc = Array.length a.(0) in
  let res = Array.init nr (fun i -> Array.make nc 0.) in
  for i = 0 to nr -1 do 
    for j = 0 to nc -1 do
      res.(i).(j) <- (a.(i).(j) t)
    done
  done ;
  res  

(* The correspondent float vector of a given vector of time dependent *)
(* functions "a" applied to a time parameter "t"                      *)

let get_float_vector_from_tfunvec (t:float) (b:vector_tfun) =
  let lb = Array.length b in
  let res = Array.make lb 0. in
  for i = 0 to lb -1 do
    res.(i) <- (b.(i) t)
  done ;
  res  
  


  
  
