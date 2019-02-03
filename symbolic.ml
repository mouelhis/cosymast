(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Sebti Mouelhi                                                           *)
(**                                                                                  *)
(** Copyright (c) 2011-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)

open Toolkit  
open Ode 


exception Break  


(* A label represents a mode and a scale *)
type label = int

(* The cardinality of the set of possible labels from a given set of modes and a finer scale *)
let get_labels (mds:string array) (finer:int) = (finer + 1) * arrlen mds - 1  

let scale_of_label (lbl:label) (nbm:int) = lbl / nbm
  
let mode_of_label (lbl:label) (nbm:int) = lbl mod nbm

(**** States ****)  
(* A state is an int vector *)
type int_vector = int array
type state = int_vector

type set_of_states = state array

(* Make a fresh copy of a state *)
let state_copy = Array.copy

let make_new_fresh_state = Array.make

let state_length = Array.length  

let make_pstate_from_pure (s:state) (p:label) = 
  let ls = state_length s in
  let ns = make_new_fresh_state (ls+1) 0 in
  ns.(0) <- p; 
  for i = 1 to ls do
    ns.(i) <- s.(i-1)
  done;
  ns

let get_pure (s:state) = 
  assert (state_length s >= 2);
  let lg = state_length s in 
  Array.sub s 1 (lg - 1)

let get_curr_mode (s:state) = 
  assert (state_length s >= 2);
  s.(0)

(**** State spaces ****)  
(* Float interval *) 
type interval = {lower : float ; upper : float}

(* A state space has two different patterns :                         *)
(* 1) [l_1, u_2] x .... x [l_n, u_n]                                  *)
(* 2) [l_1, u_2] x .... x [l_n, u_n] \ [k_1, k_2] x .... x [k_n, k_n] *)
type state_space = 
  | Sp of interval array
  | SpMinusSp of interval array * interval array

let simple_space (sp:state_space) =
  match sp with
  | Sp (simple) -> simple
  | SpMinusSp (simple1,simple2) -> simple1

let minus_space (sp:state_space) =
  match sp with 
  | Sp (simple) -> simple
  | SpMinusSp (simple1,simple2) -> simple2

let is_simple_space (sp:state_space) =
  match sp with
  | Sp (simple) -> true
  | SpMinusSp (simple1,simple2) -> false

(* A reachability specification given by an initial state-space and a reachable one *)
(* type reach_spec = {target : state_space ; bound : float} *)

  
(* Lattice specification used to define the discrete abstraction *)
type lattice = {
  (* Lattice dimension *)
  dim : int ;
  (* Space sampling parameter eta*) 
  eta : float ;
  (* Finest fractions of time and space sampling parameters *)
  scales : int ;
  (* Safety specification: safe state-space *)
  safe : state_space ;
  (* Initial state space *)
  initial : state_space ;
  (* Reachability specification: target state-space *)
  target : state_space
}

(* Given  eta_0 = eta_2 / 4, lower_fki x 0 (for eta = eta_0) = lower_fki x 2 (for eta = eta_2)*) 
let lower_fki (l:lattice) (b:interval) (scale:int) =
  (b.lower /. (2. *. l.eta)) *. sqrt (float l.dim) *. (2. ** (float scale))
  
(* Given  eta_0 = eta_2 / 4, upper_fki x 0 (for eta = eta_0) = upper_fki x 2 (for eta = eta_2)*)  
let upper_fki (l:lattice) (b:interval) (scale:int) =
  (b.upper /. (2. *. l.eta)) *. sqrt (float l.dim) *. (2. ** (float scale))
    
(* Given  eta_0 = eta_2 / 4, fki x 0 (for eta = eta_0) = fki x 2 (for eta = eta_2)*)  
let fki (l:lattice) (x:float) (scale:int) =
  (x /. (2.  *. l.eta)) *. sqrt (float l.dim) *. (2. ** (float scale))
  
let lower_ki (l:lattice) (b:interval) (scale:int) =
  int_of_ceil (lower_fki l b scale)
  
let upper_ki (l:lattice) (b:interval) (scale:int) =
  int_of_floor (upper_fki l b scale)
  
let nearest_ki (l:lattice) (x:float) (scale:int) =
  nearest_int (fki l x scale)
  
(* The continuous state of a discrete one *)
let continuous_of_discrete (s:state) (l:lattice) =
  let dim = l.dim in
  let bns = simple_space l.safe in
  let eta = l.eta in
  let finer = l.scales in
  let res = Array.make dim 0. in
  for i = 0 to dim - 1 do
    let low = lower_ki l bns.(i) finer in
    let ris_i = s.(i) + low in
    res.(i) <- ((float ris_i) *. (2. *. eta)) /. (sqrt (float dim) *. (2. ** float finer)) 
  done ;  
  res
  
 
let initial_space (l:lattice) = 
  let res = Array.make l.dim (0,0) in 
  let finer = l.scales in
  let bn = simple_space l.initial in
  for i = 0 to l.dim - 1 do
    let low = nearest_ki l bn.(i).lower finer in 
    let up = nearest_ki l bn.(i).upper finer in
    res.(i) <- (low,up)     
  done;
  res
  
let safe_space (l:lattice)  = 
  let res = Array.make l.dim (0,0) in 
  let finer = l.scales in
  let bn = simple_space l.safe in
  for i = 0 to l.dim - 1 do 
    let low = lower_ki l bn.(i) finer in
    let up = upper_ki l bn.(i) finer in
    res.(i) <- (low,up)     
  done;
  res
  
let target_space (l:lattice)  = 
  let res = Array.make l.dim (0,0) in 
  let finer = l.scales in
  let bn = simple_space l.target in
  for i = 0 to l.dim - 1 do
    let low = nearest_ki l bn.(i).lower finer in
    let up = nearest_ki l bn.(i).upper finer in
    res.(i) <- (low,up)     
  done;
  res
  
let in_target (l:lattice) (s:state) = 
  let rsp = target_space l in
  let ssp = safe_space l in
  let intarget = ref true in
  let i = ref 0 in 
  while !i < l.dim && !intarget = true do 
    let lows = fst ssp.(!i)  in
    let value = s.(!i) + lows in
    if not (value >= fst rsp.(!i) && value <= snd rsp.(!i)) then
       intarget := false;
    i := !i + 1
  done;
  !intarget

let nearest_state_safety (state:float_vector) (l:lattice) (scale:int)  =
  let finer = l.scales in
  let ni = Array.make l.dim 0 in
  let i = ref 0 in
  let frac = truncate (2. ** float finer /. 2. ** float scale) in
  let yess = ref true in
  begin 
    match l.safe with
    | Sp bns ->
      while !i < arrlen state && !yess = true do
        let ki = nearest_int ((state.(!i)  /. (2. *. l.eta)) *. sqrt(float l.dim) *. (2. ** (float scale)))  in
        let lows = lower_ki l bns.(!i) scale in
        let ups = upper_ki l bns.(!i) scale in
        ni.(!i) <- (ki - lows) * frac ;
        yess := !yess && (lows <= ki && ki <= ups) ; 
        i := !i + 1
      done;
      (ni,!yess)
    | SpMinusSp (bns1,bns2) -> 
      let yesm = ref true in
      while !i < arrlen state && !yess = true do
        let ki = nearest_int ((state.(!i)  /. (2. *. l.eta)) *. sqrt(float l.dim) *. (2. ** (float scale)))  in
        let lows = lower_ki l bns1.(!i) scale in
        let ups = upper_ki l bns1.(!i) scale in
        let lowm = nearest_ki l bns2.(!i).lower scale in
        let upm = nearest_ki l bns2.(!i).upper scale in
        ni.(!i) <- (ki - lows) * frac ;
        yess := !yess && (lows <= ki && ki <= ups); 
        yesm := !yesm && (lowm <= ki && ki <= upm);
        i := !i + 1
      done;
      if !yess then yess := !yess && not !yesm;
      (ni,!yess)
  end
  
let nearest_reachability_safety (state:float_vector) (l:lattice) (scale:int)  =
  let bnt = simple_space l.target in
  let finer = l.scales in
  let ni = Array.make l.dim 0 in
  let i = ref 0 in
  let frac = truncate (2. ** float finer /. 2. ** float scale) in
  let yess = ref true in
  let yesr = ref true in
  begin 
    match l.safe with
    | Sp bns ->
      while !i < arrlen state && !yess = true do
        let ki = nearest_int ((state.(!i)  /. (2.0 *. l.eta)) *. sqrt(float l.dim) *. (2. ** (float scale)))  in
        let lows = lower_ki l bns.(!i) scale in
        let ups = upper_ki l bns.(!i) scale in
        let lowr = nearest_ki l bnt.(!i).lower scale in
        let upr = nearest_ki l bnt.(!i).upper scale in
        ni.(!i) <- (ki - lows) * frac ;
        yess := !yess && (lows <= ki && ki <= ups) ;
        yesr := !yesr && (lowr <= ki && ki <= upr) ;
        i := !i + 1
      done;
      (ni,!yess,!yesr)
    | SpMinusSp (bns1,bns2) ->
      let yesm = ref true in 
      while !i < arrlen state && !yess = true do
        let ki = nearest_int ((state.(!i)  /. (2.0 *. l.eta)) *. sqrt(float l.dim) *. (2. ** (float scale)))  in
        let lows = lower_ki l bns1.(!i) scale in
        let ups = upper_ki l bns1.(!i) scale in
        let lowm = nearest_ki l bns2.(!i).lower scale in
        let upm = nearest_ki l bns2.(!i).upper scale in
        let lowr = nearest_ki l bnt.(!i).lower scale in
        let upr = nearest_ki l bnt.(!i).upper scale in
        ni.(!i) <- (ki - lows) * frac ;
        yess := !yess && (lows <= ki && ups >= ki) ; 
        yesr := !yesr && (lowr <= ki && upr >= ki) ;
        yesm := !yesm && (lowm <= ki && ki <= upm);
        i := !i + 1
      done;
      if !yess then yess := !yess && not !yesm;
      (ni,!yess,!yesr)
  end
  

 

let initial_safe_space_size (l:lattice) =
  let size = ref 1 in
  begin
    match l.safe with
    | Sp bns -> 
      for i = 0 to l.dim - 1 do
        let up = upper_ki l bns.(i) 0 in
        let low = lower_ki l bns.(i) 0 in
        let steps = up - low in
        size := !size * (steps + 1)
      done;
    | SpMinusSp (bns1,bns2) ->     
      for i = 0 to l.dim - 1 do
        let up = upper_ki l bns1.(i) 0 in
        let low = lower_ki l bns1.(i) 0 in
        let steps = up - low in
        size := !size * (steps + 1)
      done;
      let tmp = ref 1 in
      for i = 0 to l.dim - 1 do
        let up = upper_ki l bns2.(i) 0 in
        let low = lower_ki l bns2.(i) 0 in
        let steps = up - low in
        tmp := !tmp * (steps + 1)
      done;
      size := !size - !tmp    
  end;
  !size

  
(* Functions used to generate ocamldec.ml *)
let are_disjont (space1:interval array) (space2:interval array) =
  assert (Array.length space1 = Array.length space2);
  let disjoint = ref true in
  let i = ref 0 in
  while !i < Array.length space1 && !disjoint = true do 
    if not (space1.(!i).lower < space2.(!i).lower && space1.(!i).upper < space1.(!i).lower) &&
       not (space1.(!i).lower > space2.(!i).upper && space1.(!i).upper > space2.(!i).upper) then 
        disjoint := false;
    i := !i + 1
  done;
  !disjoint

let is_disjoint_from_spaces (space:interval array) (spaces:(interval array) array) = 
  let is_disjoint = ref true in 
  let i = ref 0 in
  let lspaces = Array.length spaces in
  while !i < lspaces && !is_disjoint = true do 
    if not (are_disjont space spaces.(!i)) then is_disjoint := false;
    i := !i + 1
  done;
  !is_disjoint
  
let in_simple_space (dim:int) (fs:float_vector) (space:state_space) =
  assert (is_simple_space space); 
  let insp = ref true in
  let i = ref 0 in 
  while !i < dim && !insp = true do 
    let coord = fs.(!i) in
    if not (coord >= (simple_space space).(!i).lower && coord <= (simple_space space).(!i).upper) then
       insp := false;
    i := !i + 1
  done;
  !insp

let in_space (fs:float_vector) (space:state_space) =
  let insp = ref true in
  (match space with 
   | Sp sp -> 
     let i = ref 0 in
     while !i < arrlen sp && !insp = true do
       let coord = fs.(!i) in
       insp := !insp && (sp.(!i).lower <= coord && coord <= sp.(!i).upper);
       i := !i + 1
     done;
     !insp
   | SpMinusSp (sp,mp) -> 
     let i = ref 0 in
     let inmp = ref true in
     while !i < arrlen sp && !insp = true do
       let coord = fs.(!i) in
       insp := !insp && (sp.(!i).lower <= coord && coord <= sp.(!i).upper);
       inmp := !inmp && (mp.(!i).lower <= coord && coord <= mp.(!i).upper);
       i := !i + 1
     done;
     if !insp then insp := !insp && not !inmp;       
     !insp)

let is_included (space1:interval array) (space2:interval array) = 
  assert (Array.length space1 = Array.length space2);
  let included = ref true in
  let i = ref 0 in 
  while !i < Array.length space1 && !included = true do 
    if not (space1.(!i).lower >= space2.(!i).lower) ||
       not (space1.(!i).upper <= space2.(!i).upper) then 
        included := false;
    i := !i + 1
  done;
  !included
  
  
