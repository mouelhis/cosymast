(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Sebti Mouelhi                                                           *)
(**                                                                                  *)
(** Copyright (c) 2011-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)

 
open ExtLib  
open Toolkit   
open Ode  
open Symbolic 
  
type node_succ = (label, state) H.t  
type node_pred = (label, state DynArray.t) H.t
  
	
(* Labeled transition systems represented by a hash table, *)
(* a state is associated to a node representing            *)
(* the set of outgoing transitions under each label        *)
type lts_hashed = {
  nb_modes : int ; 
  finer_scale : int ;
  hash : (state, (node_succ * node_pred)) H.t
}


(* Initialize a labeled transition system for a given switched system and a lattice description *) 
let initialize_lts_hashed_common (l:lattice) (mds:string array) =
  let dim = l.dim in
  let finer = l.scales in
  let (scs : (state, (node_succ * node_pred)) H.t) = H.create 0 in
  let (src:state) = make_new_fresh_state dim 0 in
  (match l.safe with 
   | Sp bns -> 
     let bni = simple_space l.initial in
     let rec initialize (i:int)  =
      if i <= dim - 1 then
        let lows = lower_ki l bns.(i) finer in
        let lowi = lower_ki l bni.(i) finer in
        let upi = upper_ki l bni.(i) finer in
        let j = ref lowi in
        while !j <= upi do 
          src.(i) <- (!j - lows);
          initialize (i + 1);
          j := !j + truncate (2. ** (float finer));
        done
      else
        begin
          let (newsrc:state) = state_copy src in
          H.add scs newsrc (H.create 0,H.create 0)
        end
     in initialize 0;
   | SpMinusSp (bnss,bnsm) ->
     let bnis = simple_space l.initial in
     let bnim = minus_space l.initial in
     let rec initialize (i:int) (yesm:bool)=
      if i <= dim - 1 then
        let lows = lower_ki l bnss.(i) finer in
        let lowis = lower_ki l bnis.(i) finer in
        let upis = upper_ki l bnis.(i) finer in
        let lowim = nearest_ki l bnim.(i).lower finer in
        let upim = nearest_ki l bnim.(i).upper finer in
        let j = ref lowis in
        while !j <= upis do 
	  src.(i) <- (!j - lows);
          initialize (i + 1) (yesm && (lowim <= !j && !j <= upim));
          j := !j + truncate (2. ** (float finer));
        done
      else
        begin
          let (newsrc:state) = state_copy src in
          if not yesm then H.add scs newsrc (H.create 0,H.create 0)
        end
     in initialize 0 true);
     {
      nb_modes = arrlen mds ;
      finer_scale = finer ;
      hash = scs
     }

let initialize_lts_hashed_multiple (l:lattice) (mds:string array) =
  let dim = l.dim in
  let nmds = arrlen mds in
  let finer = l.scales in
  let (scs : (state, (node_succ * node_pred)) H.t) = H.create 0 in
  let (src:state) = make_new_fresh_state dim 0 in
  (match l.safe with 
   | Sp bns -> 
     let bni = simple_space l.initial in
     let rec initialize (i:int)  =
      if i < dim then
        let lows = lower_ki l bns.(i) finer in
        let lowi = lower_ki l bni.(i) finer in
        let upi = upper_ki l bni.(i) finer in
        let j = ref lowi in
        while !j <= upi do 
          src.(i) <- (!j - lows);
          initialize (i + 1);
          j := !j + truncate (2. ** (float finer));
        done
      else
        begin
          for p = 0 to nmds - 1 do
            let (newsrc:state) = make_pstate_from_pure src p in
            H.add scs newsrc (H.create 0,H.create 0)
	  done	
        end
     in initialize 0;
   | SpMinusSp (bnss,bnsm) ->
     let bnis = simple_space l.initial in
     let bnim = minus_space l.initial in
     let rec initialize (i:int) (yesm:bool)=
      if i < dim  then
        let lows = lower_ki l bnss.(i) finer in
        let lowis = lower_ki l bnis.(i) finer in
        let upis = upper_ki l bnis.(i) finer in
        let lowim = nearest_ki l bnim.(i).lower finer in
        let upim = nearest_ki l bnim.(i).upper finer in
        let j = ref lowis in
        while !j <= upis do 
	  src.(i) <- (!j - lows);
          initialize (i + 1) (yesm && (lowim <= !j && !j <= upim));
          j := !j + truncate (2. ** (float finer));
        done
      else
        begin
          if not yesm then 
            begin
              for p = 0 to nmds - 1 do
	        let (newsrc:state) = make_pstate_from_pure src p in
                H.add scs newsrc (H.create 0,H.create 0)
	      done
            end
        end
     in initialize 0 true);
     {
      nb_modes = nmds ;
      finer_scale = finer ;
      hash = scs
     }
  
let empty_hashed () =
  let (scs : (state, (node_succ * node_pred)) H.t) = H.create 0 in
  {
    nb_modes = Obj.magic () ;
    finer_scale = Obj.magic () ;
    hash = scs
  }

		 
(* The function names reflect their semantics *)
  
let is_state_exist_hashed (lts:lts_hashed) (s:state) =
  H.mem lts.hash s

let is_pure_state_exist_hashed (lts:lts_hashed) (ps:state) =
  let p = ref 0 in
  let exit = ref true in
  while !p < lts.nb_modes -1 && not !exit do
    let s = make_pstate_from_pure ps !p in 
    if is_state_exist_hashed lts s then exit := true
  done;
  !exit 

let is_transition_exist_hashed (lts:lts_hashed) (src:state) (lbl:label) (dest:state) =
  try
    let trs_src = H.find lts.hash src in
    let d = H.find (fst trs_src) lbl in
    let trs_dest = H.find lts.hash dest in
    let srcs = H.find (snd trs_dest) lbl in
    (d = dest && arrmem src srcs);
  with Not_found -> false

let is_transition_exist_oneside_hashed (lts:lts_hashed) (src:state) (lbl:label) (dest:state) =
  try
    let trs_src = H.find lts.hash src in
    let d = H.find (fst trs_src) lbl in
    (d = dest);
  with Not_found -> false    
  
let is_initial_hashed (lts:lts_hashed) (s:state) = 
  let tgtp = snd (H.find lts.hash s) in
  if H.length tgtp = 0
  then true
  else false

let is_final_hashed (lts:lts_hashed) (s:state) = 
  let tgts = fst (H.find lts.hash s) in
  if H.length tgts = 0 
  then true
  else false

let remove_state_hashed (lts:lts_hashed) (s:state) =
  H.remove lts.hash s

let remove_states_hashed (lts:lts_hashed) (set:state array) =
  Array.iter (fun x -> remove_state_hashed lts x) set

let add_state_hashed (lts:lts_hashed) (s:state) =
  H.add lts.hash s (H.create 0, H.create 0)

let add_states_hashed (lts:lts_hashed) (set:state array) =
  Array.iter (fun x -> add_state_hashed lts x) set
  

  

let add_transition_hashed (lts:lts_hashed) (src:state) (lbl:label) (dest:state) =
  let trs_src = H.find lts.hash src in
  let trs_dest = H.find lts.hash dest in
  let tgts = fst trs_src in
  let tgtp = snd trs_dest in
  H.add tgts lbl dest ;
  try 
    (DynArray.add (H.find tgtp lbl) src)
  with Not_found ->
    begin
      H.add tgtp lbl (DynArray.init 1 (fun i -> src))
    end
    
let add_transition_oneside_hashed (lts:lts_hashed) (src:state) (lbl:label) (dest:state) =
  let trs_src = H.find lts.hash src in
  let tgts = fst trs_src in
  H.add tgts lbl dest


let remove_transition_hashed (lts:lts_hashed) (src:state) (lbl:label) (dest:state) =
  let trs_src = H.find lts.hash src in
  let trs_dest = H.find lts.hash dest in
  let tgts = fst trs_src in
  let tgtp = snd trs_dest in 
  H.remove tgts lbl ;
  DynArray.filter (fun x -> x <> src) (H.find tgtp lbl) ;
  if DynArray.length (H.find tgtp lbl) = 0 then  H.remove tgtp lbl 
  
  
let remove_transition_oneside_hashed (lts:lts_hashed) (src:state) (lbl:label) (dest:state) =
  let trs_src = H.find lts.hash src in
  let tgts = fst trs_src in
  H.remove tgts lbl
  
let remove_transitions_filter_hashed (lts:lts_hashed) (src:state) (pdct:label -> bool) = 
  let trs_src = H.find lts.hash src in
  let tgts = fst trs_src in  
  let remove lbl dst =   
    if pdct lbl then 
      begin  
        let trs_dst = H.find lts.hash dst in
        let tgtp = snd trs_dst in
        DynArray.filter (fun x -> x <> src) (H.find tgtp lbl) ;     
        if DynArray.length (H.find tgtp lbl) = 0 then  H.remove tgtp lbl ;
        H.remove tgts lbl
      end 
  in H.iter remove tgts
  
let remove_transitions_filter_oneside_hashed (lts:lts_hashed) (src:state) (pdct:label -> bool) = 
  let trs_src = H.find lts.hash src in
  let tgts = fst trs_src in  
  let remove lbl dst =   
    if pdct lbl then H.remove tgts lbl 
  in H.iter remove tgts
  
let remove_transitions_between2states_hashed (lts:lts_hashed) (src:state) (dest:state) =
  let trs_src = H.find lts.hash src in
  let trs_dest = H.find lts.hash dest in
  let tgts = fst trs_src in
  let tgtp = snd trs_dest in
  let remove lbl dst =   
    if dest = dst then 
      begin  
        H.remove tgts lbl ;
        DynArray.filter (fun x -> x <> src) (H.find tgtp lbl) ;
        if DynArray.length (H.find tgtp lbl) = 0 then  H.remove tgtp lbl ;
      end  
    in H.iter remove tgts
    
let remove_transitions_between2states_oneside_hashed (lts:lts_hashed) (src:state) (dest:state) =
  let trs_src = H.find lts.hash src in
  let tgts = fst trs_src in
  let remove lbl dst =   
    if dest = dst then H.remove tgts lbl
  in H.iter remove tgts


let replace_destination_hashed (lts:lts_hashed) (src:state) (dest:state) (ndest:state) =
  let trs_src = H.find lts.hash src in
  let trs_dest = H.find lts.hash dest in
  let tgts = fst trs_src in
  let tgtp = snd trs_dest in
  let remove lbl dst =   
    if dest = dst then 
      begin  
        H.remove tgts lbl ;
        DynArray.filter (fun x -> x <> src) (H.find tgtp lbl) ;
        if DynArray.length (H.find tgtp lbl) = 0 then  H.remove tgtp lbl ;
        add_transition_hashed lts src lbl ndest
      end 
    in H.iter remove tgts

let replace_destination_oneside_hashed (lts:lts_hashed) (src:state) (dest:state) (ndest:state) =
  let trs_src = H.find lts.hash src in
  let tgts = fst trs_src in
  let remove lbl dst =   
    if dest = dst then 
      begin  
        H.remove tgts lbl ;
        add_transition_hashed lts src lbl ndest
      end 
    in H.iter remove tgts

let remove_transitions_hashed (lts:lts_hashed) (srcs:state array) (dests:state array) =
  let iteration_1 (src:state) =
    let iteration_2 (dest:state) =
      remove_transitions_between2states_hashed lts src dest
    in Array.iter iteration_2 dests
  in Array.iter iteration_1 srcs
  
  
let remove_transitions_oneside_hashed (lts:lts_hashed) (srcs:state array) (dests:state array) =
  let iteration_1 (src:state) =
    let iteration_2 (dest:state) =
      remove_transitions_between2states_oneside_hashed lts src dest
    in Array.iter iteration_2 dests
  in Array.iter iteration_1 srcs

let predecessors_hashed (lts:lts_hashed) (dest:state) =
  let node_to_explore = snd (H.find lts.hash dest) in
  let res = ref [||] in
  let look lbl srcs =
    res := Array.append !res (DynArray.to_array srcs)
  in H.iter look node_to_explore;
  !res

let predecessors_underlabel_hashed (lts:lts_hashed) (dest:state) (lbl:label) =
  let node_to_explore = snd (H.find lts.hash dest) in
  DynArray.to_array (H.find node_to_explore lbl)



let predecessors_of_states_hashed (lts:lts_hashed) (dests:state array) =
  let res = ref [||] in 
  Array.iter (fun x -> res := Array.append !res (predecessors_hashed lts x)) dests ;
  !res

let predecessors_of_states_underlabel_hashed (lts:lts_hashed) (dests:state array) (lbl:label)=
  let res = ref [||] in
  Array.iter (fun x -> res := Array.append !res (predecessors_underlabel_hashed lts x lbl)) dests ;
  !res

let successors_hashed (lts:lts_hashed) (src:state) =
  let node_to_explore = fst (H.find lts.hash src) in
  let res = ref [||] in
  let look lbl dests =
    res := Array.append !res [|dests|]
  in H.iter look node_to_explore;
  !res 
  

  

let successor_underlabel_hashed (lts:lts_hashed) (src:state) (lbl:label)=
  let node_to_explore = fst (H.find lts.hash src) in
  H.find node_to_explore lbl
	

let successors_of_states_hashed (lts:lts_hashed) (srcs:state array) =
  let res = ref [||] in
  Array.iter (fun x -> res := Array.append !res (successors_hashed lts x)) srcs ;
  !res

let successors_of_states_underlabel_hashed (lts:lts_hashed) (srcs:state array) (lbl:label)=
  let res = ref [||] in
  Array.iter (fun x -> res := Array.append !res [|successor_underlabel_hashed lts x lbl|]) srcs ;
  !res
 

(* The predecessor states of dests whose successors are included in dests *)
let exclusive_predecessors_hashed (lts:lts_hashed) (dests:state array) =
  let all_preds = predecessors_of_states_hashed lts dests in
  let all_succs = successors_of_states_hashed lts all_preds in
  let out_dests = array_diff all_succs dests in
  let preds_of_outs = predecessors_of_states_hashed lts out_dests in
  (array_diff all_preds preds_of_outs)


let controllable_labels_hashed (lts:lts_hashed) (src:state) =
  let node_to_explore = fst (H.find lts.hash src) in
  hkeys node_to_explore
  
  
let label_between2states_hashed (lts:lts_hashed) (src:state) (dest:state) =
  let trs_src = H.find lts.hash src in
  let tgts = fst trs_src in
  let l = ref 0 in
  let remove lbl dst =   
    if dest = dst then l := lbl
  in H.iter remove tgts;
  !l
  
let iter_transitions_hashed (lts:lts_hashed) (src:state) (f:label -> state -> unit) = 
  let node_to_explore = fst (H.find lts.hash src) in
  H.iter f node_to_explore
  

(* Is the state src hash other successors than nothisone *)
let has_other_successors_hashed (lts:lts_hashed) (src:state) (nothisone:state) =
  let node = fst (H.find lts.hash src) in
  let yes = ref false in
  let look lbl dest =
    if nothisone <> dest
    then raise Break
  in
  (try
    H.iter look node
   with 
    Break -> yes := true ) ;
   !yes
  
  
(* Is the state src hash other predecessors than nothisone *)
let has_other_predecessors_hashed (lts:lts_hashed) (dest:state) (nothisone:state) =
  let node = snd (H.find lts.hash dest) in
  let yes = ref false in
  let look lbl srcs =
    if arrexists (fun x -> x <> nothisone) srcs 
    then raise Break
  in
  (try
    H.iter look node
   with
    Break -> yes := true ) ;
   !yes


let is_blocking_hashed (lts:lts_hashed) (s:state) =
  H.length (fst (H.find lts.hash s)) > 0
  

	
(* Print a set of transitions in the plotting file *)
let write_transition_hashed (lts:lts_hashed) (write:state -> label -> state -> unit) =
  let iteration s nd =
    H.iter (write s) (fst nd)
  in H.iter iteration lts.hash


let nbtrans_hashed (lts:lts_hashed) =
  let nbt_of_node nd = 
   let nb = ref 0 in
   let iter_labels lb dest = 
    nb := !nb + 1
   in H.iter iter_labels nd;
	!nb 
  in 
  let tot = ref 0 in 
  let iter_nodes s nd =
    tot := !tot + nbt_of_node (fst nd)
  in H.iter iter_nodes lts.hash ; !tot 
	
let nbtrans_of_duration_hashed (lts:lts_hashed) (scale:int) =
  let nbt_of_node nd = 
    let nb = ref 0 in
    let iter_labels lb dest =
      if scale_of_label lb lts.nb_modes = scale then 
        nb := !nb + 1
    in H.iter iter_labels nd;
    !nb 
  in 
  let tot = ref 0 in 
  let iter_nodes s nd =
    tot := !tot + nbt_of_node (fst nd)
  in H.iter iter_nodes lts.hash ; !tot 

let size_controlled_initials_common (lts:lts_hashed) (finer:int)= 
  let size = ref 0 in 
  let iteration src _ = 
    let i = ref 0 in
    let exit = ref false in  
    while !i < arrlen src && !exit = false do 
      if src.(!i) mod truncate (2.0 ** (float finer)) <> 0 then exit := true;
      i := !i + 1
    done ;
    if !exit = false then size := !size + 1  
  in H.iter iteration lts.hash ;
  !size


let size_controlled_initials_multiple (lts:lts_hashed) (finer:int)= 
  let size = ref 0 in 
  let iteration src _ = 
    let i = ref 1 in
    let exit = ref false in  
    while !i < arrlen src && !exit = false do 
      if src.(!i) mod truncate (2.0 ** (float finer)) <> 0 then exit := true;
      i := !i + 1
    done ;
    if !exit = false then size := !size + 1  
  in H.iter iteration lts.hash ;
  !size
  	
  
(* LTS module *)
module Lts  = 
  struct
    type t = lts_hashed  
    
    let labels lts = lts.nb_modes * (lts.finer_scale + 1) - 1 
    let number_of_modes lts = lts.nb_modes
    let size lts = H.length lts.hash 
    let content lts = lts.hash 
    let states lts = aol (hkeys lts.hash)
    let clear lts = H.clear lts.hash

    let proportion lts s = (float (nbtrans_of_duration_hashed lts s) /. (float (nbtrans_hashed lts))) *. 100.0
    let control_ratio lts l f common = 
      if common then ((float (size_controlled_initials_common lts f)) /. (float (initial_safe_space_size l))) *. 100.0
      else ((float (size_controlled_initials_multiple lts f)) /. (float ((initial_safe_space_size l) * lts.nb_modes))) *. 100.0
		
    let initialize l mds common = if common then initialize_lts_hashed_common l mds else initialize_lts_hashed_multiple l mds
    let empty () = empty_hashed ()
    
    let is_initial lts s = is_initial_hashed lts s 
    let is_final lts s = is_final_hashed lts s 
    
    let is_state_exist lts s = is_state_exist_hashed lts s
    let is_pure_state_exist lts ps = is_pure_state_exist_hashed lts ps

    let is_transition_exist lts s l d = is_transition_exist_hashed lts s l d 
    let is_transition_exist_oneside lts s l d = is_transition_exist_oneside_hashed lts s l d 
	  
    let add_state lts s = add_state_hashed lts s
    let add_states lts ss = add_states_hashed lts ss
    let remove_state lts s = remove_state_hashed lts s
    let remove_states lts ss = remove_states_hashed lts ss
    
    let add_transition lts s l d = add_transition_hashed lts s l d
    let replace_destination lts s d nd = replace_destination_hashed lts s d nd
        
    let add_transition_oneside lts s l d = add_transition_oneside_hashed lts s l d
    let replace_destination_oneside lts s d nd = replace_destination_oneside_hashed lts s d nd 
	      
    let remove_transition lts s l d = remove_transition_hashed lts s l d  
    let remove_transitions_filter lts s p = remove_transitions_filter_hashed lts s p
    let remove_transitions_between2states lts s d = remove_transitions_between2states_hashed lts s d
    let remove_transitions lts ss ds = remove_transitions_hashed lts ss ds
    
    let remove_transition_oneside lts s l d = remove_transition_oneside_hashed lts s l d
    let remove_transitions_filter_oneside lts s p = remove_transitions_filter_oneside_hashed lts s p
    let remove_transitions_between2states_oneside lts s d = remove_transitions_between2states_oneside_hashed lts s d
    let remove_transitions_oneside lts ss ds = remove_transitions_oneside_hashed lts ss ds
	  
    let label_between2states lts s d = label_between2states_hashed lts s d
    
    let predecessors lts d = predecessors_hashed lts d
    let predecessors_underlabel lts d lb = predecessors_underlabel_hashed lts d lb
    let predecessors_of_states lts ds = predecessors_of_states_hashed lts ds
    let predecessors_of_states_underlabel lts ds lb = predecessors_of_states_underlabel_hashed lts ds lb
    let exclusive_predecessors lts ds = exclusive_predecessors_hashed lts ds
		
    let successors lts s = successors_hashed lts s
    let successor_underlabel lts s lb = successor_underlabel_hashed lts s lb
    let successors_of_states lts ss = successors_of_states_hashed lts ss
    let successors_of_states_underlabel lts ss lb = successors_of_states_underlabel_hashed lts ss lb
        
    let controllable_labels lts s = controllable_labels_hashed lts s 
	  
    let has_other_successors lts s ns = has_other_successors_hashed lts s ns
    let has_other_predecessors lts d nd = has_other_predecessors_hashed lts d nd
    let is_blocking lts s = is_blocking_hashed lts s
    
    let write_transition lts wr = write_transition_hashed lts wr
  end


exception EmptyLifo
exception EmptySet 
  

type symbolic = { 
  lat : lattice ;
  modes : string array ; 
  mutable finer : int ; 
  tau : float ;
  mutable lts : Lts.t
}

let initialize_symbolic (t:float) (l:lattice) (mds:string array) (common:bool) =
  {
    lat = l;
    modes = mds;
    finer = l.scales ;
    tau = t ; 
    lts = Lts.initialize l mds common
  }


type 'a lifo = { mutable c : 'a list }
module Stack = 
  struct 
    let empty () = { c = [] } 
    let clear set = set.c <- []    
    let push set s = set.c <- s :: set.c
    let pop set = match set.c with hd :: tl -> set.c <- tl; hd | [] -> raise EmptyLifo
    let remove_top set = match set.c with hd :: tl -> set.c <- tl | [] -> raise EmptyLifo
    let replace_top set n = match set.c with hd :: tl -> set.c <- n::tl | [] -> raise EmptyLifo 
    let top set = match set.c with hd :: _ -> hd | [] -> raise EmptyLifo
    let cardinality set = List.length set.c
    let is_empty set = (set.c = [])   
    let exists p set = List.exists p set.c
    let to_array set = aol set.c
    let iter f set = List.iter f set.c
    let map f set = List.map f set.c

    let create (s:symbolic)  =
      let stk = empty () in
      let iteration src _ = push stk src in H.iter iteration s.lts.hash; stk
      
    let create2 (s:symbolic)  =
      let stk = empty () in
      let iteration src = push stk (src,-1) in List.iter iteration (hkeys s.lts.hash); stk
  end 
  
module Mapping = 
  struct
    let create (s:symbolic) (max:int) =
      let res = H.create max in
      let iteration src _ = H.add res src (false,0) in H.iter iteration s.lts.hash; res
      
    let create_costs (s:symbolic) (max:int) =
      let res = H.create max in
      let iteration src _ = H.add res src 0 in H.iter iteration s.lts.hash; res

    let empty (sz:int) = H.create sz
    let clear set = H.clear set
    let mem set s = H.mem set s
    let find set s = H.find set s
    let add set s v = H.add set s v
    let remove set s = H.remove set s
    let replace set s v = H.replace set s v
    let iter f set = H.iter f set
    let cardinality set = H.length set       
    let get_currscale set s = snd (find set s)
    let get_blocking set s = fst (find set s)
    let set_currscale set s v = H.replace set s (get_blocking set s,v)
    let set_blocking set s v = H.replace set s (v,get_currscale set s)
    let get_cost set s = find set s
    let get_label set s = DynArray.get (find set s) 0
    let get_nbsuccs set s = DynArray.get (find set s) 1
    let get_tmpcost set s = DynArray.get (find set s) 2
    let set_label set s v = DynArray.set (H.find set s) 0 v
    let set_nbsuccs set s v = DynArray.set (H.find set s) 1 v
    let set_tmpcost set s v = DynArray.set (H.find set s) 2 v 
    let set_cost set s v = H.replace set s v       
    
    let increment_nbsuccs set s =
      let fv = H.find set s in 
      let nb = DynArray.get fv 1 in
      DynArray.set (H.find set s) 1 (nb +. 1.) 
      
    let decrement_nbsuccs set s =
      let fv = H.find set s in 
      let nb = DynArray.get fv 1 in
      DynArray.set (H.find set s) 1 (nb -. 1.)
  end 
  
module StateSet =
  struct          
    let empty () = H.create 0
    let clear set = H.clear set
    let add set s = H.add set s () 
    let remove set s = H.remove set s 
    let mem set s = H.mem set s 
    let cardinality set = H.length set  
    let iter f set = H.iter f set
    let keys set = hkeys set     
    let is_empty set = (cardinality set = 0)   

    let create (s:symbolic)  =
      let set = empty () in  
      let iteration src _ =
        H.add set src ()
      in H.iter iteration s.lts.hash ;
      set

    let choose set =
      if cardinality set = 0 then raise EmptySet;
      let get elem key _ = (elem := key; raise Not_found) in
      let elem = ref (Obj.magic ()) in
      let _ = try iter (get elem) set with Not_found -> () in
      !elem
  end

module ArrayStateSet =
  struct
    let empty () = DynArray.create ()      
    let clear set = DynArray.clear set   
    let add set s = DynArray.add set s 
    let remove set s = DynArray.delete set (DynArray.index_of (fun x -> x = s) set)
    let mem set s = try (DynArray.index_of (fun x -> x = s) set) >= 0 with Not_found -> false 
    let cardinality set = DynArray.length set 
    let iter f set = DynArray.iter f set
    let choose set =  DynArray.unsafe_get set
    let keys set = set
    let is_empty set = DynArray.empty set  


    let create (s:symbolic)  =
      let set = DynArray.create () in 
      let iset = StateSet.empty () in
      let iteration src _ =
        let new_src = state_copy src in
        DynArray.append (DynArray.init 1 (fun id -> new_src)) set;
        H.add iset new_src ()
      in H.iter iteration s.lts.hash ;
      (set,iset)
end
