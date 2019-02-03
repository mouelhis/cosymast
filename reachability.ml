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
open Lts  



(************************************************************************)
(* The maximal optimal weakly lazy bounded reachability controller      *)
(************************************************************************)

let compute_starting_recommanded_scale (max_bound:float) (elapsed:float) (finer:int) = 
  truncate (elapsed /. (max_bound /. ((float finer) +. 1.)))
  
let rec explore_depth
  (q:state)
  (elapsed:float)
  (shortest:float ref)
  (vfs:(int,deqs) H.t)
  (max_bound:float)
  (target)
  (steps:int)
  (min_post:(state,float) H.t)
  (ko_pre:(state,float) H.t)
  (max_pre:(state,float) H.t)
  (nmds:int)
  (finer:int)
  (visited:(state,unit) H.t)
  (sym:symbolic)
  (count:int ref) =
    count := !count + 1;
    if elapsed > !shortest || (Mapping.mem ko_pre q && elapsed >= Mapping.find ko_pre q) then infinity
    else if in_target sym.lat q then
      begin
        if not (Lts.is_state_exist sym.lts q) then Lts.add_state sym.lts q;
        shortest := min !shortest elapsed;
        Mapping.replace min_post q 0.;
        Mapping.replace max_pre q elapsed;
        0.
      end
    else if Mapping.mem min_post q && (elapsed +. Mapping.find min_post q) <= max_bound then
      begin
        let mpostq = Mapping.find min_post q in
        shortest := min !shortest (elapsed +. mpostq);
        mpostq
      end
    else
      begin
        let costs_of_modes = Mapping.empty 0 in
        let succ_found = ref false in
        let s = ref 0 in
        while !s <= finer && not !succ_found do
          Mapping.clear costs_of_modes;
          let rq = continuous_of_discrete q sym.lat in
          let rtau = (2.0 ** (-. (float !s))) *. sym.tau in
          let nelapsed = elapsed +. rtau  in
          let succ (p:int) = rk4 (H.find vfs p) steps rq 0.0 rtau in
          let min_cp = ref infinity in
          for p = 0 to nmds - 1 do
            let succp = nearest_state_safety (succ p) sym.lat !s in
            let q' = fst succp in
            let q'safe = snd succp in
            if q'safe && not (StateSet.mem visited q') then
              begin
                StateSet.add visited q';
                let cp = (explore_depth q' nelapsed shortest vfs max_bound target steps min_post ko_pre max_pre nmds finer visited sym count) in
                StateSet.remove visited q';
                if cp < !min_cp then (min_cp := cp; Mapping.clear costs_of_modes);
                Mapping.replace costs_of_modes p q'
              end
          done;
          let mpostq = rtau +. !min_cp in
          if mpostq < infinity then
            begin
              if not !succ_found then succ_found := true;
              Mapping.replace min_post q mpostq;
              if Lts.is_state_exist sym.lts q then Lts.remove_transitions_filter_oneside sym.lts q (fun l -> true)
              else Lts.add_state sym.lts q;
              let iter_modes p q' =
                let rlbl = (nmds * !s) + p in
                Lts.add_transition_oneside sym.lts q rlbl q'
              in Mapping.iter iter_modes costs_of_modes;
            end;
          s := !s + 1
        done;
        if !succ_found then 
          begin
            Mapping.replace max_pre q elapsed; 
            Mapping.find min_post q
          end
        else
          begin
            Mapping.replace ko_pre q elapsed;
            infinity
          end
      end

  
let cleanup (sym:symbolic) (min_post:(state,float) H.t) (inits) (nmds:int) =
  let remove (q:state)  =
    if not (Mapping.mem min_post q) then Lts.remove_state sym.lts q
  in Array.iter remove (Lts.states sym.lts);
  let convergence = ref false in
  while not !convergence do
    let dom = Lts.states sym.lts in
    let reachables = StateSet.empty () in
    let get_reachable_initials (q:state)  =
      if StateSet.mem inits q then StateSet.add reachables q
    in Array.iter get_reachable_initials dom;
    let pre_size = Lts.size sym.lts in
    let propagate (q:state) =
      if not (in_target sym.lat q) then
        begin
          let succs_q = Lts.successors sym.lts q in
          let opt = ref infinity in
          let sopt = ref (-1) in
          let iter_succs (q':state) =
            let lbl_qq' = Lts.label_between2states sym.lts q q' in
            let scale_lbl_qq' = scale_of_label lbl_qq' nmds in
            if Mapping.mem min_post q' then
              begin
                let min_post_q' = Mapping.find min_post q' in
                if !sopt = -1 then sopt := scale_lbl_qq';
                if !opt > min_post_q' then (opt := min_post_q')
              end
          in Array.iter iter_succs succs_q;
          Mapping.replace min_post q (!opt +. (2.0 ** (-. float !sopt)) *. sym.tau);
          if !opt = infinity then Lts.remove_state sym.lts q
          else
            begin
              Lts.remove_transitions_filter_oneside sym.lts q
                (fun l ->
                  let q' = Lts.successor_underlabel sym.lts q l in
                  not (Mapping.mem min_post q') || Mapping.find min_post q' <> !opt);
              let add_to_reachables (q':state) =
                if not (StateSet.mem reachables q') then StateSet.add reachables q'
              in Array.iter add_to_reachables (Lts.successors sym.lts q)
            end
        end
    in Array.iter propagate dom;
    let remove_unreachables (q:state)=
      if not (StateSet.mem reachables q) then
        Lts.remove_state sym.lts q
    in Array.iter remove_unreachables dom;
    if Lts.size sym.lts = pre_size then convergence := true
  done 
    
  
let mowl_bounded_reachability_synthesis (sym:symbolic) (vfs:(int,deqs) H.t) (time_bound:float) (steps:int) =
  let lat = sym.lat in
  let finer = lat.scales in
  let nmds = Lts.number_of_modes sym.lts in
  let tsp = target_space sym.lat in
  let min_post = Mapping.empty 0 and ko_pre = Mapping.empty 0 and ok_pre = Mapping.empty 0 in
  let initials = ArrayStateSet.create sym in
  let inits = fst (initials) in
  let initsh = snd (initials) in
  let count = ref 0 in
  let stime = Sys.time () in
  let explore_initial (i:state) =
    let shortest_cost = ref time_bound in
    let visited = StateSet.empty () in
    StateSet.add visited i;
    ignore (explore_depth i 0. shortest_cost vfs time_bound tsp steps min_post ko_pre ok_pre nmds finer visited sym count);
    StateSet.remove visited i
  in ArrayStateSet.iter explore_initial inits;
  pf "Elapsed time after exploration: %f\n" (Sys.time () -. stime);
  pf "Size of initials: %d\n" (StateSet.cardinality initsh);
  pf "Number of calls of explore : %d\n" !count;
  cleanup sym min_post initsh nmds

(************************************************)
(* Static lazy bounded reachability controller  *)
(************************************************)
let rec explore_reachability_lsb
  (q:state)
  (elapsed:float)
  (vfs:(int,deqs) H.t)
  (max_bound:float)
  (target:(int * int) array)
  (steps:int)
  (ok_pre:(state,float) H.t)
  (ko_pre:(state,float) H.t)
  (nmds:int)
  (finer:int)
  (visited)
  (sym:symbolic) = 
    if elapsed > max_bound || (Mapping.mem ko_pre q && elapsed >= Mapping.find ko_pre q) then false
    else
      begin 
        if in_target sym.lat q then
          begin
            if not (Lts.is_state_exist sym.lts q) then Lts.add_state sym.lts q;      
            Mapping.replace ok_pre q elapsed;
            true
          end
        else
          begin
            if Mapping.mem ok_pre q && elapsed <= Mapping.find ok_pre q then true 
            else 
              begin
                let succ_found = ref false in
                let s = ref 0 in
                while !s <= finer && not !succ_found do
                  let rq = continuous_of_discrete q sym.lat in
                  let rtau = (2.0 ** (-. (float !s))) *. sym.tau in
                  let nelapsed = elapsed +. rtau  in
                  let succ (p:int) = rk4 (H.find vfs p) steps rq 0.0 rtau in
                  for p = 0 to nmds - 1 do                    
                    let succ_mode = nearest_state_safety (succ p) sym.lat !s in
                    let q' = fst succ_mode in
                    let q'safe = snd succ_mode in
                    if q'safe && not (StateSet.mem visited q')  then
                      begin
                        StateSet.add visited q';
                        let ctrl' = (explore_reachability_lsb  q' nelapsed vfs max_bound target steps ok_pre ko_pre nmds finer visited sym) in
                        StateSet.remove visited q';
                        if ctrl' then
                          begin 
                            if not !succ_found then 
                              begin 
                                succ_found := true;
                                if Lts.is_state_exist sym.lts q then Lts.remove_transitions_filter sym.lts q (fun l -> true)
                                else Lts.add_state sym.lts q
                              end;
                            let rlbl = (nmds * !s) + p in  
                            Lts.add_transition sym.lts q rlbl q';                             
                          end
                      end
                  done;
                  s := !s + 1
                done;
                if !succ_found then 
                  begin
                    Mapping.replace ok_pre q elapsed;  
                    true
                  end 
                else 
                  begin 
                    Mapping.replace ko_pre q elapsed;  
                    false
                  end                  
              end
          end
      end 
      
let cleanup_lsb (sym:symbolic) (ok_pre:(state,float) H.t) (inits) (nmds:int) =
  let remove (q:state)  =
    if not (Mapping.mem ok_pre q) then Lts.remove_state sym.lts q
  in Array.iter remove (Lts.states sym.lts);
  let convergence = ref false in
  while not !convergence do
    let dom = Lts.states sym.lts in
    let pre_size = Lts.size sym.lts in
    let propagate (q:state) =
      if Lts.is_initial sym.lts q && not (StateSet.mem inits q) then
        begin
          Lts.remove_transitions_filter sym.lts q (fun l -> true);
          Lts.remove_state sym.lts q 
        end
    in Array.iter propagate dom;
    if Lts.size sym.lts = pre_size then convergence := true
  done

let lsb_reachability_synthesis (sym:symbolic) (vfs:(int,deqs) H.t) (time_bound:float) (steps:int) =
  let lat = sym.lat in
  let finer = lat.scales in
  let nmds = Lts.number_of_modes sym.lts in
  let tsp = target_space sym.lat in
  let ok_pre = Mapping.empty 0 and ko_pre = Mapping.empty 0 in 
  let inits = StateSet.create sym in
  let explore_initial (i:state) _ =
    let visited = StateSet.empty () in
    StateSet.add visited i;
    swallow (explore_reachability_lsb i 0. vfs time_bound tsp steps ok_pre ko_pre nmds finer visited sym);
    StateSet.remove visited i;
  in StateSet.iter explore_initial inits;  
  cleanup_lsb sym ok_pre inits nmds;
  pf "Size of initials: %d\n" (StateSet.cardinality inits);
  
  

