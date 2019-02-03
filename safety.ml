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

let rec continuous_exploration
  (sym:symbolic) 
  (vfs:(int,deqs) H.t)
  (p:int)
  (rq:float_vector)
  (steps:int)
  (delta:float)
  (rk4steps:int) =
    if steps = 0 then true
    else
      begin
        let rq' = rk4 (H.find vfs p) rk4steps rq 0. delta in
        if in_space rq' sym.lat.safe then continuous_exploration sym vfs p rq' (steps - 1) delta rk4steps else false
      end
      

let rec explore_safety_common
  (q:state)
  (safes)
  (unsafes) 
  (visited)
  (vfs:(int,deqs) H.t)
  (steps:int)
  (nmds:int)
  (finer:int)
  (delta:float)
  (sym:symbolic) =
    (* q is unsafe *)
    if StateSet.mem unsafes q then false
    (* q is safe *) 
    else if StateSet.mem safes q then true
    (* q was visited before *)
    else if StateSet.mem visited q then (StateSet.add safes q; true)
    else
      begin
        StateSet.add visited q;
        let succ_found = ref false in
        let s = ref 0 in
        while !s <= finer && not !succ_found do
          let rq = continuous_of_discrete q sym.lat in
          let rtau = (2.0 ** (-. (float !s))) *. sym.tau in
          let succ (p:int) = rk4 (H.find vfs p) steps rq 0. rtau in
          for p = 0 to nmds - 1 do
            let succ_p = nearest_state_safety (succ p) sym.lat !s in
            let q' = fst succ_p in
            let q'safe = snd succ_p in
            let cstps = if delta = 0.0 then 0 else truncate (rtau /. delta) in
            let iqq'safe = continuous_exploration sym vfs p rq cstps delta steps in
            if q'safe && iqq'safe then
              begin 
                if explore_safety_common q' safes unsafes visited vfs steps nmds finer delta sym then
                  begin 
                    if not !succ_found then (succ_found := true; if not (Lts.is_state_exist sym.lts q) then Lts.add_state sym.lts q);
                    let rlbl = (nmds * !s) + p in
                    Lts.add_transition_oneside sym.lts q rlbl q' 
                  end 
              end
        done;
        s := !s + 1
       done;
       StateSet.remove visited q;
       (* At least one successor has been found *)
       if !succ_found then (if not (StateSet.mem safes q) then StateSet.add safes q; true)
       (* No successor has been found, add q to unsafe states *)
       else (StateSet.add unsafes q; Lts.remove_state sym.lts q; false)
      end
               
               
let rec explore_safety_multiple
  (qcm:state)
  (safes)
  (unsafes) 
  (visited)
  (vfs:(int,deqs) H.t)
  (steps:int)
  (nmds:int)
  (finer:int)
  (delta:float)
  (dwell:int)
  (sym:symbolic) =
    (* qcm is unsafe *)
    if StateSet.mem unsafes qcm then false
    (* qcm is safe *) 
    else if StateSet.mem safes qcm then true
    (* qcm was visited before *)
    else if StateSet.mem visited qcm then (StateSet.add safes qcm; true)
    else
      begin
        StateSet.add visited qcm;
        let cm = get_curr_mode qcm in 
        let q = get_pure qcm in
        let succ_found = ref false in
        let s = ref 0 in
        while !s <= finer && not !succ_found do
          let rq = continuous_of_discrete q sym.lat in
          let rtau = (2.0 ** (-. (float !s))) *. sym.tau in
          let succ (p:int) = rk4 (H.find vfs p) steps rq 0. rtau in
          for p = 0 to nmds - 1 do
            if p = cm || (p <> cm && !s <= dwell) then 
              begin 
                let succ_p = nearest_state_safety (succ p) sym.lat !s in
                let q' = fst succ_p in
                let q'safe = snd succ_p in
                let cstps = if delta = 0.0 then 0 else truncate (rtau /. delta) in
                let iqq'safe = continuous_exploration sym vfs p rq cstps delta steps in
                if q'safe && iqq'safe then
                  begin 
                    let qcm' = make_pstate_from_pure q' p in
                    if explore_safety_multiple qcm' safes unsafes visited vfs steps nmds finer delta dwell sym then
                      begin 
                        if not !succ_found then (succ_found := true; if not (Lts.is_state_exist sym.lts qcm) then Lts.add_state sym.lts qcm);
                        let rlbl = (nmds * !s) + p in
                        Lts.add_transition_oneside sym.lts qcm rlbl qcm' 
                      end 
                  end 
              end 
          done;
          s := !s + 1
       done;
       StateSet.remove visited qcm;
       (* At least one successor has been found *)
       if !succ_found then (if not (StateSet.mem safes qcm) then StateSet.add safes qcm; true)
       (* No successor has been found, add q to unsafe states *)
       else (StateSet.add unsafes qcm; Lts.remove_state sym.lts qcm; false)
      end 
    
    
type stack_tuple_for_synthesis = 
{
  s : state ;
  mutable lbl : label ;
  mutable succ_found : bool
}

let explore_safety_iterative_common 
  (path:(stack_tuple_for_synthesis) lifo)
  (safes)
  (unsafes)
  (visited)
  (vfs:(int,deqs) H.t)
  (steps:int)
  (nmds:int)
  (finer:int)
  (delta:float)
  (sym:symbolic) =
    let update_top (path:(stack_tuple_for_synthesis) lifo) (s':state) = 
      try
        let top = Stack.top path in
        if not top.succ_found then (top.succ_found <- true; if not (Lts.is_state_exist sym.lts top.s) then Lts.add_state sym.lts top.s);
        Lts.add_transition_oneside sym.lts top.s (top.lbl - 1) s';
      with EmptyLifo -> ()
    in
    while not (Stack.is_empty path) do
      let q_tuple = Stack.top path in
      let q = q_tuple.s in 
      (* q is unsafe *)
      if StateSet.mem unsafes q then Stack.remove_top path
      else
        begin
          let lbl = q_tuple.lbl in
          (* q is safe *)
          if lbl = 0 && StateSet.mem safes q then (Stack.remove_top path; update_top path q)
          (* q was visited before *) 
          else if lbl = 0 && StateSet.mem visited q then (StateSet.add safes q; Stack.remove_top path; update_top path q)
          else
            begin
              let succ_found = q_tuple.succ_found in
              let p = mode_of_label lbl nmds in
              let s = scale_of_label lbl nmds in
              (* At least one successor was found *)
              if p = 0 && succ_found then
                begin
                  StateSet.remove visited q;
                  if not (StateSet.mem safes q) then StateSet.add safes q;
                  Stack.remove_top path;
                  update_top path q;
                end
              else
                begin
                  if s <= finer then
                    begin
                      if lbl = 0 then StateSet.add visited q;
                      let rq = continuous_of_discrete q sym.lat in
                      let rtau = (2.0 ** (-. (float s))) *. sym.tau in
                      let succ (p:int) = rk4 (H.find vfs p) steps rq 0. rtau in
                      let succ_mode = nearest_state_safety (succ p) sym.lat s in
                      let q' = fst succ_mode in
                      let q'safe = snd succ_mode in
                      let cstps = if delta = 0.0 then 0 else truncate (rtau /. delta) in
                      let iqq'safe = continuous_exploration sym vfs p rq cstps delta steps in
                      if q'safe && iqq'safe then Stack.push path {s = q'; lbl = 0; succ_found = false};
                      q_tuple.lbl <- lbl + 1;
                    end
                  else
                    begin
                      (* no successor has been found *)
                      StateSet.remove visited q;
                      StateSet.add unsafes q;
                      Lts.remove_state sym.lts q;
                      Stack.remove_top path
                    end
                end
            end   
        end      
    done
      

let explore_safety_iterative_multiple 
  (path:(stack_tuple_for_synthesis) lifo)
  (safes)
  (unsafes)
  (visited)
  (vfs:(int,deqs) H.t)
  (steps:int)
  (nmds:int)
  (finer:int)
  (delta:float)
  (dwell:int)
  (sym:symbolic) =
    let update_top (path:(stack_tuple_for_synthesis) lifo) (s':state) = 
      try
        let top = Stack.top path in
        if not top.succ_found then (top.succ_found <- true; if not (Lts.is_state_exist sym.lts top.s) then Lts.add_state sym.lts top.s);
        Lts.add_transition_oneside sym.lts top.s (top.lbl - 1) s';
      with EmptyLifo -> ()
    in
    while not (Stack.is_empty path) do
      let q_tuple = Stack.top path in
      let qcm = q_tuple.s in 
      (* q is unsafe *)
      if StateSet.mem unsafes qcm then Stack.remove_top path
      else
        begin
          let lbl = q_tuple.lbl in
          (* q is unsafe *)
          if lbl = 0 && StateSet.mem safes qcm then (Stack.remove_top path; update_top path qcm)
          (* q was visited before *) 
          else if lbl = 0 && StateSet.mem visited qcm then (StateSet.add safes qcm; Stack.remove_top path; update_top path qcm)
          else
            begin
              let succ_found = q_tuple.succ_found in
              let p = mode_of_label lbl nmds in
              let s = scale_of_label lbl nmds in
              (* At least one successor was found *)
              if p = 0 && succ_found then
                begin
                  StateSet.remove visited qcm;
                  if not (StateSet.mem safes qcm) then StateSet.add safes qcm;
                  Stack.remove_top path;
                  update_top path qcm;
                end
              else
                begin
                  if s <= finer then
                    begin                    
                      if lbl = 0 then StateSet.add visited qcm;
                      let cm = get_curr_mode qcm in 
                      let q = get_pure qcm in
                      let rq = continuous_of_discrete q sym.lat in
                      let rtau = (2.0 ** (-. (float s))) *. sym.tau in
                      let succ (p:int) = rk4 (H.find vfs p) steps rq 0. rtau in
                      if p = cm || (p <> cm && s <= dwell) then
                        begin
                          let succ_mode = nearest_state_safety (succ p) sym.lat s in
                          let q' = fst succ_mode in
                          let q'safe = snd succ_mode in
                          let cstps = if delta = 0.0 then 0 else truncate (rtau /. delta) in
                          let iqq'safe = continuous_exploration sym vfs p rq cstps delta steps in
                          if q'safe && iqq'safe then 
                            begin
                              let qcm' = make_pstate_from_pure q' p in
                              Stack.push path {s = qcm'; lbl = 0; succ_found = false}
                            end 
                        end;
                      q_tuple.lbl <- lbl + 1;
                    end
                  else
                    begin
                      (* no successor has been found *)
                      StateSet.remove visited qcm;
                      StateSet.add unsafes qcm;
                      Lts.remove_state sym.lts qcm;
                      Stack.remove_top path
                    end
                end
            end   
        end      
    done
  

let lazy_safety_synthesis (sym:symbolic) (vfs:(int,deqs) H.t) (delta:float) (dwell:int) (steps:int) =
  let lat = sym.lat in
  let finer = lat.scales in
  let nmds = Lts.number_of_modes sym.lts in
  let safes = StateSet.empty () in
  let unsafes = StateSet.empty () in
  let inits = StateSet.create sym in
  let path = Stack.empty () in
  if dwell = -1 then
    let explore_initial (i:state) _ =
      let visited = StateSet.empty () in
      Stack.push path {s = i; lbl = 0; succ_found = false};
      explore_safety_iterative_common path safes unsafes visited vfs steps nmds finer delta sym
      (* ignore (explore_safety_common i safes unsafes visited vfs steps nmds finer delta sym); *)
    in StateSet.iter explore_initial inits
  else 
    let explore_initial (i:state) _ =
      let visited = StateSet.empty () in
      Stack.push path {s = i; lbl = 0; succ_found = false};
      explore_safety_iterative_multiple path safes unsafes visited vfs steps nmds finer delta dwell sym
      (* ignore (explore_safety_multiple i safes unsafes visited vfs steps nmds finer delta dwell sym) *)
    in StateSet.iter explore_initial inits
