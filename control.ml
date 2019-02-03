(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Sebti Mouelhi                                                           *)
(**                                                                                  *)
(** Copyright (c) 2011-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)

open Toolkit
open Ode
open Symbolic 
open Lts

    
let generic_control 
(sym:symbolic) 
(vfs:(int,deqs) H.t) 
(init:float_vector) 
(final_time:float) 
(delta:float) 
(rk4stp:int) =
  assert (final_time > 0.0) ;
  let bnd = simple_space sym.lat.safe in
  let resc = ref [||] in 
  let resd = ref [||] in  
  let nmds = Lts.number_of_modes sym.lts in 
  let fraction = ((2.0 ** (1.0 -. float sym.finer)) *. sym.lat.eta) /. sqrt (float sym.lat.dim) in
  let time = ref 0.0 in
  let near_init = nearest_state_safety init sym.lat 0 in 
  if snd near_init = false then 
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice for is not safe !" 
  else if not (Lts.is_state_exist sym.lts (fst near_init)) then 
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice is not controllable !"
  else
    begin
      let rec iteration fs curr currt = 
        if currt > final_time then ()
        else
          begin
            let cont = Lts.controllable_labels sym.lts (fst curr) in 
            if List.length cont = 0 then ()
            else
              begin
                (* let slbl = random_element cont in*)
                let slbl = max_of_int_list cont in  
                let md_slbl = mode_of_label slbl nmds in
                let sc_slbl = scale_of_label slbl nmds in
                let rtau = (2.0 ** (-. (float sc_slbl))) *. sym.tau in
                let cfs = ref fs in
                resd := Array.append !resd [|Array.map (fun x -> float x *. fraction) (fst curr)|];
                resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) fs|]  ;
                for i = 1 to truncate (rtau /. delta) do
                  cfs := rk4 (H.find vfs md_slbl) rk4stp !cfs 0.0 delta ;
                  resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) !cfs|]
                done ;
                let succ = Lts.successor_underlabel sym.lts (fst curr) slbl in  
                time := !time +. rtau ;
                iteration !cfs (succ,true) (!time)
              end
         end
      in iteration init near_init 0.0 ;
      (!resc,!resd)
    end

let generic_control_multiple 
(sym:symbolic) 
(vfs:(int,deqs) H.t) 
(curr_mode:int) 
(init:float_vector) 
(final_time:float) 
(delta:float) 
(rk4stp:int) =
  assert (final_time > 0.0) ;
  let bnd = simple_space sym.lat.safe in
  let resc = ref [||] in 
  let resd = ref [||] in  
  let nmds = Lts.number_of_modes sym.lts in 
  let fraction = ((2.0 ** (1.0 -. float sym.finer)) *. sym.lat.eta) /. sqrt (float sym.lat.dim) in
  let time = ref 0.0 in
  let near_init = nearest_state_safety init sym.lat 0 in 
  let pinit = make_pstate_from_pure (fst near_init) curr_mode in
  if snd near_init = false then 
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice for is not safe !" 
  else if not (Lts.is_state_exist sym.lts pinit) then 
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice is not controllable !"
  else
    begin
      let rec iteration fs curr currt = 
        if currt > final_time then ()
        else
          begin
            let cont = Lts.controllable_labels sym.lts (fst curr) in 
            if List.length cont = 0 then ()
            else
              begin
                (* let slbl = random_element cont in*)
                let slbl = max_of_int_list cont in  
                let md_slbl = mode_of_label slbl nmds in
                let sc_slbl = scale_of_label slbl nmds in
                let rtau = (2.0 ** (-. (float sc_slbl))) *. sym.tau in
                let cfs = ref fs in
                resd := Array.append !resd [|Array.map (fun x -> float x *. fraction) (get_pure (fst curr))|];
                resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) fs|]  ;
                for i = 1 to truncate (rtau /. delta) do
                  cfs := rk4 (H.find vfs md_slbl) rk4stp !cfs 0.0 delta ;
                  resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) !cfs|]
                done ;
                let succ = Lts.successor_underlabel sym.lts (fst curr) slbl in  
                time := !time +. rtau ;
                iteration !cfs (succ,true) (!time)
              end
         end
      in iteration init (pinit, snd near_init) 0.0 ;
      (!resc,!resd)
    end


    
let safety_control 
(sym:symbolic) 
(vfs:(int,deqs) H.t) 
(init:float_vector) 
(final_time:float) 
(delta:float) 
(rk4stp:int) =
  assert (final_time > 0.0) ;
  let bnd = simple_space sym.lat.safe in
  let resc = ref [||] in
  let resd = ref [||] in
  let nmds = Lts.number_of_modes sym.lts in
  let time = ref 0.0 in
  let fraction = ((2.0 ** (1.0 -. float sym.finer)) *. sym.lat.eta) /. sqrt (float sym.lat.dim) in
  let near_init = nearest_state_safety init sym.lat 0 in 
  if snd near_init = false then 
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice for is not safe !"
  else if not (Lts.is_state_exist sym.lts (fst near_init)) then 
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice is not controllable !"     
  else
    begin
      let rec iteration fs curr currt =
        if currt > final_time then ()
        else
          begin
            let cont = Lts.controllable_labels sym.lts (fst curr) in
            if List.length cont = 0 then ()
            else
              begin
                let slbl = random_element cont in
                (* let slbl = max_of_int_list cont in*)
                let md_slbl = mode_of_label slbl nmds in
                let sc_slbl = scale_of_label slbl nmds in
                let rtau = (2.0 ** (-. (float sc_slbl))) *. sym.tau in
                let cfs = ref fs in 
                resd := Array.append !resd [|Array.map (fun x -> float x *. fraction) (fst curr)|];
                resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) fs|]  ;
                for i = 1 to truncate (rtau /. delta) do 
                  cfs := rk4 (H.find vfs md_slbl) rk4stp !cfs 0.0 delta ;
                  resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) !cfs|] 
                done ; 
                let succ = Lts.successor_underlabel sym.lts (fst curr) slbl in 
                time := !time +. rtau ;
                let near_cfs = nearest_state_safety !cfs sym.lat sc_slbl in
                if Lts.is_state_exist sym.lts (fst near_cfs) then
                  iteration !cfs ((fst near_cfs),true) (!time)
                else
                  iteration !cfs (succ,true) (!time)
             end
         end
      in iteration init near_init 0.0 ;
      (!resc,!resd)
    end

                      
let safety_control_multiple 
(sym:symbolic) 
(vfs:(int,deqs) H.t) 
(curr_mode:int) 
(init:float_vector) 
(final_time:float) 
(delta:float) 
(rk4stp:int) =
  assert (final_time > 0.0) ;
  let bnd = simple_space sym.lat.safe in
  let resc = ref [||] in
  let resd = ref [||] in
  let nmds = Lts.number_of_modes sym.lts in
  let time = ref 0.0 in
  let fraction = ((2.0 ** (1.0 -. float sym.finer)) *. sym.lat.eta) /. sqrt (float sym.lat.dim) in
  let near_init = nearest_state_safety init sym.lat 0 in
  let pinit = make_pstate_from_pure (fst near_init) curr_mode in  
  if snd near_init = false  then
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice is not safe !" 
  else if not (Lts.is_state_exist sym.lts pinit) then 
    failwith "ERROR(trajectory:generic_control [simulation]): approximation of the initial state on the coarsest lattice is not controllable !" 
  else
    begin
      let rec iteration fs curr currt =
        if currt > final_time then ()
        else
          begin
            let cont = Lts.controllable_labels sym.lts (fst curr) in
            if List.length cont = 0 then ()
            else
              begin
                let slbl = random_element cont in
                (* let slbl = max_of_int_list cont in*)
                let md_slbl = mode_of_label slbl nmds in
                let sc_slbl = scale_of_label slbl nmds in
                let rtau = (2.0 ** (-. (float sc_slbl))) *. sym.tau in
                let cfs = ref fs in 
                resd := Array.append !resd [|Array.map (fun x -> float x *. fraction) (get_pure (fst curr)) |];
                resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) fs|]  ;
                for i = 1 to truncate (rtau /. delta) do 
                  cfs := rk4 (H.find vfs md_slbl) rk4stp !cfs 0.0 delta ;
                  resc := Array.append !resc [|Array.mapi (fun i x -> x -. (float (lower_ki sym.lat bnd.(i) sym.finer) *. fraction)) !cfs|] 
                done ; 
                let succ = Lts.successor_underlabel sym.lts (fst curr) slbl in 
                time := !time +. rtau ;
                let near_cfs = nearest_state_safety !cfs sym.lat sc_slbl in
                let p_near_cfs = make_pstate_from_pure (fst near_cfs) (get_curr_mode succ) in
                if Lts.is_state_exist sym.lts p_near_cfs then iteration !cfs (p_near_cfs,true) (!time)
                else iteration !cfs (succ,true) (!time)
             end
         end
      in iteration init (pinit, snd near_init) 0.0 ;
      (!resc,!resd)
    end
