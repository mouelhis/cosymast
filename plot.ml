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

let plot_trajectory 
(s:symbolic) 
(traj:float_vector array * float_vector array) 
(xpart:float) 
(ypart:float) 
(both:bool) 
(discrete:bool) =
  let bnd = simple_space s.lat.safe in
  let xcms = (bnd.(0).upper -. bnd.(0).lower) *. xpart in 
  let ycms = (bnd.(1).upper -. bnd.(1).lower) *. ypart in
  let xes = Array.make ((int_of_float xcms) + 1) 0.0 in
  let yes = Array.make ((int_of_float ycms) + 1) 0.0 in
  let cont = fst traj in
  let dis = snd traj in
  let buff = Buffer.create 16 in 
  let add = Buffer.add_string buff in
  add @@ va "\\input rgb.tex \n\n" ;
  add @@ va "\\begin{tikzpicture}[x=%.3fcm/%.3f,y=%.3fcm/%.3f] \n\n" xcms xcms ycms ycms ; (* To fill by the user *)
  add @@ va "\\draw[step=1cm,gray!30,very thin] (-0.2,-0.2) grid (%.3f,%.3f); \n\n" (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (%.3f,-0.2) ; \n" (xcms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (-0.2,%.3f) ; \n" (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,%.3f) -- (%.3f,%.3f) ;\n" (ycms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (%.3f,-0.2) -- (%.3f,%.3f) ;\n\n" (xcms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  for i = 0 to (int_of_float xcms)  do
    xes.(i) <- bnd.(0).lower +. float i *. ((bnd.(0).upper -. bnd.(0).lower) /. xcms)
  done;
  for i = 0 to (int_of_float ycms)  do
    yes.(i) <- bnd.(1).lower +. float i *. ((bnd.(1).upper -. bnd.(1).lower) /. ycms)
  done ;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen xes) - 1 do
    if i = (arrlen xes) - 1 then
      add @@ va "%.2f, %.2f}\n" xes.(i) bnd.(0).upper
    else
      add @@ va "%.2f, " xes.(i)
  done ;
  add @@ va "  \\draw[shift={(\\pos*%.2f-%.2f*%.2f,-0.2)}] (0pt,2pt) -- (0pt,-2pt) node[below] {\\small{\\pos}}; \n\n" xpart bnd.(0).lower xpart;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen yes) - 1 do 
    if i = (arrlen yes) - 1 then
      add @@ va "%.2f, %.2f}\n" yes.(i) bnd.(1).upper
    else
      add @@ va "%.2f, " yes.(i)
  done ;
  add @@ va "  \\draw[shift={(-0.2,\\pos*%.3f-%.3f*%.3f)}] (2pt,0pt) -- (-2pt,0pt) node[left] {\\small{\\pos}}; \n\n" ypart bnd.(1).lower ypart;
  
  if discrete then 
    begin
      if both then
        begin
          for i = 0 to arrlen cont - 2 do
            let rx = cont.(i).(0) *. xpart in
            let ry = cont.(i).(1) *. ypart in
            let rx' = cont.(i+1).(0) *. xpart in
            let ry' = cont.(i+1).(1) *. ypart in
            add @@ va "\\draw[-,blue] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" rx ry rx' ry' ;
          done
        end;
      for i = 0 to arrlen dis - 2 do
        let rx = dis.(i).(0) *. xpart in
        let ry = dis.(i).(1) *. ypart in
        let rx' = dis.(i+1).(0) *. xpart in
        let ry' = dis.(i+1).(1) *. ypart in
        add @@ va "\\draw[-,red] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" rx ry rx' ry' ;
      done
    end 
  else 
    begin
      if both then
        begin
          for i = 0 to arrlen dis - 2 do
            let rx = dis.(i).(0) *. xpart in
            let ry = dis.(i).(1) *. ypart in
            let rx' = dis.(i+1).(0) *. xpart in
            let ry' = dis.(i+1).(1) *. ypart in
            add @@ va "\\draw[-,red] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" rx ry rx' ry' ;
          done
        end;
      for i = 0 to arrlen cont - 2 do
        let rx = cont.(i).(0) *. xpart in
        let ry = cont.(i).(1) *. ypart in
        let rx' = cont.(i+1).(0) *. xpart in
        let ry' = cont.(i+1).(1) *. ypart in
        add @@ va "\\draw[-,blue] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" rx ry rx' ry' ;
      done
    end;
  add @@ va "\\end{tikzpicture}" ;
  let file = open_out "./plot.tikz" in
  output_string file (Buffer.contents buff) ;
  close_out file
  
let plot_2d_dots 
(s:symbolic) 
(xpart:float) 
(ypart:float) 
(lblidx:int list) 
(mode:bool) 
(mcolors:string array) 
(scolors:string array) 
(common:bool) 
(curr_mode:int)=
  let eta = s.lat.eta in
  let dim = s.lat.dim in 
  let finer = float s.lat.scales in
  let bnd = simple_space s.lat.safe in
  let xcms = (bnd.(0).upper -. bnd.(0).lower) *. xpart in
  let ycms = (bnd.(1).upper -. bnd.(1).lower) *. ypart in
  let xes = Array.make ((int_of_float xcms) + 1) 0.0 in
  let yes = Array.make ((int_of_float ycms) + 1) 0.0 in
  let fraction = ref (((2.0 ** (1.0 -. finer)) *. eta) /. sqrt (float dim)) in
  let hcolors = H.create 10 in
  let buff = Buffer.create 16 in
  let add = Buffer.add_string buff in
  if s.lat.scales = 0 then fraction := ((2.0 *. eta) /. sqrt (float dim)) ;
  (match mode with
    | false ->
      let shift = ref 0 in
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do  
          H.add hcolors j scolors.(i)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done 
    | true ->
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do
          H.add hcolors j mcolors.(j- !shift)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done);
  add @@ va "\\input rgb.tex \n\n" ;
  add @@ va "\\begin{tikzpicture}[x=%.3fcm/%.3f,y=%.3fcm/%.3f] \n\n" xcms xcms ycms ycms ; (* To fill by the user *)
  add @@ va "\\draw[step=1cm,gray!30,very thin] (-0.2,-0.2) grid (%.3f,%.3f); \n\n" (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (%.3f,-0.2) ; \n" (xcms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (-0.2,%.3f) ; \n" (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,%.3f) -- (%.3f,%.3f) ;\n" (ycms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (%.3f,-0.2) -- (%.3f,%.3f) ;\n\n" (xcms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  for i = 0 to (int_of_float xcms)  do
    xes.(i) <- bnd.(0).lower +. float i *. ((bnd.(0).upper -. bnd.(0).lower) /. xcms)
  done;
  for i = 0 to (int_of_float ycms)  do
    yes.(i) <- bnd.(1).lower +. float i *. ((bnd.(1).upper -. bnd.(1).lower) /. ycms)
  done ;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen xes) - 1 do
    if i = (arrlen xes) - 1 then
      add @@ va "%.2f, %.2f}\n" xes.(i) bnd.(0).upper
    else
      add @@ va "%.2f, " xes.(i)
  done ;
  add @@ va "  \\draw[shift={(\\pos*%.2f-%.2f*%.2f,-0.2)}] (0pt,2pt) -- (0pt,-2pt) node[below] {\\small{\\pos}}; \n\n" xpart bnd.(0).lower xpart;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen yes) - 1 do
    if i = (arrlen yes) - 1 then add @@ va "%.2f, %.2f}\n" yes.(i) bnd.(1).upper
    else add @@ va "%.2f, " yes.(i)
  done ;
  add @@ va "  \\draw[shift={(-0.2,\\pos*%.3f-%.3f*%.3f)}] (2pt,0pt) -- (-2pt,0pt) node[left] {\\small{\\pos}}; \n\n" ypart bnd.(1).lower ypart;
  add @@ va "\\begin{scope}[fill opacity=0.8]\n";
  let explored = H.create 0 in
  let get_label_m ss lb dd =
    if common then 
      begin
        let x = ss.(0) in
        let y = ss.(1) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        if List.exists (fun xx -> xx = lb) lblidx then add @@ va "\\fill[%s] (%.3f,%.3f) circle (0.8pt);\n\n" (H.find hcolors lb) rx ry 
      end
    else  
      begin
        if curr_mode = get_curr_mode ss then
          begin
            let pss = get_pure ss in
            let x = pss.(0) in
            let y = pss.(1) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            if List.exists (fun xx -> xx = lb) lblidx then add @@ va "\\fill[%s] (%.3f,%.3f) circle (0.8pt);\n\n" (H.find hcolors lb) rx ry 
          end 
      end
  in
  let get_label_s s lb dd =
    if common then 
      begin
        let x = s.(0) in
        let y = s.(1) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        if List.exists (fun xx -> xx = lb) lblidx then (if not (H.mem explored s) then (add @@ va "\\fill[%s] (%.3f,%.3f) circle (0.8pt);\n\n" (H.find hcolors lb) rx ry; H.add explored s (Obj.magic ())))
      end
    else 
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let x = ps.(0) in
            let y = ps.(1) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            if List.exists (fun xx -> xx = lb) lblidx then (if not (H.mem explored s) then (add @@ va "\\fill[%s] (%.3f,%.3f) circle (0.8pt);\n\n" (H.find hcolors lb) rx ry; H.add explored s (Obj.magic ())))
          end 
      end 
  in
  (match mode with
    | true -> Lts.write_transition s.lts get_label_m
    | false -> Lts.write_transition s.lts get_label_s) ;
  add @@ va "\\end{scope}";
  add @@ va "\\end{tikzpicture}" ;
  let file = open_out "./plot.tikz" in
  output_string file (Buffer.contents buff) ;
  close_out file


let plot_2d_arrows (s:symbolic) (xpart:float) (ypart:float) (mode:bool) (mcolors:string array) (scolors:string array) (common:bool) (curr_mode:int) =
  let eta = s.lat.eta in
  let dim = s.lat.dim in
  let finer = float s.finer in
  let bnd = simple_space s.lat.safe in
  let xcms = (bnd.(0).upper -. bnd.(0).lower) *. xpart in
  let ycms = (bnd.(1).upper -. bnd.(1).lower) *. ypart in
  let xes = Array.make ((int_of_float xcms) + 1) 0.0 in
  let yes = Array.make ((int_of_float ycms) + 1) 0.0 in
  let fraction = ref (((2.0 ** (1.0 -. finer)) *. eta) /. sqrt (float dim)) in
  let hcolors = H.create 10 in
  let buff = Buffer.create 16 in
  let add = Buffer.add_string buff in
  if s.finer = 0 then fraction := ((2.0 *. eta) /. sqrt (float dim)) ;
  (match mode with
    | false ->
      let shift = ref 0 in
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do
          H.add hcolors j scolors.(i)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done
    | true ->
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do
          H.add hcolors j mcolors.(j- !shift)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done);
  add @@ va "\\input rgb.tex \n\n" ;
  add @@ va "\\begin{tikzpicture}[x=%.3fcm/%.3f,y=%.3fcm/%.3f] \n\n" xcms xcms ycms ycms ; (* To fill by the user *)
  add @@ va "\\draw[step=1cm,gray!30,very thin] (-0.2,-0.2) grid (%.3f,%.3f); \n\n" (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (%.3f,-0.2) ; \n" (xcms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (-0.2,%.3f) ; \n" (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,%.3f) -- (%.3f,%.3f) ;\n" (ycms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (%.3f,-0.2) -- (%.3f,%.3f) ;\n\n" (xcms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  for i = 0 to (int_of_float xcms)  do
    xes.(i) <- bnd.(0).lower +. float i *. ((bnd.(0).upper -. bnd.(0).lower) /. xcms)
  done;
  for i = 0 to (int_of_float ycms)  do
    yes.(i) <- bnd.(1).lower +. float i *. ((bnd.(1).upper -. bnd.(1).lower) /. ycms)
  done ;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen xes) - 1 do
    if i = (arrlen xes) - 1 then add @@ va "%.2f, %.2f}\n" xes.(i) bnd.(0).upper
    else add @@ va "%.2f, " xes.(i)
  done ;
  add @@ va "  \\draw[shift={(\\pos*%.2f-%.2f*%.2f,-0.2)}] (0pt,2pt) -- (0pt,-2pt) node[below] {\\small{\\pos}}; \n\n" xpart bnd.(0).lower xpart;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen yes) - 1 do
    if i = (arrlen yes) - 1 then add @@ va "%.3f, %.3f}\n" yes.(i) bnd.(1).upper 
    else add @@ va "%.3f, " yes.(i)
  done ;
  add @@ va "  \\draw[shift={(-0.2,\\pos*%.3f-%.3f*%.3f)}] (2pt,0pt) -- (-2pt,0pt) node[left] {\\small{\\pos}}; \n\n"
  ypart bnd.(1).lower ypart;
  add @@ va "\\begin{scope}[fill opacity=0.6]";
  let get_label s lb d =
    if common then 
      begin
        let x = s.(0) in
        let y = s.(1) in
        let x' = d.(0) in
        let y' = d.(1) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        let rx' = (float x' *. !fraction) *. xpart in
        let ry'= (float y' *. !fraction)  *. ypart in
        add @@ va "\\draw[-stealth',%s] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" (H.find hcolors lb) rx ry rx' ry' ;
        add @@ va "\\fill[%s] (%.3f,%.3f) circle (1pt);\n\n" (H.find hcolors lb) rx ry
      end
    else 
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let pd = get_pure d in
            let x = ps.(0) in
            let y = ps.(1) in
            let x' = pd.(0) in
            let y' = pd.(1) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            let rx' = (float x' *. !fraction) *. xpart in
            let ry'= (float y' *. !fraction)  *. ypart in
            add @@ va "\\draw[-stealth',%s] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" (H.find hcolors lb) rx ry rx' ry' ;
            add @@ va "\\fill[%s] (%.3f,%.3f) circle (1pt);\n\n" (H.find hcolors lb) rx ry
          end 
      end
  in
  Lts.write_transition s.lts get_label;
  add @@ va "\\end{scope}";
  add @@ va "\\end{tikzpicture}" ;
  let file = open_out "./plot.tikz" in
  output_string file (Buffer.contents buff) ;
  close_out file



let plot_2d_arrows_v2 (s:symbolic) (xpart:float) (ypart:float) (common:bool) (curr_mode:int) =
  let eta = s.lat.eta in
  let dim = s.lat.dim in
  let finer = float s.finer in
  let bnd = simple_space s.lat.safe in
  let xcms = (bnd.(0).upper -. bnd.(0).lower) *. xpart in
  let ycms = (bnd.(1).upper -. bnd.(1).lower) *. ypart in
  let xes = Array.make ((int_of_float xcms) + 1) 0.0 in
  let yes = Array.make ((int_of_float ycms) + 1) 0.0 in
  let fraction = ref (((2.0 ** (1.0 -. finer)) *. eta) /. sqrt (float dim)) in
  let hcolors = H.create 10 in
  let buff = Buffer.create 16 in
  let add = Buffer.add_string buff in
  if s.finer = 0 then fraction := ((2.0 *. eta) /. sqrt (float dim)) ;
  (
   if s.finer = 2 then
    (H.add hcolors 5 "green" ; H.add hcolors 4 "orange" ; H.add hcolors 3 "blue" ; H.add hcolors 2 "brown");
   if s.finer = 1 then
    (H.add hcolors 3 "blue" ; H.add hcolors 2 "brown" );
   H.add hcolors 1 "black" ; H.add hcolors 0 "red" ;
  );
  add @@ va "\\begin{tikzpicture}[x=%.3fcm/%.3f,y=%.3fcm/%.3f] \n\n" xcms xcms ycms ycms ; (* To fill by the user *)
  add @@ va "\\draw[step=1cm,gray!30,very thin] (-0.2,-0.2) grid (%.3f,%.3f); \n\n" (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (%.3f,-0.2) ; \n" (xcms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (-0.2,%.3f) ; \n" (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,%.3f) -- (%.3f,%.3f) ;\n" (ycms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (%.3f,-0.2) -- (%.3f,%.3f) ;\n\n" (xcms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  for i = 0 to (int_of_float xcms)  do
    xes.(i) <- bnd.(0).lower +. float i *. ((bnd.(0).upper -. bnd.(0).lower) /. xcms)
  done;
  for i = 0 to (int_of_float ycms)  do
    yes.(i) <- bnd.(1).lower +. float i *. ((bnd.(1).upper -. bnd.(1).lower) /. ycms)
  done ;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen xes) - 1 do
    if i = (arrlen xes) - 1 then add @@ va "%.2f, %.2f}\n" xes.(i) bnd.(0).upper 
    else add @@ va "%.2f, " xes.(i)
  done ;
  add @@ va "  \\draw[shift={(\\pos*%.2f-%.2f*%.2f,-0.2)}] (0pt,2pt) -- (0pt,-2pt) node[below] {\\small{\\pos}}; \n\n" xpart bnd.(0).lower xpart;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen yes) - 1 do
    if i = (arrlen yes) - 1 then
      add @@ va "%.3f, %.3f}\n" yes.(i) bnd.(1).upper
    else
      add @@ va "%.3f, " yes.(i)
  done ;
  add @@ va "  \\draw[shift={(-0.2,\\pos*%.3f-%.3f*%.3f)}] (2pt,0pt) -- (-2pt,0pt) node[left] {\\small{\\pos}}; \n\n" ypart bnd.(1).lower ypart;
  add @@ va "\\begin{scope}[fill opacity=0.6]";
  let get_label s lb d =
    if common then 
      begin
        let x = s.(0) in
        let y = s.(1) in
        let x' = d.(0) in
        let y' = d.(1) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        let rx' = (float x' *. !fraction) *. xpart in
        let ry'= (float y' *. !fraction)  *. ypart in 
        add @@ va "\\draw[-stealth',%s,line width = 1pt] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" (H.find hcolors lb) rx ry rx' ry' ;
        add @@ va "\\fill[%s] (%.3f,%.3f) circle (2pt);\n\n" (H.find hcolors lb) rx ry
      end
    else 
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let pd = get_pure d in
            let x = ps.(0) in
            let y = ps.(1) in
            let x' = pd.(0) in
            let y' = pd.(1) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            let rx' = (float x' *. !fraction) *. xpart in
            let ry'= (float y' *. !fraction)  *. ypart in 
            add @@ va "\\draw[-stealth',%s,line width = 1pt] (%.3f,%.3f) -- (%.3f,%.3f) ;\n\n" (H.find hcolors lb) rx ry rx' ry' ;
            add @@ va "\\fill[%s] (%.3f,%.3f) circle (2pt);\n\n" (H.find hcolors lb) rx ry
          end 
      end
  in
  Lts.write_transition s.lts get_label  ;
  add @@ va "\\end{scope}";
  add @@ va "\\end{tikzpicture}" ;
  let file = open_out "./plot.tikz" in 
  output_string file (Buffer.contents buff) ;
  close_out file
    

let plot_2dof3d_dots 
(s:symbolic) 
(c1:int) 
(c2:int) 
(rc3max:float) 
(xpart:float) 
(ypart:float) 
(lblidx:int list) 
(mode:bool) 
(mcolors:string array) 
(scolors:string array) 
(common:bool) 
(curr_mode:int) =
  let nmds = Lts.number_of_modes s.lts in 
  let eta = s.lat.eta in
  let c3 = (aol (lists_diff [0;1;2] [c1;c2])).(0) in  
  let finer = float s.lat.scales in
  let dim = s.lat.dim in
  let bnd = simple_space s.lat.safe in
  let c3max = ((nearest_ki s.lat rc3max 0) - (lower_ki s.lat bnd.(c3) 0)) * truncate (2.0 ** (finer)) in
  let xcms = (bnd.(c1).upper -. bnd.(c1).lower) *. xpart in
  let ycms = (bnd.(c2).upper -. bnd.(c2).lower) *. ypart in
  let xes = Array.make ((int_of_float xcms) + 1) 0.0 in
  let yes = Array.make ((int_of_float ycms) + 1) 0.0 in
  let explored = H.create 0 in
  let fraction = ref (((2.0 ** (1.0 -. finer)) *. eta) /. sqrt (float dim)) in
  let hcolors = H.create 10 in
  let buff = Buffer.create 16 in
  let add = Buffer.add_string buff in
  if s.lat.scales = 0 then fraction := ((2.0 *. eta) /. sqrt (float dim)) ;
  (match mode with
    | false ->
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do  
          H.add hcolors j scolors.(i)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done 
    | true ->
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do  
          H.add hcolors j mcolors.(j- !shift)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done);
  add @@ va "\\input rgb.tex \n\n" ;
  add @@ va "\\begin{tikzpicture}[x=%.3fcm/%.3f,y=%.3fcm/%.3f] \n\n" xcms xcms ycms ycms ; (* To fill by the user *)
  add @@ va "\\draw[step=1cm,gray!30,very thin] (-0.2,-0.2) grid (%.3f,%.3f); \n\n" (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (%.3f,-0.2) ; \n" (xcms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (-0.2,%.3f) ; \n" (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,%.3f) -- (%.3f,%.3f) ;\n" (ycms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (%.3f,-0.2) -- (%.3f,%.3f) ;\n\n" (xcms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\node at (-0.7,20.5) {$t_2$} ;\n" ;
  add @@ va "\\node at (10.1,-1) {$t_1$} ;\n\n" ;
  for i = 0 to (int_of_float xcms)  do
    xes.(i) <- bnd.(c1).lower +. float i *. ((bnd.(c1).upper -. bnd.(c1).lower) /. xcms)
  done;
  for i = 0 to (int_of_float ycms)  do
    yes.(i) <- bnd.(c2).lower +. float i *. ((bnd.(c2).upper -. bnd.(c2).lower) /. ycms)
  done ;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen xes) - 1 do
    if i = (arrlen xes) - 1 then add @@ va "%.2f, %.2f}\n" xes.(i) bnd.(c1).upper
    else add @@ va "%.2f, " xes.(i)
  done ;
  add @@ va "  \\draw[shift={(\\pos*%.2f-%.2f*%.2f,-0.2)}] (0pt,2pt) -- (0pt,-2pt) node[below] {\\small{\\pos}}; \n\n" xpart bnd.(c1).lower xpart;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen yes) - 1 do
    if i = (arrlen yes) - 1 then
      add @@ va "%.3f, %.3f}\n" yes.(i) bnd.(c2).upper
    else
      add @@ va "%.3f, " yes.(i)
  done ;
  add @@ va "  \\draw[shift={(-0.2,\\pos*%.3f-%.3f*%.3f)}] (2pt,0pt) -- (-2pt,0pt) node[left] {\\small{\\pos}}; \n\n" ypart bnd.(c2).lower ypart;
  add @@ va "\\begin{scope}[fill opacity=0.5]";
  let get_label_m s lb dd =
    if common then 
      begin
        let x = s.(c1) in
        let y = s.(c2) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        if List.exists (fun x -> x = lb) lblidx && (s.(c3) = c3max) then
          begin
            if not (H.mem explored [|s.(c1);s.(c2)|]) then
              (add @@ va "\\fill[%s] (%.3f,%.3f) circle (3pt);\n\n" (H.find hcolors lb) rx ry; H.add explored [|s.(c1);s.(c2)|] [mode_of_label lb nmds])
            else
              begin 
                let elabels = (H.find explored [|s.(c1);s.(c2)|]) in
                if not (List.exists (fun x -> x = mode_of_label lb nmds) elabels) then
                  (add @@ va "\\fill[%s] (%.3f,%.3f) circle (3pt);\n\n" (H.find hcolors lb) rx ry ;
                   H.replace explored [|s.(c1);s.(c2)|] (List.append (try H.find explored [|s.(c1);s.(c2)|] with Not_found -> []) [mode_of_label lb nmds]))
              end
          end
      end
    else 
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let x = ps.(c1) in
            let y = ps.(c2) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            if List.exists (fun x -> x = lb) lblidx && (ps.(c3) = c3max) then
              begin
                if not (H.mem explored [|s.(c1+1);s.(c2+1)|]) then
                  (add @@ va "\\fill[%s] (%.3f,%.3f) circle (3pt);\n\n" (H.find hcolors lb) rx ry; 
                   H.add explored [|s.(c1+1);s.(c2+1)|] [mode_of_label lb nmds])
                else
                  begin 
                    let elabels = (H.find explored [|s.(c1+1);s.(c2+1)|]) in
                    if not (List.exists (fun x -> x = mode_of_label lb nmds) elabels) then
                      (add @@ va "\\fill[%s] (%.3f,%.3f) circle (3pt);\n\n" (H.find hcolors lb) rx ry ;
                       H.replace explored [|s.(c1+1);s.(c2+1)|] (List.append (try H.find explored [|s.(c1+1);s.(c2+1)|] with Not_found -> []) [mode_of_label lb nmds]))
                  end
              end
          end
      end
  in
  let get_label_s s lb dd =
    if common then 
      begin
        let x = s.(c1) in
        let y = s.(c2) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        if List.exists (fun x -> x = mode_of_label lb nmds) lblidx && (s.(c3) = c3max) then
          begin
            if H.mem explored [|s.(c1);s.(c2)|] = false then
              (add @@ va "\\fill[%s] (%.3f,%.3f) circle (2.5pt);\n\n" (H.find hcolors lb) rx ry ;
               H.add explored [|s.(c1);s.(c2)|] [mode_of_label lb nmds] ;)
          end 
      end
    else 
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let x = ps.(c1) in
            let y = ps.(c2) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            if List.exists (fun x -> x = mode_of_label lb nmds) lblidx && (ps.(c3) = c3max) then
              begin
                if H.mem explored [|s.(c1+1);s.(c2+1)|] = false then
                  (add @@ va "\\fill[%s] (%.3f,%.3f) circle (2.5pt);\n\n" (H.find hcolors lb) rx ry ;
                   H.add explored [|s.(c1+1);s.(c2+1)|] [mode_of_label lb nmds] ;)
              end 
          end
      end
  in
  (match mode with
    | true ->  Lts.write_transition s.lts get_label_m
    | false -> Lts.write_transition s.lts get_label_s) ;
  add @@ va "\\end{scope}";
  add @@ va "\\end{tikzpicture}" ;
  let file = open_out "./plot.tikz" in
  output_string file (Buffer.contents buff) ;
  close_out file

let plot_2dof4d_dots 
(s:symbolic) 
(c1:int) 
(c2:int) 
(rc3max:float) 
(rc4max:float) 
(xpart:float) 
(ypart:float) 
(lblidx:int list)
(mode:bool) 
(mcolors:string array) 
(scolors:string array) 
(common:bool) 
(curr_mode:int) =
  let nmds = Lts.number_of_modes s.lts in 
  let eta = s.lat.eta in
  let c34 = (aol (lists_diff [0;1;2;3] [c1;c2])) in
  let c3 = c34.(0) in
  let c4 = c34.(1) in
  let finer = float s.lat.scales in
  let dim = s.lat.dim in
  let bnd = simple_space s.lat.safe in
  let c3max = ((nearest_ki s.lat rc3max 0) - (lower_ki s.lat bnd.(c3) 0)) * truncate (2.0 ** (finer)) in
  let c4max = ((nearest_ki s.lat rc4max 0) - (lower_ki s.lat bnd.(c4) 0)) * truncate (2.0 ** (finer)) in
  let xcms = (bnd.(c1).upper -. bnd.(c1).lower) *. xpart in
  let ycms = (bnd.(c2).upper -. bnd.(c2).lower) *. ypart in
  let xes = Array.make ((int_of_float xcms) + 1) 0.0 in
  let yes = Array.make ((int_of_float ycms) + 1) 0.0 in
  let explored = H.create 0 in
  let fraction = ref (((2.0 ** (1.0 -. finer)) *. eta) /. sqrt (float dim)) in
  let hcolors = H.create 10 in
  let buff = Buffer.create 16 in
  let add = Buffer.add_string buff in
  if s.lat.scales = 0 then fraction := ((2.0 *. eta) /. sqrt (float dim)) ;
  (match mode with
    | false ->
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do
          H.add hcolors j scolors.(i)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done 
    | true -> 
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do
          H.add hcolors j mcolors.(j- !shift)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done );
  add @@ va "\\input rgb.tex \n\n" ;
  add @@ va "\\begin{tikzpicture}[x=%.3fcm/%.3f,y=%.3fcm/%.3f] \n\n" xcms xcms ycms ycms ; (* To fill by the user *)
  add @@ va "\\draw[step=1cm,gray!30,very thin] (-0.2,-0.2) grid (%.3f,%.3f); \n\n" (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (%.3f,-0.2) ; \n" (xcms +. 0.2);
  add @@ va "\\draw[-] (-0.2,-0.2) -- (-0.2,%.3f) ; \n" (ycms +. 0.2);
  add @@ va "\\draw[-] (-0.2,%.3f) -- (%.3f,%.3f) ;\n" (ycms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  add @@ va "\\draw[-] (%.3f,-0.2) -- (%.3f,%.3f) ;\n\n" (xcms +. 0.2) (xcms +. 0.2) (ycms +. 0.2);
  for i = 0 to (int_of_float xcms)  do
    xes.(i) <- bnd.(c1).lower +. float i *. ((bnd.(c1).upper -. bnd.(c1).lower) /. xcms)
  done;
  for i = 0 to (int_of_float ycms)  do
    yes.(i) <- bnd.(c2).lower +. float i *. ((bnd.(c2).upper -. bnd.(c2).lower) /. ycms)
  done ;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen xes) - 1 do
    if i = (arrlen xes) - 1 then
      add @@ va "%.2f, %.2f}\n" xes.(i) bnd.(c1).upper
    else
      add @@ va "%.2f, " xes.(i)
  done ;
  add @@ va "  \\draw[shift={(\\pos*%.2f-%.2f*%.2f,-0.2)}] (0pt,2pt) -- (0pt,-2pt) node[below] {\\small{\\pos}}; \n\n" xpart bnd.(c1).lower xpart;
  add @@ va "\\foreach \\pos in {" ;
  for i = 0 to (arrlen yes) - 1 do
    if i = (arrlen yes) - 1 then
      add @@ va "%.3f, %.3f}\n" yes.(i) bnd.(c2).upper
    else
      add @@ va "%.3f, " yes.(i)
  done ;
  add @@ va "  \\draw[shift={(-0.2,\\pos*%.3f-%.3f*%.3f)}] (2pt,0pt) -- (-2pt,0pt) node[left] {\\small{\\pos}}; \n\n" ypart bnd.(c2).lower ypart;
  add @@ va "\\begin{scope}[fill opacity=0.7]";
  let get_label_m s lb dd =
    if common then 
      begin
        let x = s.(c1) in
        let y = s.(c2) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        if List.exists (fun x -> x = lb) lblidx && (s.(c3) = c3max) && (s.(c4) = c4max)then
          begin
            if H.mem explored [|s.(c1);s.(c2)|] = false then
              (add @@ va "\\fill[%s] (%.3f,%.3f) circle (5pt);\n\n" (H.find hcolors lb) rx ry ;
               H.add explored [|s.(c1);s.(c2)|] [mode_of_label lb nmds])
            else
              (let elabels = (H.find explored [|s.(c1);s.(c2)|]) in
               if List.exists (fun x -> x = mode_of_label lb nmds) elabels = false then
                 (add @@ va "\\fill[%s] (%.3f,%.3f) circle (5pt);\n\n" (H.find hcolors lb) rx ry ;
                  H.replace explored [|s.(c1);s.(c2)|] (List.append (try H.find explored [|s.(c1);s.(c2)|] with Not_found -> []) [mode_of_label lb nmds])))
          end
      end
    else
      begin
        if get_curr_mode s = curr_mode then
          begin
            let ps = get_pure s in
            let x = ps.(c1) in
            let y = ps.(c2) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            if List.exists (fun x -> x = lb) lblidx && (ps.(c3) = c3max) && (ps.(c4) = c4max)then
              begin
                if H.mem explored [|s.(c1+1);s.(c2+1)|] = false then
                  (add @@ va "\\fill[%s] (%.3f,%.3f) circle (5pt);\n\n" (H.find hcolors lb) rx ry ;
                   H.add explored [|s.(c1+1);s.(c2+1)|] [mode_of_label lb nmds])
                else
                  (let elabels = (H.find explored [|s.(c1+1);s.(c2+1)|]) in
                   if List.exists (fun x -> x = mode_of_label lb nmds) elabels = false then
                     (add @@ va "\\fill[%s] (%.3f,%.3f) circle (5pt);\n\n" (H.find hcolors lb) rx ry ;
                      H.replace explored [|s.(c1+1);s.(c2+1)|] (List.append (try H.find explored [|s.(c1+1);s.(c2+1)|] with Not_found -> []) [mode_of_label lb nmds])))
              end
          end 
      end 
  in
  let get_label_s s lb dd =
    if common then 
      begin
        let x = s.(c1) in
        let y = s.(c2) in
        let rx = (float x *. !fraction) *. xpart in
        let ry = (float y *. !fraction) *. ypart in
        if List.exists (fun x -> x = lb) lblidx && (s.(c3) = c3max) && (s.(c4) = c4max)then
          begin
            if H.mem explored [|s.(c1);s.(c2)|] = false then
              (add @@ va "\\fill[%s] (%.3f,%.3f) circle (5pt);\n\n" (H.find hcolors lb) rx ry ;
               H.add explored [|s.(c1);s.(c2)|] [mode_of_label lb nmds])
          end
      end
    else
      begin
        if curr_mode = get_curr_mode s then
          begin
            let ps  = get_pure s in 
            let x = ps.(c1) in
            let y = ps.(c2) in
            let rx = (float x *. !fraction) *. xpart in
            let ry = (float y *. !fraction) *. ypart in
            if List.exists (fun x -> x = lb) lblidx && (ps.(c3) = c3max) && (ps.(c4) = c4max)then
              begin
                if H.mem explored [|s.(c1+1);s.(c2+1)|] = false then
                  (add @@ va "\\fill[%s] (%.3f,%.3f) circle (5pt);\n\n" (H.find hcolors lb) rx ry ;
                   H.add explored [|s.(c1+1);s.(c2+1)|] [mode_of_label lb nmds])
              end
          end
      end
  in
  (match mode with
    | true -> Lts.write_transition s.lts get_label_m
    | false -> Lts.write_transition s.lts get_label_s) ;
  add @@ va "\\end{scope}";
  add @@ va "\\end{tikzpicture}" ;
  let file = open_out "./plot.tikz" in
  output_string file (Buffer.contents buff) ;
  close_out file


let plot_3d 
(s:symbolic) 
(lblidx:int list) 
(lx:string) (ly:string) 
(lz:string) 
(v:int) (h:int) 
(mode:bool) 
(mcolors:string array) 
(scolors:string array) 
(common:bool) 
(curr_mode:int) =
  let lbls = Lts.labels s.lts in
  let hcolors = H.create 10 in
  let buff = Buffer.create 16 in
  let buffs = Array.init (lbls + 1) (fun i -> Buffer.create 16) in
  let add = Buffer.add_string buff in
  (match mode with
    | false -> 
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do  
          H.add hcolors j scolors.(i)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done 
    | true -> 
      let shift = ref 0 in 
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do  
          H.add hcolors j mcolors.(j- !shift)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done 
  );
  for i = 0 to lbls do
    Buffer.add_string buffs.(i) 
     (va "\\addplot3+[mark size=0.7pt,mark options={fill opacity=0.6,fill=%s,draw opacity=0},mark=*] coordinates {\n\n" (H.find hcolors i)) ;
  done ;
  add @@ va "\\input rgb.tex \n\n" ;
  add @@ va "\\begin{tikzpicture}\n\n" ;
  add @@ va "\\begin{axis}[3d box,xlabel=$%s$,ylabel=$%s$,zlabel=$%s$,view={%d}{%d}]\n" lx ly lz v h;
  let explored = H.create 0 in
  let get_label_m s lb dd =
    if common then 
      begin
        let x = s.(0) in 
        let y = s.(1) in
        let z = s.(2) in
        if List.exists (fun x -> x = lb) lblidx then Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z) ;
      end 
    else 
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let x = ps.(0) in 
            let y = ps.(1) in
            let z = ps.(2) in
            if List.exists (fun x -> x = lb) lblidx then Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z) ;
          end
      end 
  in
  let get_label_s s lb dd =
    if common then 
      begin
        let x = s.(0) in
        let y = s.(1) in
        let z = s.(2) in
        if not (H.mem explored s) then
          (if List.exists (fun x -> x = lb) lblidx then Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z);
           H.add explored s ())
      end
    else
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let x = ps.(0) in
            let y = ps.(1) in
            let z = ps.(2) in
            if not (H.mem explored s) then
              (if List.exists (fun x -> x = lb) lblidx then Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z);
               H.add explored s ())
    
          end 
      end 
  in
  (match mode with
    | true -> Lts.write_transition s.lts get_label_m
    | false -> Lts.write_transition s.lts get_label_s
  );
  for i = 0 to lbls do
    Buffer.add_string buffs.(i) (va "};\n\n") ;
  done ;
  for i = 0 to lbls do
    add @@ va "%s" (Buffer.contents buffs.(i))
  done ; 
  add @@ va "\\end{axis}\n\n" ;
  add @@ va "\\end{tikzpicture}\n\n" ;
  let file = open_out "./plot.tikz" in
  output_string file (Buffer.contents buff) ;
  close_out file




let plot_3dof4d 
(s:symbolic) 
(lblidx:int list) 
(c1:int) 
(c2:int) 
(c3:int) 
(rc4max:float) 
(lx:string) 
(ly:string) 
(lz:string) 
(v:int) 
(h:int) 
(mode:bool) 
(mcolors:string array) 
(scolors:string array) 
(common:bool) 
(curr_mode:int) =
  let lbls = Lts.labels s.lts in
  let c4 = (aol (lists_diff [0;1;2;3] [c1;c2;c3])).(0) in
  let c4max = ((nearest_ki s.lat rc4max 0) - (lower_ki s.lat (simple_space s.lat.safe).(c4) 0)) * truncate (2.0 ** (float s.lat.scales)) in
  let hcolors = H.create 10 in
  let explored = H.create 0 in
  let buff = Buffer.create 16 in
  let buffs = Array.init (lbls + 1) (fun i -> Buffer.create 16) in
  let add = Buffer.add_string buff in
  (match mode with
    | false ->
      let shift = ref 0 in
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do
          H.add hcolors j scolors.(i)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done 
    | true ->
      let shift = ref 0 in
      for i = 0 to s.finer do
        for j = !shift to !shift + Lts.number_of_modes s.lts - 1 do
          H.add hcolors j mcolors.(j- !shift)
        done;
        shift := !shift + Lts.number_of_modes s.lts
      done 
  );
  for i = 0 to lbls do
    Buffer.add_string buffs.(i)
     (va "\\addplot3+[mark size=0.7pt,mark options={fill opacity=0.6,fill=%s,draw opacity=0},mark=*] coordinates {\n\n"
     (H.find hcolors i)) ;
  done ;
  add @@ va "\\input rgb.tex \n\n" ;
  add @@ va "\\begin{tikzpicture}\n\n" ;
  add @@ va "\\begin{axis}[3d box,xlabel=$%s$,ylabel=$%s$,zlabel=$%s$,view={%d}{%d}]\n" lx ly lz v h;
  let get_label s lb dd =
    if common then
      begin
        let x = s.(c1) in
        let y = s.(c2) in
        let z = s.(c3) in
        if List.exists (fun x -> x = lb) lblidx && (s.(c4) = c4max) then
          begin
            if H.mem explored [|s.(c1);s.(c2);s.(c3)|] = false then
              (Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z) ;
               H.add explored [|s.(c1);s.(c2);s.(c3)|] [id])
            else
              (let elabels = (H.find explored [|s.(c1);s.(c2);s.(c3)|]) in
               if List.exists (fun x -> x = id) elabels = false then
                 (Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z)  ;
                  H.replace explored [|s.(c1);s.(c2);s.(c3)|]
                    (List.append (try H.find explored [|s.(c1);s.(c2);s.(c3)|] with Not_found -> []) [id])))
          end
      end 
    else
      begin
        if curr_mode = get_curr_mode s then 
          begin
            let ps = get_pure s in
            let x = ps.(c1) in
            let y = ps.(c2) in
            let z = ps.(c3) in
            if List.exists (fun x -> x = lb) lblidx && (ps.(c4) = c4max) then
              begin
                if H.mem explored [|s.(c1+1);s.(c2+1);s.(c3+1)|] = false then
                  (Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z) ;
                   H.add explored [|s.(c1+1);s.(c2+1);s.(c3+1)|] [id])
                else
                  (let elabels = (H.find explored [|s.(c1+1);s.(c2+1);s.(c3+1)|]) in
                   if List.exists (fun x -> x = id) elabels = false then
                     (Buffer.add_string buffs.(lb) (va "(%d,%d,%d)\n\n" x y z)  ;
                      H.replace explored [|s.(c1);s.(c2);s.(c3)|]
                        (List.append (try H.find explored [|s.(c1);s.(c2);s.(c3)|] with Not_found -> []) [id])))
              end
          end 
      end 
  in Lts.write_transition s.lts get_label ;
  for i = 0 to lbls do
    Buffer.add_string buffs.(i) (va "};\n\n") ;
  done ;
  for i = 0 to lbls do
    add @@ va "%s" (Buffer.contents buffs.(i))
  done ;
  add @@ va "\\end{axis}\n\n" ;
  add @@ va "\\end{tikzpicture}\n\n" ;
  let file = open_out "./plot.tikz" in 
  output_string file (Buffer.contents buff) ;
  close_out file



