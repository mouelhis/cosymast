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

let semantic_analysis_error = 3

(** Reference to the current lexing buffer, used by the parser for error tracking purposes *)
let the_lexbuf = ref (None : Lexing.lexbuf option)


(** Types available to variables of the language *)  
type var_type = 
  | VT_real
  | VT_integer
  | VT_interval of (float * float)
  | VT_realtoreal
  | VT_realtointerval of (float * float)
  | VT_matrix of int
  | VT_vector of int 
 

let is_matrix_type = function VT_matrix _ -> true | _ -> false  
let is_vector_type = function VT_vector _ -> true | _ -> false 
let is_interval_type = function VT_interval (_,_) -> true | _ -> false 


type binary_operator = 
  | BOP_plus | BOP_minus | BOP_times | BOP_divide | BOP_modulus | BOP_power | BOP_logarithm | BOP_equals


type unary_operator = 
  | UOP_plus | UOP_minus 
  | UOP_sine | UOP_cosine | UOP_tangent | UOP_cotangent | UOP_secant | UOP_cosecant 
  | UOP_arcsine | UOP_arccosine | UOP_arctangent | UOP_hypsine | UOP_hypcosine | UOP_hyptangent
  | UOP_squareroot | UOP_natlogarithm | UOP_exponential

(** Type of the parse ast: this is the raw output of the parser *)
type ast = 
  | Root of string * ast * ast * ast * ast * ast * ast * ast * ast * ast list * ast * ast 
  | Dimension of ast
  | Modes of ast list
  | Enum_field of string
  | Constants of ast list 
  | Constants_by_value of ast list * ast
  | Constant of ast * ast 
  | Variables of ast list
  | Variables_by_type of ast list * var_type
  | Variable of ast * var_type
  | Trajectories of ast list 
  | Trajectory of ast * ast list
  | No_coefficients
  | Coefficients of ast list * ast list 
  | Coefficients_by_type of ast list * var_type
  | Coefficient of ast * var_type
  | Coefficient_value of ast * ast
  | Matrix of ast list
  | Vector of ast list
  | Vector_fields of ast list 
  | Vector_field_for of ast * ast
  | Cases_by_spaces of ast list
  | Unique_case of ast list
  | Case_by_space of ast * ast
  | Simplified_vecfield of ast * ast * ast
  | Expanded_vecfields of ast * ast list
  | Expanded_vecfield of ast * ast 
  | Sampling_params of ast list 
  | Time_sampling of ast 
  | Space_sampling of ast
  | Finer of ast 
  | Scale_minimal_dwell of ast
  | Safety of ast * ast * ast  * ast * ast
  | Reachability of ast * ast * ast * ast * ast * ast 
  | Plot_2d of string * string * string list * string list * ast * ast * ast list * ast option 
  | Plot_2d3d of string * string * string list * string list * ast * ast * ast list * ast * ast * ast * ast option
  | Plot_2d4d of string * string * string list * string list * ast * ast * ast list * ast * ast * ast * ast * ast option
  | Plot_arrows of ast * ast * ast option
  | Plot_trajectory of ast list * ast * ast * ast * ast * ast * ast option * bool option
  | Label of ast * ast
  | Space of ast * ast list 
  | SpaceMinusSpace of ast * ast list * ast list 
  | Spacei of var_type
  | Xp of ast 
  | Parenthesed_xp of ast
  | Binary_op of binary_operator * ast * ast
  | Unary_op of unary_operator * ast 
  | Id of string 
  | Natural of int 
  | Integer of int
  | Real of float
  | Bool of bool
  | Time 

(** Number of errors detected after parsing *)
let _errors = ref 0
let _warnings = ref 0

(** Print error or warning message *)
let error loc = incr _errors; epf "ERROR(%s): " loc; epf
let warning loc = incr _warnings; epf "WARNING(%s): " loc; epf


let uw_id = function
  | Id i -> i
  | _ -> failwith "typecheck:(uw_id:bad ast!)"

let uw_enum_field = function
  | Enum_field e -> e
  | _ -> failwith "typecheck:(uw_enum_field:bad ast!)"

let rec uw_id_list = function
  | Id i :: ids_tail -> i :: uw_id_list ids_tail
  | [] -> []
  | _ -> failwith "typecheck:(uw_id_list:bad ast!)"

let uw_real = function
  | Real i -> i
  | _ -> failwith "typecheck:(uw_real:bad ast!)"

let rec uw_real_list = function
  | Real i :: reals_tail -> i :: uw_real_list reals_tail
  | [] -> []
  | _ -> failwith "typecheck:(uw_real_list:bad ast!)"

let uw_nat = function
  | Natural i -> i
  | _ -> failwith "typecheck:(uw_nat:bad ast!)"

let rec uw_nat_list = function
  | Natural i :: nats_tail -> i :: uw_nat_list nats_tail
  | [] -> []
  | _ -> failwith "typecheck:(uw_nat_list:bad ast!)"

let rec uw_enum_fields = function
  | Enum_field e :: efs_tail -> e :: uw_enum_fields efs_tail
  | [] -> []
  | _ -> failwith "typecheck:(uw_enum_fields:bad ast!)"


(** Type-checking functions *)


let get_dimension
(b:Buffer.t)
(t:ast) = 
  begin
    match t with
    | Dimension (Natural dim) ->
      if dim >= 6 then warning "typecheck:get_dimension" "are you really sure that the system dimension is equal to %d?\n" dim
      else if dim < 1 then
        (error "typecheck:get_dimension" "the system dimension have to be grater or equal to 1!\n";
         exit semantic_analysis_error)
      else
                (Buffer.add_string b @@ va "\n\n"; Buffer.add_string b @@ va "let dim = %d \n\n" dim) ; dim
    | _ -> failwith "typecheck(get_dimension:bad ast!)"
  end

  
let get_modes
(b:Buffer.t)
(t:ast) =
  begin
    match t with
    | Modes mds ->
      if mds = [] then
        (error "typecheck:get_modes" "at least one mode have to be defined!\n";
         exit semantic_analysis_error)
      else (Buffer.add_string b @@ va "let mds = %s \n\n" (string_of_string_list_bis (uw_enum_fields mds)); (uw_enum_fields mds))
    | _ -> failwith "typecheck(get_modes:bad ast!)"
  end
  
let check_constants_xps
(t:ast)
(cts:(string,unit) H.t) =
  let rec sub tt =
    (match tt with
      | Time ->
        (error "typecheck:check_coefficients_xps" "the time parameter is not allowed in constants declarations!\n";
         exit semantic_analysis_error)
      | (Natural _ | Real _ | Integer _) -> ()
      | Id id ->
        if not (H.mem cts id) && id <> "pi" then
          (error "typecheck:check_coefficients_xps" "the constant '%s' is not declared before!\n" id;
           exit semantic_analysis_error)
        else ()
      | (Parenthesed_xp xp | Unary_op (_,xp)) -> sub xp
      | Binary_op (_,xp1,xp2) -> sub xp1 ; sub xp2
      | _ -> failwith "typecheck(check_coefficients_xps: bad ast!)")
  in (sub t)
  
let get_xps
(t:ast)
(cts:(string,unit) H.t) =
  let tp = ref false in
  let rec sub tt =
    (match tt with
      | Time -> if !tp = false then tp := true ; "t"
      | Id id -> String.lowercase_ascii id
      | Natural n -> string_of_float (float n)
      | Integer i -> string_of_float (float i)
      | Real r -> string_of_float r
      | Parenthesed_xp xp -> "(" ^ sub xp ^ ")"
      | Unary_op (UOP_minus, xp) ->  "-. (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_plus, xp) ->   (sub xp)
      | Unary_op (UOP_sine, xp) ->  "sin (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_cosine, xp) ->  "cos (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_tangent, xp) ->  "tan (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_cotangent, xp) ->  "(1.0 /.  tan (" ^ (sub xp) ^ "))"
      | Unary_op (UOP_secant, xp) ->  "(1.0 /. cos (" ^ (sub xp) ^ "))"
      | Unary_op (UOP_cosecant, xp) ->  "(1.0 /. sin (" ^ (sub xp) ^ "))"
      | Unary_op (UOP_arcsine, xp) ->  "asin (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_arccosine, xp) ->  "acos (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_arctangent, xp) ->  "atan (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_hypsine, xp) ->  "sinh (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_hypcosine, xp) ->  "cosh (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_hyptangent, xp) ->  "tanh (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_squareroot, xp) ->  "sqrt (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_natlogarithm, xp) ->  "log (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_exponential, xp) ->  "exp (" ^ (sub xp) ^ ")"
      | Binary_op (BOP_plus, xp1, xp2) -> (sub xp1) ^ " +. " ^ (sub xp2)
      | Binary_op (BOP_minus, xp1, xp2) -> (sub xp1) ^ " -. " ^ (sub xp2)
      | Binary_op (BOP_times, xp1, xp2) -> (sub xp1)  ^ " *. " ^ (sub xp2)
      | Binary_op (BOP_divide, xp1, xp2) -> (sub xp1)  ^ " /. " ^ (sub xp2)
      | Binary_op (BOP_modulus, xp1, xp2) ->   "mod_float " ^ (sub xp1) ^ " " ^ (sub xp2)
      | Binary_op (BOP_power, xp1, xp2) -> (sub xp1) ^ " ** " ^ (sub xp2)
      | Binary_op (BOP_logarithm, xp1, xp2) ->  "logB " ^ (sub xp1) ^ " " ^ (sub xp2)
      | _ -> failwith "typecheck(get_coefficients_xps:bad ast!)")
  in (sub t,!tp)

let get_constants
(b:Buffer.t)
(t:ast) =
  let res = H.create 0 in
  begin
    match t with
      | Constants cts ->
        let iter1 lc =
          match lc with
            | Constants_by_value (cs,v) ->
              check_constants_xps v res;
              List.iter (fun c -> Buffer.add_string b @@ va "let %s = %s\n" (uw_id c) (fst (get_xps v res)) ; H.add res (uw_id c) ()) cs
            | _ -> failwith "typecheck(get_constants:bad ast!)"
        in List.iter iter1 cts
      | _ -> failwith "typecheck(get_constants:bad ast!)"
  end;
  Buffer.add_string b @@ va "\n\n";
  res
  
let get_variables
(t:ast)
(cts:(string,unit) H.t) =
  let res = H.create 0 in
  begin
    match t with
      | Variables vars ->
        let iter1 lv =
          (match lv with
            | Variables_by_type (vs,t) ->
              let iter2 v =
                let vv = uw_id v in
                if H.mem cts vv then
                  (error "typecheck:get_variables" "the variable identifier '%s' was declared before as constant!\n" vv ;
                   exit semantic_analysis_error)
                else
                  begin
                    match t with
                      | VT_interval (li,ui) ->
                        if li < ui then H.add res vv t
                        else
                          (error "typecheck:get_variables" "the type of the variable '%s' is not well-defined (%f >= %f)!\n" vv li ui;
                           exit semantic_analysis_error)
                      | VT_realtointerval (li,ui) ->
                        if li < ui then H.add res vv t
                        else
                          (error "typecheck:get_variables" "the type of the variable '%s' is not well-defined (%f >= %f)!\n" vv li ui;
                           exit semantic_analysis_error)
                      | _ -> H.add res vv t
                  end;
              in List.iter iter2 vs
            | _ -> failwith "typecheck(get_variables:bad ast!)")
         in List.iter iter1 vars
      | _ -> failwith "typecheck(get_variables:bad ast!)"
  end;
  res
  
let get_trajectories
(t:ast)
(dim:int)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t) =
  let res = H.create 0 in
  begin
    match t with
      | Trajectories trs ->
        let iter1 lt =
          (match lt with
            | Trajectory (Id tt,elems) ->
              if H.mem cts tt then
                (error "typecheck:get_trajectories" "the trajectory identifier '%s' was declared before as constant!\n" tt ;
                 exit semantic_analysis_error)
              else if H.mem vars tt then
                (error "typecheck:get_trajectories" "the trajectory identifier '%s' was declared before as variable!\n" tt ;
                 exit semantic_analysis_error)
              else if List.length elems <> dim then
                (error "typecheck:get_trajectories" "the dimension of the trajectory '%s' is different from the selected dimension %d!\n" tt dim ;
                 exit semantic_analysis_error)
              else
                begin
                  let vtt = ref [] in
                  let iter2 elemm =
                    match elemm with
                    | Id elem ->
                       (if H.mem cts elem then
                          (error "typecheck:get_trajectories" "the element '%s' of the trajectory '%s' was declared before as constant!\n" elem tt ;
                           exit semantic_analysis_error)
                        else if not (H.mem vars elem) then
                          (error "typecheck:get_trajectories" "the element '%s' of the trajectory '%s' is undefined!\n" elem tt ;
                           exit semantic_analysis_error)
                        else
                          begin
                            let velem = H.find vars elem in
                            match velem with
                              | (VT_real |  VT_interval (_,_)) ->
                                (error "typecheck:get_trajectories" "the element '%s' of the trajectory '%s' is have to be a time-dependent variable!\n" elem tt ;
                                 exit semantic_analysis_error)
                              | _ ->  vtt := List.append !vtt [elem]
                          end)
                      | _ -> failwith "typecheck(get_trajectories:bad ast!)"
                  in List.iter iter2 elems;
                  H.add res tt !vtt
                end
            | _ -> failwith "typecheck(get_trajectories:bad ast!)")
        in List.iter iter1 trs
      | _ -> failwith "typecheck(get_trajectories:bad ast!)"
  end;
  res
  
let check_coefficients_xps
(t:ast)
(cts:(string,unit) H.t) =
  let rec sub tt =
    (match tt with
      | (Time | Natural _ | Real _ | Integer _) -> ()
      | Id id ->
        if not (H.mem cts id) && id <> "pi" then
          (error "typecheck:check_coefficients_xps" "the identifier '%s' have to be a constant!\n" id;
           exit semantic_analysis_error)
        else ()
      | (Parenthesed_xp xp | Unary_op (_,xp)) -> sub xp
      | Binary_op (_,xp1,xp2) -> sub xp1 ; sub xp2
      | _ -> failwith "typecheck(check_coefficients_xps:bad ast!)")
  in (sub t)
  
  
let get_coefficients
(b:Buffer.t)
(t:ast)
(dim:int)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t)
(trajs:(string,string list) H.t) =
  let coefs_types = H.create 0 in
  begin
    match t with
      | Coefficients (tcfs,vcfs) ->
        let iter1 tcf =
          match tcf with
            | Coefficients_by_type (cfs,typ) ->
              (match typ with
                | VT_matrix d ->
                  if d <> dim then
                    (error "typecheck:get_coefficients" "the dimension of matrices must be equal to %d!\n" dim;
                     exit semantic_analysis_error)
                | VT_vector d ->
                  if d <> dim then
                    (error "typecheck:get_coefficients" "the dimension of vectors must be equal to %d!\n" dim;
                     exit semantic_analysis_error)
                | (VT_realtoreal | VT_realtointerval _) -> ()
                | _ -> failwith "typecheck(get_coefficients:bad ast!)");
              let iter12 cf =
                let cff = uw_id cf in
                if H.mem cts cff then
                  (error "typecheck:get_coefficients" "the coefficient identifier '%s' was declared before as constant!\n" cff;
                   exit semantic_analysis_error)
                else if H.mem vars cff then
                  (error "typecheck:get_coefficients" "the coefficient identifier '%s' was declared before as variable!\n" cff;
                  exit semantic_analysis_error)
                else if H.mem trajs cff then
                  (error "typecheck:get_coefficients" "the coefficient identifier '%s' was declared before as trajectory!\n" cff;
                   exit semantic_analysis_error)
                else H.add coefs_types cff typ
              in List.iter iter12 cfs
            | _ -> failwith "typecheck(get_coefficients:bad ast!)"
        in List.iter iter1 tcfs ;
        let iter2 vcf =
          match vcf with
            | Coefficient_value (cf, vcf) ->
              let cff = uw_id cf in
              if H.mem cts cff then
                (error "typecheck:get_coefficients" "the coefficient identifier '%s' was declared before as constant!\n" cff;
                 exit semantic_analysis_error)
              else if H.mem vars cff then
                (error "typecheck:get_coefficients" "the coefficient identifier '%s' was declared before as variable!\n" cff;
                 exit semantic_analysis_error)
              else if H.mem trajs cff then
                (error "typecheck:get_coefficients" "the coefficient identifier '%s' was declared before as trajectory!\n" cff;
                 exit semantic_analysis_error)
              else if not (H.mem coefs_types cff) then
                (error "typecheck:get_coefficients" "the coefficient identifier '%s' is not declared!\n" cff;
                 exit semantic_analysis_error)
              else
                (match vcf with
                  | Matrix elems_mat ->
                    if List.length elems_mat <> dim then
                      (error "typecheck:get_coefficients" "the matrix '%s' has more then %d lines!\n" cff dim;
                       exit semantic_analysis_error)
                    else
                      begin
                        let iter_vectors elem_vec =
                          (match elem_vec with
                            | Vector vec ->
                              if List.length vec <> dim then
                                (error "typecheck:get_coefficients" "one vector of the matrix '%s' has more than %d elements!\n" cff dim;
                                 exit semantic_analysis_error)
                            | _ -> failwith "typecheck(get_coefficients:bad ast!)")
                        in List.iter iter_vectors elems_mat;
                        let lg = List.length elems_mat in
                        let i = ref 0 in
                        let iter_vectors_2 elem_vec =
                          (match elem_vec with
                            | Vector vec ->
                              let j = ref 0 in
                              let iter_elems e =
                                (match e with
                                   | Xp e ->
                                    begin
                                      check_coefficients_xps e cts ;
                                      let se = get_xps e cts in
                                      if snd se then
                                        Buffer.add_string b @@ va "let %s_%d_%d (t:float) = %s\n" (String.lowercase_ascii cff) !i !j (fst se)
                                      else
                                        begin
                                          Buffer.add_string b @@ va "let %s_%d_%d = %s\n" ((String.lowercase_ascii cff) ^ "_val") !i !j (fst se) ;
                                          Buffer.add_string b @@ va "let %s_%d_%d (t:float) = %s_%d_%d\n\n"
                                             (String.lowercase_ascii cff) !i !j ((String.lowercase_ascii cff)^ "_val") !i !j
                                        end
                                    end
                                   | _ -> failwith "typecheck(get_coefficients:bad ast!)");
                                   j := !j + 1
                              in List.iter iter_elems vec ;
                            | _ -> failwith "typecheck(get_coefficients:bad ast!)");
                          i := !i + 1
                        in List.iter iter_vectors_2 elems_mat;
                        let print_vec i =
                          Buffer.add_string b @@ va "[|";
                          for j = 0 to lg -1 do
                            if j < lg - 1 then
                              Buffer.add_string b @@ va "%s_%d_%d ; " (String.lowercase_ascii cff) i j
                            else
                              Buffer.add_string b @@ va "%s_%d_%d" (String.lowercase_ascii cff) i j
                          done;
                          Buffer.add_string b @@ va "|]"
                        in
                        Buffer.add_string b @@ va "let %s = [|" (String.lowercase_ascii cff);
                        for i = 0 to lg -1 do
                          if i < lg - 1 then (print_vec i; Buffer.add_string b @@ va "; ")
                          else print_vec i
                        done;
                        Buffer.add_string b @@ va "|]\n\n";
                      end
                  | Vector elems_vec ->
                    if List.length elems_vec <> dim then
                      (error "typecheck:get_coefficients" "the vector '%s' has more than %d elements!\n" cff dim;
                       exit semantic_analysis_error)
                    else
                      begin
                        let lg = List.length elems_vec in
                        let i = ref 0 in
                        let iter_elems e =
                          (match e with
                            | Xp e ->
                              begin
                                check_coefficients_xps e cts ;
                                let se = get_xps e cts in
                                if snd se then
                                  Buffer.add_string b @@ va "let %s_%d (t:float) = %s\n" (String.lowercase_ascii cff) !i (fst se)
                                else
                                  begin
                                    Buffer.add_string b @@ va "let %s_%d = %s\n" ((String.lowercase_ascii cff) ^ "_val") !i (fst se) ;
                                    Buffer.add_string b @@ va "let %s_%d (t:float) = %s_%d\n\n" (String.lowercase_ascii cff) !i ((String.lowercase_ascii cff)^ "_val") !i
                                  end
                              end ;
                              i := !i + 1
                            | _ -> failwith "typecheck(get_coefficients:bad ast!)")
                        in List.iter iter_elems elems_vec ;
                        Buffer.add_string b @@ va "let %s = [|" (String.lowercase_ascii cff);
                        for j = 0 to lg -1 do
                          if j < lg - 1 then Buffer.add_string b @@ va "%s_%d ; " (String.lowercase_ascii cff) j
                          else Buffer.add_string b @@ va "%s_%d" (String.lowercase_ascii cff) j
                        done;
                        Buffer.add_string b @@ va "|]\n\n";
                      end
                  | Xp e ->
                    begin
                      check_coefficients_xps e cts ;
                      let se = get_xps e cts in
                      if snd se then Buffer.add_string b @@ va "let %s (t:float) = %s\n\n" (String.lowercase_ascii cff) (fst se)
                      else
                        begin
                          Buffer.add_string b @@ va "let %s = %s\n" ((String.lowercase_ascii cff) ^ "_val") (fst se) ;
                          Buffer.add_string b @@ va "let %s (t:float) = %s\n\n" (String.lowercase_ascii cff) ((String.lowercase_ascii cff)^ "_val")
                        end
                    end
                  | _ -> failwith "typecheck(get_coefficients:bad ast!)")
            | _ -> failwith "typecheck(get_coefficients:bad ast!)"
        in List.iter iter2 vcfs ;
      | No_coefficients -> ()
      | _ -> failwith "typecheck(get_coefficients:bad ast!)"
  end ;
  coefs_types
  
let get_spaces
(b:Buffer.t)
(tl:ast list)
(ddim:int)
(mds:string list)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t)
(trajs:(string,string list) H.t)
(coefs: ((string,var_type) H.t)) =
  let spcs = H.create 0 in
  let iter1 (tt:ast) =
    match tt with
      | Space (Id id,bounds) ->
        if H.mem cts id || H.mem vars id || H.mem trajs id || H.mem coefs id then
          (error "typecheck:get_spaces" "the identifier '%s' was used before!\n" id;
           exit semantic_analysis_error)
        else
          begin
            if listlen bounds <> ddim then
              (error "typecheck:get_spaces" "the number of boundaries intervals of the state space defining '%s' must be equal to %d!\n" id ddim;
               exit semantic_analysis_error)
            else
              Buffer.add_string b @@ va "let %s = \n " (String.lowercase_ascii id);
              Buffer.add_string b @@ va " Sp ([|";
              let k = ref 0 in
              let lg = listlen bounds in
              let bnds = ref [||] in
              let iter2 bnd =
                (match bnd with
                  | (Spacei (VT_interval (li,ui))) ->
                    if li < ui then
                      begin
                        if !k < lg - 1 then Buffer.add_string b @@ va "{lower = %.4f ; upper = %.4f}; " li ui
                        else Buffer.add_string b @@ va "{lower = %.4f ; upper = %.4f}|])\n\n" li ui;
                        bnds := Array.append !bnds [|{lower = li ; upper = ui}|]
                      end
                    else
                      (error "typecheck:get_spaces" "the boundaries of the state-space '%s' are not well-defined (%f >= %f)!\n" id li ui;
                       exit semantic_analysis_error)
                  | _ -> failwith "typecheck(get_spaces:bad ast!)");
                  k := !k + 1
              in List.iter iter2 bounds ;
              H.add spcs id (Sp (!bnds))
          end ;
      | SpaceMinusSpace (Id id,bounds1,bounds2) ->
        if H.mem cts id || H.mem vars id || H.mem trajs id || H.mem coefs id then
          (error "typecheck:get_spaces" "the identifier '%s' was used before!\n" id;
           exit semantic_analysis_error)
        else
          begin
            if listlen bounds1 <> ddim || listlen bounds2 <> ddim then
              (error "typecheck:get_spaces" "the number of boundaries intervals of the state spaces defining '%s' must be equal to %d!\n" id ddim;
               exit semantic_analysis_error)
            else
              Buffer.add_string b @@ va "let %s = \n " (String.lowercase_ascii id);
              Buffer.add_string b @@ va " SpMinusSp ([|";
              let k = ref 0 in
              let lg = listlen bounds1 in
              let bnds1 = ref [||] in
              let iter2 bnd =
                (match bnd with
                  | (Spacei (VT_interval (li,ui))) ->
                    if li < ui then
                      begin
                        if !k < lg - 1 then Buffer.add_string b @@ va "{lower = %.4f ; upper = %.4f}; " li ui
                        else Buffer.add_string b @@ va "{lower = %.4f ; upper = %.4f}|], " li ui;
                        bnds1 := Array.append !bnds1 [|{lower = li ; upper = ui}|]
                      end
                    else
                      (error "typecheck:get_spaces" "the boundaries of the state-spaces defining '%s' are not well-defined (%f >= %f)!\n" id li ui;
                       exit semantic_analysis_error)
                  | _ -> failwith "typecheck(get_spaces:bad ast!)");
                  k := !k + 1
              in List.iter iter2 bounds1 ;
              Buffer.add_string b @@ va "[|";
              k := 0;
              let bnds2 = ref [||] in
              let iter3 bnd =
                (match bnd with
                  | (Spacei (VT_interval (li,ui))) ->
                    if li < ui then
                      begin
                        if !bnds1.(!k).lower <= li && !bnds1.(!k).upper >= ui then
                          begin
                            if !k < lg - 1 then Buffer.add_string b @@ va "{lower = %.4f ; upper = %.4f}; " li ui
                            else Buffer.add_string b @@ va "{lower = %.4f ; upper = %.4f}|])\n\n" li ui;
                            bnds2 := Array.append !bnds2 [|{lower = li ; upper = ui}|]
                          end
                        else
                          begin
                            if !bnds1.(!k).lower > li then
                              (error "typecheck:get_spaces" "the state-space to be substracted shall be included in the substractable in '%s' (%f > %f)!\n" id !bnds1.(!k).lower li;
                               exit semantic_analysis_error)
                            else
                              (error "typecheck:get_spaces" "the state-space to be substracted shall be included in the substractable in '%s' (%f < %f)!\n" id !bnds1.(!k).upper ui;
                               exit semantic_analysis_error)
                          end
                      end
                    else
                      (error "typecheck:get_spaces" "the boundaries of the state-spaces to be substracted in '%s' are not well-defined (%f >= %f)!\n" id li ui;
                       exit semantic_analysis_error)
                  | _ -> failwith "typecheck(get_spaces:bad ast!)");
                  k := !k + 1
              in List.iter iter3 bounds2 ;
              H.add spcs id (SpMinusSp (!bnds1,!bnds2))
          end
      | _ -> failwith "typecheck(get_spaces:bad ast!)"
  in List.iter iter1 tl;
  spcs




let get_simplifiedvf_xp
(bb:Buffer.t)
(t:ast)
(iddim:int)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t)
(trajs:(string,string list) H.t)
(coefs:(string,var_type) H.t)
(traj:string) =
  match t with
    | Binary_op (BOP_plus,Binary_op (BOP_times,Id a,Id x), Id b) ->
      if not (H.mem trajs x) then
        (error "typecheck:get_simplifiedvf_xps" "'%s' must be a trajectory identifier!\n" x;
         exit semantic_analysis_error)
      else if List.length (H.find trajs x) <> iddim then
        (error "typecheck:check_simplifiedvf_xps" "the dimension of the used trajectory to must be equal to %d!\n" iddim;
         exit semantic_analysis_error)
      else if not (H.mem coefs a) then
        (error "typecheck:get_simplifiedvf_xps" "'%s' must be a coefficient identifier!\n" a;
         exit semantic_analysis_error)
      else if not (H.mem coefs b) then
        (error "typecheck:get_simplifiedvf_xps" "'%s' must be a coefficient identifier!\n" b;
         exit semantic_analysis_error)
      else
        begin
          (match H.find coefs a with
            | (VT_realtoreal | VT_realtointerval _ | VT_vector _) ->
              (error "typecheck:get_simplifiedvf_xps" "'%s' must be a matrix!\n" a;
               exit semantic_analysis_error)
            | VT_matrix d ->
              if d <> iddim then
                (error "typecheck:get_simplifiedvf_xps" "the dimension of the matrix '%s' must be equal to %d!\n" a iddim;
                 exit semantic_analysis_error)
            | _ -> failwith "typecheck(get_simplifiedvf_xps:bad ast!)");
          (match H.find coefs b with
            | (VT_realtoreal | VT_realtointerval _ | VT_matrix _) ->
              (error "typecheck:get_simplifiedvf_xps" "'%s' must be a vector!\n" a;
               exit semantic_analysis_error)
            | VT_vector d ->
              if d <> iddim then
                (error "typecheck:get_simplifiedvf_xps" "the dimension of the vector '%s' must be equal to %d!\n" a iddim;
                 exit semantic_analysis_error)
            | _ -> failwith "typecheck(get_simplifiedvf_xps:bad ast!)")
        end;
        Buffer.add_string bb @@ va "(vector_float_plus (matrix_vector_float_prod (get_float_matrix_from_tfunmat t %s) %s) (get_float_vector_from_tfunvec t %s))"
        (String.lowercase_ascii a) traj (String.lowercase_ascii b)
    (* other linear system kinds can be added here *)
    | _ -> failwith "typecheck(get_simplifiedvf_xps:bad ast!)"

let check_expandedvf_xps
(t:ast)
(traj:string)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t)
(trajs:(string,string list) H.t)
(coefs: (string,var_type) H.t) =
  let rec sub tt =
    match tt with
      | (Time | Natural _ | Real _ | Integer _) -> ()
      | Id id ->
        if H.mem trajs id then
          (error "typecheck:check_expandedvf_xps" "expanded vector fields must contain only variable, constant, and coefficient identifiers but %s is a trajectory!\n" id;
           exit semantic_analysis_error)
        else if H.mem coefs id then
          begin
            match H.find coefs id with
              | (VT_realtoreal | VT_realtointerval _) -> ()
              | (VT_matrix _ | VT_vector _) ->
                (error "typecheck:check_expandedvf_xps" "expanded vector fields must not contain matrix or vector coefficients!\n";
                 exit semantic_analysis_error)
              | _ -> failwith "typecheck(check_expandedvf_xps:bad ast!)"
          end
        else if H.mem vars id then
          if not (List.exists (fun x -> x = id) (H.find trajs traj)) then
            (error "typecheck:check_expandedvf_xps" "'%s' is not a field of '%s'!\n" id traj;
             exit semantic_analysis_error)
        else ()
      | Unary_op (_,xp) -> sub xp
      | Parenthesed_xp xp -> sub xp
      | Binary_op (_,xp1,xp2) -> sub xp1 ; sub xp2
      | _ -> failwith "typecheck(check_expandedvf_xps:bad ast!)"
  in (sub t)
  
let get_expandedvf_xps
(t:ast)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t)
(trajs:(string,string list) H.t)
(coefs:(string,var_type) H.t)
(traj:string) =
  let rec sub tt =
    (match tt with
      | Time -> "t"
      | Id id ->
        if id = "pi" then "pi"
        else if H.mem cts id then String.lowercase_ascii id
        else if H.mem vars id then
          if List.exists (fun x -> x = id) (H.find trajs traj) then
            begin
              let buff = Buffer.create 0 in
              let iid = lindex id (H.find trajs traj) in
              Buffer.add_string buff @@ va "%s.(%d)" traj iid;
              Buffer.contents buff
            end
          else
            begin
              match H.find vars id with
                | VT_real ->
                  string_of_float (random_float (-.999.0) (999.0))
                | VT_interval (l,u) -> string_of_float (random_float l u)
                | _ -> failwith "typecheck(get_expandedvf_xps: bad variable !)"
            end
        else if H.mem coefs id then
          (match H.find coefs id with
            | (VT_realtoreal | VT_realtointerval _) -> (String.lowercase_ascii id) ^ " t"
            | _ -> failwith "typecheck(get_expandedvf_xps: bad coefficient !)")
        else failwith "typecheck(get_expandedvf_xps: bad identifier !)"
      | Natural n -> string_of_float (float n)
      | Integer i -> string_of_float (float i)
      | Real r -> string_of_float r
      | Parenthesed_xp xp -> "(" ^ sub xp ^ ")"
      | Unary_op (UOP_minus, xp) ->  "-. (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_plus, xp) ->   (sub xp)
      | Unary_op (UOP_sine, xp) ->  "sin (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_cosine, xp) ->  "cos (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_tangent, xp) ->  "tan (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_cotangent, xp) ->  "(1.0 /.  tan (" ^ (sub xp) ^ "))"
      | Unary_op (UOP_secant, xp) ->  "(1.0 /. cos (" ^ (sub xp) ^ "))"
      | Unary_op (UOP_cosecant, xp) ->  "(1.0 /. sin (" ^ (sub xp) ^ "))"
      | Unary_op (UOP_arcsine, xp) ->  "asin (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_arccosine, xp) ->  "acos (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_arctangent, xp) ->  "atan (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_hypsine, xp) ->  "sinh (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_hypcosine, xp) ->  "cosh (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_hyptangent, xp) ->  "tanh (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_squareroot, xp) ->  "sqrt (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_natlogarithm, xp) ->  "log (" ^ (sub xp) ^ ")"
      | Unary_op (UOP_exponential, xp) ->  "exp (" ^ (sub xp) ^ ")"
      | Binary_op (BOP_plus, xp1, xp2) -> (sub xp1) ^ " +. " ^ (sub xp2)
      | Binary_op (BOP_minus, xp1, xp2) -> (sub xp1) ^ " -. " ^ (sub xp2)
      | Binary_op (BOP_times, xp1, xp2) -> (sub xp1)  ^ " *. " ^ (sub xp2)
      | Binary_op (BOP_divide, xp1, xp2) -> (sub xp1)  ^ " /. " ^ (sub xp2)
      | Binary_op (BOP_modulus, xp1, xp2) ->   "mod_float " ^ (sub xp1) ^ " " ^ (sub xp2)
      | Binary_op (BOP_power, xp1, xp2) -> (sub xp1) ^ " ** " ^ (sub xp2)
      | Binary_op (BOP_logarithm, xp1, xp2) ->  "logB " ^ (sub xp1) ^ " " ^ (sub xp2)
      | _ -> failwith "typecheck(get_expandedvf_xps:bad ast!)")
  in sub t
  
let check_cases
(tl:ast list)
(traj:string)
(mds:string list)
(spcs:(string,state_space) H.t) =
  let spsoftraj = H.create 0 in
  let iter1 case =
    match case with
      | Case_by_space (Id spc, Unique_case modes) ->
        if not (H.mem spcs spc) then
          (error "typecheck:check_cases" "the identifier %s was not defined as a space identifier!\n" spc;
           exit semantic_analysis_error)
        else if H.mem spsoftraj spc then
          (error "typecheck:check_cases" "the space identifier %s was used before in the definition of %s!\n" spc traj;
           exit semantic_analysis_error)
        else
          begin
            let spaces = ref [||] in
            (match (H.find spcs spc) with
             | Sp (simple) -> if H.mem spsoftraj spc then spaces := Array.append [|simple|] !spaces
             | SpMinusSp (simple1,simple2) ->
               (error "typecheck:check_cases" "state spaces used in cases shall be simple not composed (%s = <space1> \\ <space2>) !\n" spc;
                exit semantic_analysis_error));
            if not (is_disjoint_from_spaces (simple_space (H.find spcs spc)) !spaces) then
              (error "typecheck:check_cases" "the space %s is not disjoint from the previous spaces used in the definition of '%s'!\n" spc traj;
               exit semantic_analysis_error)
            else
              begin
                H.add spsoftraj spc [];
                let iter2 md =
                  match md with
                    | Simplified_vecfield (Enum_field ef, _, _) | Expanded_vecfields (Enum_field ef, _) ->
                      if not (List.exists (fun x -> x = ef) mds) then
                        (error "typecheck:check_cases" "the mode '%s' does not exist!\n" ef;
                         exit semantic_analysis_error)
                      else if List.exists (fun x -> x = lindex ef mds) (H.find spsoftraj spc) then
                        (error "typecheck:check_cases" "the mode '%s' was used before for '%s' in the definition of the space '%s'!\n" ef traj spc;
                         exit semantic_analysis_error)
                      else H.replace spsoftraj spc ((lindex ef mds) :: (H.find spsoftraj spc))
                    | _  -> failwith "typecheck(check_cases: bad ast!)"
                in List.iter iter2 modes;
              end
          end
      | Unique_case (modes)  ->
        H.add spsoftraj "restsp" [];
        let iter3 md =
          match md with
            | Simplified_vecfield (Enum_field ef, _, _) | Expanded_vecfields (Enum_field ef, _) ->
              if not (List.exists (fun x -> x = ef) mds) then
                (error "typecheck:get_vectorfields" "the mode '%s' does not exist\n" ef;
                 exit semantic_analysis_error)
              else if List.exists (fun x -> x = lindex ef mds) (H.find spsoftraj "restsp") then
                (error "typecheck:get_vectorfields" "the mode '%s' was used before for '%s' in the remaining space!\n" ef traj;
                 exit semantic_analysis_error)
              else H.replace spsoftraj "rest" ((lindex ef mds) :: (H.find spsoftraj "restsp"))
            | _  -> failwith "typecheck(check_cases: bad ast!)"
        in List.iter iter3 modes
      | _ -> failwith "typecheck(check_cases: bad ast!)"
  in List.iter iter1 tl

let get_vectorfields
(b:Buffer.t)
(t:ast)
(ddim:int)
(mds:string list)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t)
(trajs:(string,string list) H.t)
(coefs:(string,var_type) H.t)
(spcs:(string,state_space) H.t) =
  let vecfs = H.create 0 in
  begin
    match t with
      | Vector_fields vfs ->
        let iter1 vf =
          match vf with
            | Vector_field_for (Id traj, definition) ->
              Buffer.add_string b @@ va "(* the vector fields for %s *)\n\n" (String.lowercase_ascii traj);
              if H.mem vecfs traj then
                (error "typecheck:get_vectorfields" "the vector fields for %s was defined before!\n" traj;
                 exit semantic_analysis_error)
              else if not (H.mem trajs traj) then
                (error "typecheck:get_vectorfields" "%s is not a trajctory identifier!\n" traj;
                 exit semantic_analysis_error)
              else
                begin
                  match definition with
                    | Cases_by_spaces (cases) ->
                      let vfofmd = H.create 0 in
                      check_cases cases traj mds spcs;
                      let buffs_by_modes = ref [] in
                      for i = 0 to (List.length mds - 1) do buffs_by_modes := (ref (Buffer.create 0)) :: !buffs_by_modes done;
                      let idx_case = ref (-1) in
                      let iter2 case =
                        idx_case := !idx_case + 1;
                        match case with
                          | Case_by_space (Id spc, Unique_case modes) ->
                            let iter3 md  =
                              match md with
                                | Simplified_vecfield (Enum_field ef, Id id, exp) ->
                                  if String.length id <= 4 then
                                    (error "typecheck:get_vectorfields"
                                           "the length of the trajectory derivate's name identifier must be strictly greater than 4 \n and suffixed by _dot!\n";
                                     exit semantic_analysis_error)
                                  else
                                    begin
                                      let tidl = String.length id in
                                      let dot = String.sub id (tidl - 4) 4 in
                                      if dot <> "_dot" then
                                        (error "typecheck:get_vectorfields" "the trajectory derivate identifier must be suffixed '_dot'!\n";
                                         exit semantic_analysis_error)
                                      else
                                        begin
                                          let tid = String.sub id 0 (tidl - 4) in
                                          if tid <> traj then
                                            (error "typecheck:get_vectorfields" "the trajectory identifier '%s' must be equal to '%s'!\n" tid traj;
                                             exit semantic_analysis_error)
                                          else
                                            begin
                                              let imd = (lindex ef mds) in
                                              let buff_md = List.nth !buffs_by_modes imd in
                                              if Buffer.contents !buff_md = "" then
                                                begin
                                                  Buffer.add_string !buff_md @@ va "let f_%s_%d (t:float) (%s:float_vector) = \n" (String.lowercase_ascii traj) (lindex ef mds) (String.lowercase_ascii traj);
                                                  Buffer.add_string !buff_md @@ va " if in_simple_space %d %s %s then \n  " ddim (String.lowercase_ascii traj) spc
                                                end
                                              else Buffer.add_string !buff_md @@ va " else if in_simple_space %d %s %s then \n  " ddim (String.lowercase_ascii traj) spc;
                                              get_simplifiedvf_xp !buff_md exp (List.length (H.find trajs tid)) cts vars trajs coefs (String.lowercase_ascii traj);
                                              Buffer.add_string !buff_md @@ va "\n"
                                            end
                                        end
                                    end
                                | Expanded_vecfields (Enum_field ef, expds) ->
                                  let iter4 expd =
                                    match expd with
                                      | Expanded_vecfield (Id var, evf) ->
                                        let vecofv = ref [] in
                                        let tvarl = String.length var in
                                        let dot = String.sub var (tvarl - 4) 4 in
                                        if dot <> "_dot" then
                                          (error "typecheck:get_vectorfields" "the vector field derivate identifier must be suffixed by '_dot'!\n";
                                           exit semantic_analysis_error)
                                        else
                                          begin
                                            let tvar = String.sub var 0 (tvarl - 4) in
                                            if not (H.mem vars tvar) then
                                              (error "typecheck:get_vectorfields" "the vector field identifier '%s' is not declared!\n" tvar;
                                               exit semantic_analysis_error)
                                            else if not (List.exists (fun x -> x = tvar) (H.find trajs traj)) then
                                              (error "typecheck:get_vectorfields" "the vector field identifier '%s' is not declared in the trajectory '%s'!\n" tvar traj;
                                               exit semantic_analysis_error)
                                            else if List.exists (fun x -> x = tvar) !vecofv then
                                              (error "typecheck:get_vectorfields" "the vector field identifier '%s' was defined before!\n" tvar;
                                               exit semantic_analysis_error)
                                            else
                                              begin
                                                check_expandedvf_xps evf traj cts vars trajs coefs ;
                                                vecofv := tvar :: !vecofv ;
                                                Buffer.add_string b @@ va "let f_%s_%d_%d_%d (%s:float_vector) (t:float) = \n "
                                                  (String.lowercase_ascii traj) !idx_case (lindex ef mds) (lindex tvar (H.find trajs traj)) (String.lowercase_ascii traj) ;
                                                Buffer.add_string b @@ va " %s\n\n" (get_expandedvf_xps evf cts vars trajs coefs (String.lowercase_ascii traj))
                                              end
                                          end
                                      | _ -> failwith "typecheck(get_vectorfields: bad ast!)"
                                                      in List.iter iter4 expds;
                                  let imd = (lindex ef mds) in
                                  let buff_md = List.nth !buffs_by_modes imd in
                                  if Buffer.contents !buff_md = "" then
                                    begin
                                      Buffer.add_string !buff_md @@ va "let f_%s_%d (t:float) (%s:float_vector) = \n"
                                        (String.lowercase_ascii traj) (lindex ef mds) (String.lowercase_ascii traj);
                                      Buffer.add_string !buff_md @@ va " if in_simple_space %d %s %s then \n  "
                                        ddim (String.lowercase_ascii traj) spc
                                    end
                                  else Buffer.add_string !buff_md @@ va " else if in_simple_space %d %s %s then \n  " ddim (String.lowercase_ascii traj) spc;
                                  Buffer.add_string !buff_md @@ va "get_float_vector_from_tfunvec t [|";
                                  let lg = listlen (H.find trajs traj) in
                                  for i = 0 to lg - 1 do
                                    if i < lg - 1 then
                                      Buffer.add_string !buff_md @@ va "f_%s_%d_%d_%d %s; "
                                        (String.lowercase_ascii traj) !idx_case (lindex ef mds) i (String.lowercase_ascii traj)
                                    else
                                      Buffer.add_string !buff_md @@ va "f_%s_%d_%d_%d %s|]"
                                        (String.lowercase_ascii traj) !idx_case (lindex ef mds) i (String.lowercase_ascii traj)
                                  done;
                                  Buffer.add_string !buff_md @@ va "\n";
                                | _  -> failwith "typecheck(check_cases: bad ast!)"
                            in List.iter iter3 modes;
                          | Unique_case (modes) ->
                            let iter3 md  =
                              match md with
                                | Simplified_vecfield (Enum_field ef, Id id, exp) ->
                                  if String.length id <= 4 then
                                    (error "typecheck:get_vectorfields"
                                           "the length of the trajectory derivate's name identifier must be strictly greater than 4 \n and suffixed by _dot!\n";
                                     exit semantic_analysis_error)
                                  else
                                    begin
                                      let tidl = String.length id in
                                      let dot = String.sub id (tidl - 4) 4 in
                                      if dot <> "_dot" then
                                        (error "typecheck:get_vectorfields" "the trajectory derivate identifier must be suffixed '_dot'!\n";
                                         exit semantic_analysis_error)
                                      else
                                        begin
                                          let tid = String.sub id 0 (tidl - 4) in
                                          if tid <> traj then
                                            (error "typecheck:get_vectorfields" "the trajectory identifier '%s' must be equal to '%s'!\n" tid traj;
                                             exit semantic_analysis_error)
                                          else
                                            begin
                                              let imd = (lindex ef mds) in
                                              let buff_md = List.nth !buffs_by_modes imd in
                                              Buffer.add_string !buff_md @@ va " else \n  ";
                                              get_simplifiedvf_xp !buff_md exp (List.length (H.find trajs tid)) cts vars trajs coefs (String.lowercase_ascii traj);
                                              Buffer.add_string !buff_md @@ va "\n"
                                            end
                                        end
                                    end
                                | Expanded_vecfields (Enum_field ef, expds) ->
                                  let iter4 expd =
                                    match expd with
                                      | Expanded_vecfield (Id var, evf) ->
                                        let vecofv = ref [] in
                                        let tvarl = String.length var in
                                        let dot = String.sub var (tvarl - 4) 4 in
                                        if dot <> "_dot" then
                                          (error "typecheck:get_vectorfields" "the vector field derivate identifier must be suffixed by '_dot'!\n";
                                           exit semantic_analysis_error)
                                        else
                                          begin
                                            let tvar = String.sub var 0 (tvarl - 4) in
                                            if not (H.mem vars tvar) then
                                              (error "typecheck:get_vectorfields" "the vector field identifier '%s' is not declared!\n" tvar;
                                               exit semantic_analysis_error)
                                            else if not (List.exists (fun x -> x = tvar) (H.find trajs traj)) then
                                              (error "typecheck:get_vectorfields" "the vector field identifier '%s' is not declared in the trajectory '%s'!\n" tvar traj;
                                               exit semantic_analysis_error)
                                            else if List.exists (fun x -> x = tvar) !vecofv then
                                              (error "typecheck:get_vectorfields" "the vector field identifier '%s' was defined before!\n" tvar;
                                               exit semantic_analysis_error)
                                            else
                                              begin
                                                check_expandedvf_xps evf traj cts vars trajs coefs ;
                                                vecofv := tvar :: !vecofv ;
                                                Buffer.add_string b @@ va "let f_%s_%d_%d_%d (%s:float_vector) (t:float) = \n "
                                                  (String.lowercase_ascii traj) (List.length cases -1)  (lindex ef mds) (lindex tvar (H.find trajs traj)) (String.lowercase_ascii traj);
                                                Buffer.add_string b @@ va " %s\n\n" (get_expandedvf_xps evf cts vars trajs coefs (String.lowercase_ascii traj))
                                              end
                                          end
                                      | _ -> failwith "typecheck(get_vectorfields: bad ast!)"
                                                      in List.iter iter4 expds;
                                  let imd = (lindex ef mds) in
                                  let buff_md = List.nth !buffs_by_modes imd in
                                  Buffer.add_string !buff_md @@ va " else \n  ";
                                  Buffer.add_string !buff_md @@ va "get_float_vector_from_tfunvec t [|";
                                  let lg = listlen (H.find trajs traj) in
                                  for i = 0 to lg - 1 do
                                    if i < lg - 1 then
                                      Buffer.add_string !buff_md @@ va "f_%s_%d_%d_%d %s; "
                                        (String.lowercase_ascii traj) (List.length cases -1) (lindex ef mds) i (String.lowercase_ascii traj)
                                    else
                                      Buffer.add_string !buff_md @@ va "f_%s_%d_%d_%d %s|]"
                                        (String.lowercase_ascii traj) (List.length cases -1) (lindex ef mds) i (String.lowercase_ascii traj)
                                  done;
                                  Buffer.add_string !buff_md @@ va "\n";
                                  H.add vfofmd (lindex ef mds) ()
                                | _  -> failwith "typecheck(check_cases: bad ast!)"
                            in List.iter iter3 modes;
                          | _  -> failwith "typecheck(check_cases: bad ast!)"
                      in List.iter iter2 cases;
                      let copy (buff_md:(Buffer.t) ref) =
                        Buffer.add_buffer b !buff_md;
                        Buffer.add_string b @@ va "\n"
                      in List.iter copy !buffs_by_modes;
                      Buffer.add_string b @@ va "let vfs_%s () = \n let res = H.create 0 in\n" (String.lowercase_ascii traj);
                      for k = 0 to listlen mds - 1 do Buffer.add_string b @@ va " H.add res %d f_%s_%d;\n" k (String.lowercase_ascii traj) k done;
                      Buffer.add_string b @@ va " res\n\n";
                      H.add vecfs traj vfofmd
                    | Unique_case (modes) ->
                      let vfofmd = H.create 0 in
                      let iter4 md =
                        match md with
                          | Simplified_vecfield (Enum_field ef, Id id, exp) ->
                            if not (List.exists (fun x -> x = ef) mds) then
                              (error "typecheck:get_vectorfields" "the mode '%s' is not declared before!\n" ef;
                               exit semantic_analysis_error)
                            else if H.mem vfofmd (lindex ef mds) then
                              (error "typecheck:get_vectorfields" "the mode '%s' was used before for '%s'!\n" ef traj;
                               exit semantic_analysis_error)
                            else if String.length id <= 4 then
                              (error "typecheck:get_vectorfields"
                                     "the length of the trajectory derivate's name identifier must be strictly greater than 4 \n and suffixed by _dot!\n";
                               exit semantic_analysis_error)
                            else
                              begin
                                let tidl = String.length id in
                                let dot = String.sub id (tidl - 4) 4 in
                                if dot <> "_dot" then
                                  (error "typecheck:get_vectorfields" "the trajectory derivate identifier must be suffixed by '_dot'!\n";
                                   exit semantic_analysis_error)
                                else
                                  begin
                                    let tid = String.sub id 0 (tidl - 4) in
                                    if tid <> traj then
                                      (error "typecheck:get_vectorfields" "the trajectory identifier '%s' must be equal to '%s'!\n" tid traj;
                                       exit semantic_analysis_error)
                                    else
                                      begin
                                        Buffer.add_string b @@ va "let f_%s_%d (t:float) (%s:float_vector) = \n "
                                          (String.lowercase_ascii traj) (lindex ef mds) (String.lowercase_ascii traj);
                                        get_simplifiedvf_xp b exp (List.length (H.find trajs tid)) cts vars trajs coefs (String.lowercase_ascii traj);
                                        Buffer.add_string b @@ va "\n\n";
                                      end;
                                      H.add vfofmd (lindex ef mds) ()
                                  end
                              end
                          | Expanded_vecfields (Enum_field ef, expds) ->
                            let vecofv = ref [] in
                            let iter5 expd =
                              if not (List.exists (fun x -> x = ef) mds) then
                                (error "typecheck:get_vectorfields" "the mode %s does not exist!\n" ef;
                                 exit semantic_analysis_error)
                              else if H.mem vfofmd (lindex ef mds) then
                                (error "typecheck:get_vectorfields" "the mode '%s' was used before for '%s'!\n" ef traj;
                                 exit semantic_analysis_error)
                              else
                                begin
                                  match expd with
                                    | Expanded_vecfield (Id var, evf) ->
                                      let tvarl = String.length var in
                                      let dot = String.sub var (tvarl - 4) 4 in
                                      if dot <> "_dot" then
                                        (error "typecheck:get_vectorfields" "the vector field derivate identifier must be suffixed by '_dot'!\n";
                                         exit semantic_analysis_error)
                                      else
                                        begin
                                          let tvar = String.sub var 0 (tvarl - 4) in
                                          if not (H.mem vars tvar) then
                                            (error "typecheck:get_vectorfields" "the vector field identifier '%s' is not declared!\n" tvar;
                                             exit semantic_analysis_error)
                                          else if not (List.exists (fun x -> x = tvar) (H.find trajs traj)) then
                                            (error "typecheck:get_vectorfields" "the vector field identifier '%s' is not declared in the trajectory '%s'!\n" tvar traj;
                                             exit semantic_analysis_error)
                                          else if List.exists (fun x -> x = tvar) !vecofv then
                                            (error "typecheck:get_vectorfields" "the vector field identifier '%s' was defined before!\n" tvar;
                                             exit semantic_analysis_error)
                                          else
                                            begin
                                              check_expandedvf_xps evf traj cts vars trajs coefs ;
                                              vecofv := tvar :: !vecofv ;
                                              Buffer.add_string b @@ va "let f_%s_%d_%d (%s:float_vector) (t:float) = \n "
                                                (String.lowercase_ascii traj) (lindex ef mds) (lindex tvar (H.find trajs traj)) (String.lowercase_ascii traj) ;
                                              Buffer.add_string b @@ va " %s\n\n" (get_expandedvf_xps evf cts vars trajs coefs (String.lowercase_ascii traj))
                                            end
                                        end
                                    | _ -> failwith "typecheck(get_vectorfields: bad ast!)"
                                end
                            in List.iter iter5 expds;
                            Buffer.add_string b @@ va "let f_%s_%d (t:float) (%s:float_vector) = \n "
                              (String.lowercase_ascii traj) (lindex ef mds) (String.lowercase_ascii traj);
                            Buffer.add_string b @@ va "get_float_vector_from_tfunvec t [|";
                            let lg = listlen (H.find trajs traj) in
                            for i = 0 to lg - 1 do
                              if i < lg - 1 then Buffer.add_string b @@ va "f_%s_%d_%d %s; " (String.lowercase_ascii traj) (lindex ef mds) i (String.lowercase_ascii traj)
                              else Buffer.add_string b @@ va "f_%s_%d_%d %s|] \n\n" (String.lowercase_ascii traj) (lindex ef mds) i (String.lowercase_ascii traj)
                            done;
                            H.add vfofmd (lindex ef mds) ()
                          | _  -> failwith "typecheck(get_vectorfields: bad ast!)"
                      in List.iter iter4 modes;
                      Buffer.add_string b @@ va "let vfs_%s () = \n let res = H.create 0 in\n" (String.lowercase_ascii traj);
                      for k = 0 to listlen mds - 1 do Buffer.add_string b @@ va " H.add res %d f_%s_%d;\n" k (String.lowercase_ascii traj) k done;
                      Buffer.add_string b @@ va " res\n\n";
                      H.add vecfs traj vfofmd
                    | _ -> failwith "typecheck(get_vectorfields: bad ast!)"
                end
            | _ -> failwith "typecheck(get_vectorfields: bad ast!)"
        in List.iter iter1 vfs;
        Buffer.add_string b @@ va "let vfs () = \n let res = H.create 0 in \n";
        let iter6 trj _ =
          Buffer.add_string b @@ va " H.add res \"%s\" (vfs_%s ());\n" (String.lowercase_ascii trj) (String.lowercase_ascii trj)
        in H.iter iter6 vecfs;
        Buffer.add_string b @@ va " res\n\n";
      | _  -> failwith "typecheck(get_vectorfields: bad ast!)"
  end;
  vecfs
   
  
let check_tss_xp
(t:ast)
(cts:(string,unit) H.t) =
  let rec sub tt =
    (match tt with
      | (Natural _ | Real _ | Integer _) -> ()
      | Time ->
        (error "typecheck:check_tss_xp" "time identifier is illegal in sampling parameter definitions!\n" ;
         exit semantic_analysis_error)
      | Id id ->
        if not (H.mem cts id) then
          (error "typecheck:check_tss_xp" "the identifier '%s' is not a constant!\n" id;
           exit semantic_analysis_error)
      | Unary_op (_,xp) -> sub xp
      | Parenthesed_xp xp -> sub xp
      | Binary_op (_,xp1,xp2) -> (sub xp1 ; sub xp2)
      | _ -> failwith "typecheck(check_simplifiedvf_xps:bad ast!)")
  in (sub t)
    
let get_samplingparams
(b:Buffer.t)
(t:ast)
(cts:(string,unit) H.t) =
  let finr = ref 0 in
  let dw = ref (-1) in
  begin
    match t with
      | Sampling_params sps ->
        (match List.nth sps 0 with
          | Time_sampling tt ->
            check_tss_xp tt cts;
            Buffer.add_string b @@ va "let tau = %s\n\n" (fst (get_xps tt cts))
          | _ -> failwith "typecheck(get_samplingparams:bad ast!)");
        (match List.nth sps 1 with
          | Space_sampling ee ->
            check_tss_xp ee cts;
            Buffer.add_string b @@ va "let eta = %s\n\n" (fst (get_xps ee cts))
          | _ -> failwith "typecheck(get_samplingparams:bad ast!)");
        (match List.nth sps 2 with
          | Finer (Natural ss) ->
            Buffer.add_string b @@ va "let fscale = %d\n\n" ss;
            finr := ss
          | _ -> failwith "typecheck(get_samplingparams:bad ast!)");
        if List.length sps = 4 then
          (match List.nth sps 3 with
           | Scale_minimal_dwell (Natural dd) ->
             if dd > !finr then
              (error "typecheck:check_tss_xp" "the scale of the minimal dwell time shall be lesser or equal to the finer scale sampling parameter '%d' !\n" !finr;
               exit semantic_analysis_error)
             else (Buffer.add_string b @@ va "let sdwell = %d\n\n" dd; dw := dd)
          | _ -> failwith "typecheck(get_samplingparams:bad ast!)")
        else Buffer.add_string b @@ va "let sdwell = %d\n\n" !dw;
      | _ -> failwith "typecheck(get_samplingparams:bad ast!)"
  end;
  (!finr,!dw)
  
  
let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  Unix.close_process (ic, oc)



let synthesis 
(b:Buffer.t)
(t:ast)
(d:int)
(mds:string list)
(cts:(string,unit) H.t)
(vars:(string,var_type) H.t)
(trajs:(string,string list) H.t)
(coefs:(string,var_type) H.t)
(spcs:(string,state_space) H.t)
(vecfs:(string,(int,unit) H.t) H.t)
(finr:int) =
    match t with
      | Safety (Id tr, Id saf, Id ini, Real ctime, Natural stps) ->
        if not (H.mem vecfs tr) then
          (error "typecheck:synthesis" "the vector fields for %s was not defined!\n" tr ;
           exit semantic_analysis_error)
        else if not (H.mem spcs saf) then
          (error "typecheck:synthesis" "the state-space '%s' is not declared before!\n" saf ;
           exit semantic_analysis_error)
        else if not (H.mem spcs ini) then
          (error "typecheck:synthesis" "the state-space '%s' is not declared before!\n" ini ;
           exit semantic_analysis_error)
        else if ctime < 0.0 then 
          (error "typecheck:synthesis" "the time sampling parameter for continuous synthesis must be greater or equal to 0 (%.3f) !\n" ctime;
           exit semantic_analysis_error)
        else
          begin
            let sf = H.find spcs saf in
            let it = H.find spcs ini in
            (match sf with
             | Sp (ssf) ->
               if not (is_simple_space it) then
                (error "typecheck:synthesis" "initial space '%s' shall be simple (<space> without \\<space>) like '%s'!\n" ini saf;
                 exit semantic_analysis_error)
               else
                begin
                  let sit = simple_space it in
                  if not (is_included sit ssf) then
                    (error "typecheck:synthesis" "%s is not included in %s!\n" ini saf;
                     exit semantic_analysis_error)
                end
             | SpMinusSp (ssf,msf) ->
                 if is_simple_space it then
                  (error "typecheck:synthesis" "initial space '%s' shall have the form (<space> \\ <space>) like '%s'!\n" ini saf;
                   exit semantic_analysis_error)
                     else
                  begin
                    let sit = simple_space it in
                    let mit = minus_space it in
                    if not (is_included sit ssf) then
                      (error "typecheck:synthesis" "%s is not included in %s !\n" ini saf ;
                       exit semantic_analysis_error)
                    else if not (is_included msf sit) then
                      (error "typecheck:synthesis" "%s is not included in %s !\n" ini saf ;
                       exit semantic_analysis_error)
                    else if not (is_included msf mit) then
                      (error "typecheck:synthesis" "%s is not included in %s !\n" ini saf ;
                       exit semantic_analysis_error)
                     end);
            Buffer.add_string b @@ va "let lat = {\n";
            Buffer.add_string b @@ va " dim = dim;\n";
            Buffer.add_string b @@ va " eta = eta;\n";
            Buffer.add_string b @@ va " scales = fscale;\n";
            Buffer.add_string b @@ va " safe = %s;\n" (String.lowercase_ascii saf);
            Buffer.add_string b @@ va " initial = %s;\n" (String.lowercase_ascii ini);
            Buffer.add_string b @@ va " target = Obj.magic ()\n";
            Buffer.add_string b @@ va "}\n\n";
            Buffer.add_string b @@ va "let sys = H.find (vfs ()) \"%s\" \n\n" (String.lowercase_ascii tr);
            Buffer.add_string b @@ va "let synthesis () = \n";
            Buffer.add_string b @@ va " let symbo = initialize_symbolic tau lat mds (sdwell = -1) in\n" ;
            Buffer.add_string b @@ va " let stime = Sys.time () in\n";
	    Buffer.add_string b @@ va " lazy_safety_synthesis symbo sys %f sdwell %d;\n" ctime stps;
            Buffer.add_string b @@ va " let etime = Sys.time () in\n";
            Buffer.add_string b @@ va " Printf.printf \"Ended in %s seconds\\n\" (etime -. stime);\n" "%f";
            Buffer.add_string b @@ va " Printf.printf \"The abstraction size is %s states\\n\" (Lts.size symbo.lts);\n" "%d";
            Buffer.add_string b @@ va " Printf.printf \"The controllability ratio is %s\\n\" (Lts.control_ratio symbo.lts symbo.lat symbo.finer (sdwell = -1));\n" "%.2f";
            for i = 0 to finr do
              if i < finr then Buffer.add_string b @@ va " Printf.printf \"Proportion of transitions of duration %s is %s\\n\" ((2.0 ** (-%d.0)) *. tau) (Lts.proportion symbo.lts %d);\n" "%.3f" "%.2f" i i
              else Buffer.add_string b @@ va " Printf.printf \"Proportion of transitions of duration %s is %s\\n\" ((2.0 ** (-%d.0)) *. tau) (Lts.proportion symbo.lts %d);\n" "%.3f" "%.2f" i i
            done;
          end; tr
      | Reachability (Id tr, Id saf, Id ini, Id tgt, Real maxb, Natural stps) ->
        if not (H.mem vecfs tr) then
          (error "typecheck:synthesis" "the vector fields for %s was not defined!\n" tr ;
           exit semantic_analysis_error)
        else if not (H.mem spcs saf) then
          (error "typecheck:synthesis" "the state-space '%s' is not declared before!\n" saf ;
           exit semantic_analysis_error)
        else if not (H.mem spcs ini) then
          (error "typecheck:synthesis" "the state-space '%s' is not declared before!\n" ini ;
           exit semantic_analysis_error)
        else if not (H.mem spcs tgt) then
          (error "typecheck:synthesis" "the state-space '%s' is not declared before!\n" tgt ;
           exit semantic_analysis_error)
        else if maxb <= 0. then
          (error "typecheck:synthesis" "the maximal time-boundary must be strictly grater than 0 (%f * tau <= 0)!\n" maxb ;
           exit semantic_analysis_error)
        else
          begin
            let sf = H.find spcs saf in
            let it = H.find spcs ini in
            let gt = H.find spcs tgt in
            if not (is_simple_space gt) then
              (error "typecheck:synthesis" "target space '%s' shall be simple (<space> without \\<space>) !\n" tgt;
               exit semantic_analysis_error);
            (match sf with
             | Sp (ssf) -> 
               if not (is_simple_space it) then
                (error "typecheck:synthesis" "initial space '%s' shall be simple (<space> without \\<space>) like '%s'!\n" ini saf;
                 exit semantic_analysis_error)
               else
                begin
                  let sit = simple_space it in
                  let sgt = simple_space gt in                  
                  if not (is_included sit ssf) then
                    (error "typecheck:synthesis" "%s is not included in %s!\n" ini saf;
                     exit semantic_analysis_error)
                  else if not (is_included sgt ssf) then
                    (error "typecheck:synthesis" "%s is not included in %s!\n" tgt saf;
                     exit semantic_analysis_error)
                end 
             | SpMinusSp(ssf,msf) ->
               if is_simple_space it then
                (error "typecheck:synthesis" "initial space '%s' shall have the form (<space> without \\<space>) like '%s'!\n" ini saf;
                 exit semantic_analysis_error)
               else
                begin
                  let sit = simple_space it in
                  let mit = minus_space it in
                  let sgt = simple_space gt in
                  if not (is_included sit ssf) then
                    (error "typecheck:synthesis" "%s is not included in %s !\n" ini saf ;
                     exit semantic_analysis_error)
                  else if not (is_included msf sit) then
                    (error "typecheck:synthesis" "%s is not included in %s !\n" ini saf ;
                     exit semantic_analysis_error)
                  else if not (is_included msf mit) then
                    (error "typecheck:synthesis" "%s is not included in %s !\n" ini saf ;
                     exit semantic_analysis_error)
                  else if not (is_included sgt ssf) then
                    (error "typecheck:synthesis" "%s is not included in %s !\n" tgt saf ;
                     exit semantic_analysis_error)
                  else if not (are_disjont sgt msf) then 
                    (error "typecheck:synthesis" "%s is not included in %s !\n" tgt saf;
                     exit semantic_analysis_error)
                end); 
            Buffer.add_string b @@ va "let time_bound = %f\n\n" maxb;
            Buffer.add_string b @@ va "let lat = {\n";
            Buffer.add_string b @@ va " dim = dim;\n";
            Buffer.add_string b @@ va " eta = eta;\n";
            Buffer.add_string b @@ va " scales = fscale;\n";
            Buffer.add_string b @@ va " safe = %s;\n" (String.lowercase_ascii saf);
            Buffer.add_string b @@ va " initial = %s;\n" (String.lowercase_ascii ini);
            Buffer.add_string b @@ va " target = %s\n" (String.lowercase_ascii tgt);
            Buffer.add_string b @@ va "}\n\n";
            Buffer.add_string b @@ va "let sys = H.find (vfs ()) \"%s\" \n\n" (String.lowercase_ascii tr);
            Buffer.add_string b @@ va "let synthesis () = \n";
            Buffer.add_string b @@ va " let symbo = initialize_symbolic tau lat mds (sdwell = -1) in\n";
            Buffer.add_string b @@ va " let stime = Sys.time () in\n";
            Buffer.add_string b @@ va " lsb_reachability_synthesis symbo sys time_bound %d;\n" stps;
            Buffer.add_string b @@ va " let etime = Sys.time () in\n";
            Buffer.add_string b @@ va " Printf.printf \"Ended in %s seconds\\n\" (etime -. stime);\n" "%f";
            Buffer.add_string b @@ va " Printf.printf \"The abstraction size is %s states\\n\" (Lts.size symbo.lts);\n" "%d";
            Buffer.add_string b @@ va " Printf.printf \"The controllability ratio is %s\\n\" (Lts.control_ratio symbo.lts symbo.lat symbo.finer (sdwell = -1));\n" "%.2f";
            for i = 0 to finr do
              if i < finr then Buffer.add_string b @@ va " Printf.printf \"Proportion of transitions of duration %s is %s\\n\" ((2.0 ** (-%d.0)) *. tau) (Lts.proportion symbo.lts %d);\n" "%.3f" "%.2f" i i
              else Buffer.add_string b @@ va " Printf.printf \"Proportion of transitions of duration %s is %s\\n\" ((2.0 ** (-%d.0)) *. tau) (Lts.proportion symbo.lts %d);\n" "%.3f" "%.2f" i i
            done;
          end; tr
      | _ -> failwith "typecheck(synthesis:bad ast!)"

let convert_labels
(tl:ast list)
(mds:string list)
(finr:int) =
  let rlbls = ref [] in
  let iter1 (label:ast) =
    match label with
      | Label (Enum_field ef, Natural sc) ->
        if not (List.exists (fun x -> x = ef) mds) then
          (error "typecheck:plot" "the mode '%s' does not exist!\n" ef;
           exit semantic_analysis_error)
        else if not (sc >= 0 && sc <= finr) then
          (error "typecheck:plot" "the scale of transitions to be plotted has to be an integer in {0..%d}!\n" finr;
           exit semantic_analysis_error)
        else
          begin
            let nmds = List.length mds in
            let imd = (lindex ef mds) in
            rlbls := ((nmds * sc) + imd) :: !rlbls
          end
      | _ ->  failwith "typecheck(convert_labels:bad ast!)"
  in List.iter iter1 tl;
  !rlbls

let plot
(b:Buffer.t)
(t:ast)
(d:int)
(mds:string list)
(trajs:(string,string list) H.t)
(fs:int)
(traj:string) 
(dw:int)=
  match t with
    | Plot_2d (aord,mors,mods_colors,scls_colors,Real xres,Real yres,lbls,curr_mode) ->
      if xres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" xres;
         exit semantic_analysis_error)
      else if yres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" yres;
         exit semantic_analysis_error)
      else if aord <> "arrows" && aord <> "dots" then
        (error "typecheck:plot" "allowed plotting modes are 'arrows' or 'dots'!\n";
         exit semantic_analysis_error)
      else if listlen mds <> listlen mods_colors then
        (* check colors before introduce them *)
        (error "typecheck:plot" "number of modes colors must be equal to %d and ordered with respect to modes declaration!\n" (listlen mds);
         exit semantic_analysis_error)
      else if listlen scls_colors <> fs + 1 then
        (error "typecheck:plot" "number of scales colors must be equal to %d and ordered from 0 to %d!\n" (fs + 1) fs;
         exit semantic_analysis_error)
      else if mors <> "modes" && mors <> "scales" then
        (error "typecheck:plot" "states are plotted according to the mode 'modes' or the scale 'scales' of the outgoing transitions!\n";
         exit semantic_analysis_error)
      else if d <> 2 then
        (error "typecheck:plot" "2D plotting is possible only for systems of dimension %d!\n" d;
         exit semantic_analysis_error)
      else if dw <> -1 && curr_mode = None then 
        (error "typecheck:plot" "a current mode shall be specified (multiple lyapunov functions) !\n";
         exit semantic_analysis_error)
      else if dw = -1 && curr_mode <> None then
        (error "typecheck:plot" "a current mode shall not be specified (common lyapunov functions) !\n";
         exit semantic_analysis_error)
      else
        begin
          (match curr_mode with 
            | Some e ->  
              let md = uw_enum_field e in
              if not (List.exists (fun x -> x = md) mds) then
                (error "typecheck:check_cases" "the mode '%s' does not exist!\n" md;
                 exit semantic_analysis_error)
              else
                begin
                  let rlbls = convert_labels lbls mds fs in
                  let modes_tp = ref true in
                  if mors = "scales" then modes_tp := false ;
                  if aord = "dots" then
                    Buffer.add_string b @@ va " plot_2d_%s symbo %.3f %.3f %s %b %s %s (sdwell = -1) (aindex \"%s\" symbo.modes)\n"
                      aord xres yres
                      (string_of_int_list_bis rlbls)
                      !modes_tp
                      (string_of_string_list_bis mods_colors)
                      (string_of_string_list_bis scls_colors)
                      md
                  else
                    Buffer.add_string b @@ va " plot_2d_%s symbo %.3f %.3f %b %s %s (sdwell = -1) (aindex \"%s\" symbo.modes)\n"
                      aord xres yres
                      !modes_tp
                      (string_of_string_list_bis mods_colors)
                      (string_of_string_list_bis scls_colors)
                      md
                end 
            | None -> 
              begin
                let rlbls = convert_labels lbls mds fs in
                let modes_tp = ref true in
                if mors = "scales" then modes_tp := false ;
                if aord = "dots" then
                  Buffer.add_string b @@ va " plot_2d_%s symbo %.3f %.3f %s %b %s %s (sdwell = -1) (Obj.magic ())\n"
                    aord xres yres
                    (string_of_int_list_bis rlbls)
                    !modes_tp
                    (string_of_string_list_bis mods_colors)
                    (string_of_string_list_bis scls_colors)
                else
                  Buffer.add_string b @@ va " plot_2d_%s symbo %.3f %.3f %b %s %s (sdwell = -1) (Obj.magic ())\n"
                    aord xres yres
                    !modes_tp
                    (string_of_string_list_bis mods_colors)
                    (string_of_string_list_bis scls_colors)
              end);
        end
    | Plot_2d3d (aord,mors,mods_colors,scls_colors,Real xres,Real yres,lbls,Id abs, Id ord, Real x3cord, curr_mode) ->
      if d <> 3 then
        (error "typecheck:plot" "plot trajectories only allowed for systems of dimension %d!\n" d;
         exit semantic_analysis_error)
      else if xres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" xres;
         exit semantic_analysis_error)
      else if yres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" yres;
         exit semantic_analysis_error)
      else if aord <> "dots" then
        (error "typecheck:plot" "allowed plotting modes is 'dots'!\n";
         exit semantic_analysis_error)
      else if listlen mds <> listlen mods_colors then
        (* check colors before introduce them *)
        (error "typecheck:plot" "number of modes colors must be equal to %d and ordered with respect to modes declaration!\n" (listlen mds);
         exit semantic_analysis_error)
      else if listlen scls_colors <> fs + 1 then
        (error "typecheck:plot" "number of scales colors must be equal to %d and ordered from 0 to %d!\n" (fs + 1) fs;
         exit semantic_analysis_error)
      else if mors <> "modes" && mors <> "scales" then
        (error "typecheck:plot" "states are plotted according to the mode 'modes' or the scale 'scales' of the outgoing transitions!\n";
         exit semantic_analysis_error)
      else if not (List.mem abs (H.find trajs traj)) then
        (error "typecheck:plot" "the coordinate %s doesn't belong to the trajectory %s !\n" abs traj;
         exit semantic_analysis_error)
      else if not (List.mem ord (H.find trajs traj)) then
        (error "typecheck:plot" "the coordinate %s doesn't belong to the trajectory %s !\n" ord traj;
         exit semantic_analysis_error)
      else if dw <> -1 && curr_mode = None then 
        (error "typecheck:plot" "a current mode shall be specified (multiple lyapunov functions) !\n";
         exit semantic_analysis_error)
      else if dw = -1 && curr_mode <> None then
        (error "typecheck:plot" "a current mode shall not be specified (common lyapunov functions) !\n";
         exit semantic_analysis_error)
      else
        begin 
          (match curr_mode with 
            | Some e ->
              let md = uw_enum_field e in
              if not (List.exists (fun x -> x = md) mds) then
                (error "typecheck:check_cases" "the mode '%s' does not exist!\n" md;
                 exit semantic_analysis_error)
              else 
                begin
                  let rlbls = convert_labels lbls mds fs in
                  let modes_tp = ref true in
                  if mors = "scales" then modes_tp := false ;
                  Buffer.add_string b @@ va " plot_2dof3d_dots symbo %d %d  %f %.3f %.3f %s %b %s %s (sdwell = -1) (aindex \"%s\" symbo.modes) \n"
                    (lindex abs (H.find trajs traj))
                    (lindex ord (H.find trajs traj))
                    x3cord
                    xres yres
                    (string_of_int_list_bis rlbls)
                    !modes_tp
                    (string_of_string_list_bis mods_colors)
                    (string_of_string_list_bis scls_colors)
                    md
                end 
            | None -> 
              let rlbls = convert_labels lbls mds fs in
              let modes_tp = ref true in
              if mors = "scales" then modes_tp := false ;
              Buffer.add_string b @@ va " plot_2dof3d_dots symbo %d %d  %f %.3f %.3f %s %b %s %s (sdwell = -1) (Obj.magic ()) \n"
                (lindex abs (H.find trajs traj))
                (lindex ord (H.find trajs traj))
                x3cord
                xres yres
                (string_of_int_list_bis rlbls)
                !modes_tp
                (string_of_string_list_bis mods_colors)
                (string_of_string_list_bis scls_colors));
       end
    | Plot_2d4d (aord,mors,mods_colors,scls_colors,Real xres,Real yres,lbls,Id abs, Id ord, Real x3cord, Real x4cord, curr_mode) ->
      if d <> 4 then
        (error "typecheck:plot" "plot trajectories only allowed for systems of dimension %d!\n" d;
         exit semantic_analysis_error)
      else if xres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" xres;
         exit semantic_analysis_error)
      else if yres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" yres;
         exit semantic_analysis_error)
      else if aord <> "dots" then
        (error "typecheck:plot" "allowed plotting modes is 'dots'!\n";
         exit semantic_analysis_error)
      else if listlen mds <> listlen mods_colors then
        (* check colors before introduce them *)
        (error "typecheck:plot" "number of modes colors must be equal to %d and ordered with respect to modes declaration!\n" (listlen mds);
         exit semantic_analysis_error)
      else if listlen scls_colors <> fs + 1 then
        (error "typecheck:plot" "number of scales colors must be equal to %d and ordered from 0 to %d!\n" (fs + 1) fs;
         exit semantic_analysis_error)
      else if mors <> "modes" && mors <> "scales" then
        (error "typecheck:plot" "states are plotted according to the mode 'modes' or the scale 'scales' of the outgoing transitions!\n";
         exit semantic_analysis_error)
      else if not (List.mem abs (H.find trajs traj)) then
        (error "typecheck:plot" "the coordinate %s doesn't belong to the trajectory %s !\n" abs traj;
         exit semantic_analysis_error)
      else if not (List.mem ord (H.find trajs traj)) then
        (error "typecheck:plot" "the coordinate %s doesn't belong to the trajectory %s !\n" ord traj;
         exit semantic_analysis_error)
      else if dw <> -1 && curr_mode = None then 
        (error "typecheck:plot" "a current mode shall be specified (multiple lyapunov functions) !\n";
         exit semantic_analysis_error)
      else if dw = -1 && curr_mode <> None then
        (error "typecheck:plot" "a current mode shall not be specified (common lyapunov functions) !\n";
         exit semantic_analysis_error)
      else
        begin
          (match curr_mode with 
            | Some e ->  
              let md = uw_enum_field e in
              if not (List.exists (fun x -> x = md) mds) then
                (error "typecheck:check_cases" "the mode '%s' does not exist!\n" md;
                 exit semantic_analysis_error)
              else 
                begin
                  let rlbls = convert_labels lbls mds fs in
                  let modes_tp = ref true in
                  if mors = "scales" then modes_tp := false ;
                  Buffer.add_string b @@ va " plot_2dof4d_dots symbo %d %d  %f %f %.3f %.3f %s %b %s %s (sdwell = -1) (aindex \"%s\" symbo.modes)\n"
                    (lindex abs (H.find trajs traj))
                    (lindex ord (H.find trajs traj))
                    x3cord
                    x4cord
                    xres yres
                    (string_of_int_list_bis rlbls)
                    !modes_tp
                    (string_of_string_list_bis mods_colors)
                    (string_of_string_list_bis scls_colors)
                    md
                end
            | None -> 
              let rlbls = convert_labels lbls mds fs in
              let modes_tp = ref true in
              if mors = "scales" then modes_tp := false ;
              Buffer.add_string b @@ va " plot_2dof4d_dots symbo %d %d  %f %f %.3f %.3f %s %b %s %s (sdwell = -1) (Obj.magic ())\n"
                (lindex abs (H.find trajs traj))
                (lindex ord (H.find trajs traj))
                x3cord
                x4cord
                xres yres
                (string_of_int_list_bis rlbls)
                !modes_tp
                (string_of_string_list_bis mods_colors)
                (string_of_string_list_bis scls_colors));
        end
    | Plot_arrows (Real xres,Real yres, curr_mode) ->
      if xres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" xres;
         exit semantic_analysis_error)
      else if yres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" yres;
         exit semantic_analysis_error)
      else if d <> 2 then
        (error "typecheck:plot" "simple arrows plotting is possible only for systems of dimension %d!\n" d;
         exit semantic_analysis_error)
      else if dw <> -1 && curr_mode = None then 
        (error "typecheck:plot" "a current mode shall be specified (multiple lyapunov functions) !\n";
         exit semantic_analysis_error)
      else if dw = -1 && curr_mode <> None then
        (error "typecheck:plot" "a current mode shall not be specified (common lyapunov functions) !\n";
         exit semantic_analysis_error)    
      else 
        (match curr_mode with 
          | Some e -> 
            let md = uw_enum_field e in
            if not (List.exists (fun x -> x = md) mds) then
              (error "typecheck:check_cases" "the mode '%s' does not exist!\n" md;
               exit semantic_analysis_error)
            else Buffer.add_string b @@ va " plot_2d_arrows_v2 symbo %.3f %.3f (sdwell = -1) (aindex \"%s\" symbo.modes)\n\n" xres yres md
          | None -> Buffer.add_string b @@ va " plot_2d_arrows_v2 symbo %.3f %.3f (sdwell = -1) (Obj.magic ())\n\n" xres yres);
        
    | Plot_trajectory (state,Real fin,Real step, Natural rk4stp,Real xres,Real yres, curr_mode, dc_or_none)->
      if d <> 2 then
        (error "typecheck:plot" "plot trajectories only allowed for systems of dimension %d!\n" d;
         exit semantic_analysis_error)
      else if xres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" xres;
         exit semantic_analysis_error)
      else if yres < 0.0 then
        (error "typecheck:plot" "resolutions must be positive (%.3f < 0)!\n" yres;
         exit semantic_analysis_error)
      else if listlen state <> d then
        (error "typecheck:plot" "the initial state must be of length %d!\n" d;
         exit semantic_analysis_error)
      else if fin <= 0.0 then
        (error "typecheck:plot" "the final time must be srictly grater than 0 (%f <= 0)!\n" fin;
         exit semantic_analysis_error)
      else if step <= 0.0 then
        (error "typecheck:plot" "the rk4 step must be srictly grater than 0 (%f <= 0)!\n" step;
         exit semantic_analysis_error)
      else if rk4stp <= 0 then
        (error "typecheck:plot" "the rk4 number's of iterations must be srictly grater than 0 (%d <= 0)!\n" rk4stp;
         exit semantic_analysis_error)
      else if dw <> -1 && curr_mode = None then 
        (error "typecheck:plot" "a current mode shall be specified (multiple lyapunov functions) !\n";
         exit semantic_analysis_error)
      else if dw = -1 && curr_mode <> None then
        (error "typecheck:plot" "a current mode shall not be specified (common lyapunov functions) !\n";
         exit semantic_analysis_error) 
      else
        (let both = (dc_or_none = None) in
         let discrete = (match dc_or_none with Some v -> v | None -> true) in
         match curr_mode with 
           | Some e ->  
             let md = uw_enum_field e in
               if dw <> -1 && not (List.mem md mds) then
                 (error "typecheck:check_cases" "the mode '%s' does not exist!\n" md;
                  exit semantic_analysis_error)
               else 
                 begin
                   Buffer.add_string b @@ va " plot_trajectory symbo (safety_control_multiple symbo sys (aindex \"%s\" symbo.modes) %s %f %f %d) %.3f %.3f %b %b\n"
                     md (string_of_float_list_bis (uw_real_list state)) fin step rk4stp xres yres both discrete
                 end
         | None -> 
           Buffer.add_string b @@ va " plot_trajectory symbo (safety_control symbo sys %s %f %f %d) %.3f %.3f %b %b\n"
             (string_of_float_list_bis (uw_real_list state)) fin step rk4stp xres yres both discrete)
    | _ -> failwith "typecheck(plot: bad ast!)"
   
let typecheck =
  function 
    | Root (_,dimension,modes,constants,variables,trajectories,coefficients,vecfields,sampling,spaces,synth,pspec) ->
      let buff = Buffer.create 0 in
      let file = open_out_gen [Open_append] 0o777 "./ocamldec.ml" in
      let dim = get_dimension buff dimension in
      let mds = get_modes buff modes in
      let cts = get_constants buff constants in
      let vars = get_variables variables cts in
      let trajs = get_trajectories trajectories dim cts vars in
      let coefs = get_coefficients buff coefficients dim cts vars trajs in
      let spcs = get_spaces buff spaces dim mds cts vars trajs coefs in
      let vecfs = get_vectorfields buff vecfields dim mds cts vars trajs coefs spcs in
      let finr_dw = get_samplingparams buff sampling cts in
      let traj = synthesis buff synth dim mds cts vars trajs coefs spcs vecfs (fst finr_dw) in
      if dim > 4 || dim = 1 then  Buffer.add_string buff @@ va " ()\n"
      else plot buff pspec dim mds trajs (fst finr_dw) traj (snd finr_dw);
      output_string file (Buffer.contents buff);
      close_out file
    | _ -> failwith "typecheck(bad ast !)"
