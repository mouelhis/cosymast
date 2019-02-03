%{
(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Sebti Mouelhi                                                           *)
(**                                                                                  *)
(** Copyright (c) 2011-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)
  
open Toolkit
open Typecheck 

(** Get the location of *)
let get_location pstart pend =
  let col_start = (pstart.Lexing.pos_cnum - pstart.Lexing.pos_bol) in
  let col_end = (pend.Lexing.pos_cnum - pend.Lexing.pos_bol) - 1 in
  let line_num = pstart.Lexing.pos_lnum in
    (line_num, col_start, col_end)

(** Parse error message. *)
let parse_error s =
  let lb = opt_get !Typecheck.the_lexbuf in
  let (l, cs, ce) = get_location (Lexing.lexeme_start_p lb) (Lexing.lexeme_end_p lb) in
  failwith (spf "Syntax error detected at line %d and characters %d-%d !" l cs ce)

(** Tuple (line,column). *)
let l_cs_ce () = get_location (symbol_start_pos ()) (symbol_end_pos ())
   
%}

 
/** UNTYPED TOKENS */
    
%token EOF COMMA SEMICOLON IN OPEN CLOSE OPEN_PAREN CLOSE_PAREN 
%token OPEN_BRACKET CLOSE_BRACKET DEFINITION MAP 
%token REALS VECTOR MATRIX
%token PLUS MINUS TIMES DIVIDE MODULUS POWER EQUALS
%token SINE COSINE TANGENT COTANGENT SECANT COSECANT ARCSINE ARCCOSINE ARCTANGENT HYPSINE HYPCOSINE HYPTANGENT
%token SQUAREROOT NATLOGARITHM EXPONENTIAL LOGARITHM 
%token SWITCHED DIMENSION MODES CONSTANTS VARIABLES TRAJECTORIES COEFFICIENTS VECTORFIELDS SAMPLING SAFETY REACHABILITY 
%token PLOT2D PLOT2DOF3D PLOT2DOF4D SIMULATION DISCRETE CONTINUOUS
%token TIME TAU ETA FINERSCALE SCALEMINDWELL WHEN FOR BELONGS THEN ELSE SIMPLIFIED EXPANDED SETIMES SETMINUS

/** TYPED TOKENS */

%token <string> ID ENUMFIELD
%token <int> INTEGER NATURAL
%token <float * float> INTERVAL
%token <float> REAL

/** PRECEDENCES */ 

%nonassoc MAP DEFINITION
%nonassoc POWER
%left PLUS MINUS
%left TIMES DIVIDE MODULUS
%left UPLUS UMINUS 
%left EQUALS


%start specification
%type <Typecheck.ast> specification
%%

/** Rules */  


specification:
  |  SWITCHED ID OPEN dimension modes constants variables trajectories 
     maybe_coefficients vecfields sampling spaces synthesis  
     plot CLOSE EOF                                           { Root ($2, $4, $5, $6, $7,  $8, $9, $10, $11, $12, $13, $14) }
;

dimension:
  | DIMENSION EQUALS NATURAL SEMICOLON                        { Dimension (Natural $3) }
;

modes:
  | MODES EQUALS OPEN mode_list CLOSE SEMICOLON               { Modes $4 }
;

mode_list: 
  | /* empty */                                               { [] }
  | ENUMFIELD mode_list                                       { Enum_field $1 :: $2 }
  | COMMA ENUMFIELD mode_list                                 { Enum_field $2 :: $3}
;
  
constants:
  | CONSTANTS IN cts_list                                     { Constants $3 }
;

cts_list: 
  | /* empty */                                               { [] }
  | id_list EQUALS xp SEMICOLON cts_list                      { Constants_by_value ($1, $3) :: $5}
;
 


id_list: 
  | ID id_list_tail                                           { Id $1 :: $2 }
;

id_list_tail:
  | /* empty */                                               { [] }
  | COMMA ID id_list_tail                                     { Id $2 :: $3 } 
;

variables: 
  | VARIABLES IN vars_list                                    { Variables $3 } 
;


vars_declaration:
  | id_list IN var_type                                       { Variables_by_type ($1, $3) }
;

vars_list:
  | vars_declaration vars_list_tail                           { $1 :: $2 }  
;

vars_list_tail:
  | SEMICOLON                                                 { [] }
  | SEMICOLON vars_declaration vars_list_tail                 { $2 :: $3 }
;  

var_type:
  | REALS                                                     { VT_real }
  | OPEN_BRACKET INTERVAL CLOSE_BRACKET                       { VT_interval $2 }
  | REALS MAP REALS                                           { VT_realtoreal }
  | REALS MAP OPEN_BRACKET INTERVAL CLOSE_BRACKET             { VT_realtointerval $4}
;

trajectories:
  | TRAJECTORIES IN trajs_list                                { Trajectories $3 }
;


traj_delaration:
  | ID EQUALS OPEN_BRACKET id_list CLOSE_BRACKET              { Trajectory ( Id $1, $4) }
;


trajs_list:
  | traj_delaration trajs_list_tail                           { $1 :: $2 }
;

trajs_list_tail:
  | SEMICOLON                                                 { [] }
  | SEMICOLON traj_delaration trajs_list_tail                 { $2 :: $3 }
;

maybe_coefficients:
  | /* empty */                                               { No_coefficients }
  | COEFFICIENTS IN coefs_list SEMICOLON coefs_values         { Coefficients ($3, $5) }
;

coefs_declaration:
  | id_list IN coef_type                                      { Coefficients_by_type ($1,$3) }
;

coefs_list: 
  | coefs_declaration coefs_list_tail                         { $1 :: $2 }
;

coefs_list_tail:
  | SEMICOLON                                                 { [] }
  | SEMICOLON coefs_declaration coefs_list_tail               { $2 :: $3 }
;  

coef_type:
  | MATRIX OPEN_PAREN NATURAL CLOSE_PAREN                     { VT_matrix $3 }
  | VECTOR OPEN_PAREN NATURAL CLOSE_PAREN                     { VT_vector $3 }
  | REALS MAP REALS                                           { VT_realtoreal }
  | REALS MAP OPEN_BRACKET INTERVAL CLOSE_BRACKET             { VT_realtointerval $4}
;

coef_value:
  | ID EQUALS coef_value                                      { Coefficient_value (Id $1, $3) }
;

coefs_values:
  | coef_value coefs_values_tail                              { $1 :: $2 }
;

coefs_values_tail:
  | SEMICOLON                                                 { [] }
  | SEMICOLON coef_value coefs_values_tail                    { $2 :: $3 }
;

coef_value:
  | xp                                                        { Xp $1 }
  | matrix                                                    { $1 }
  | vector                                                    { $1 }
;

/* 
   Note: the expressions are not necessarily coherent; 
   it is up to type-cheking to make sure that they are 
   well-formed.
  
   This kind of task can not be done here because expression 
   "ID" could really be {i anything}, and there is no way to know.  
*/

xp:
  | TIME                                                      { Time }
  | ID                                                        { Id $1 }
  | NATURAL                                                   { Natural $1 }
  | INTEGER                                                   { Integer $1 }
  | REAL                                                      { Real $1 }                                           
  | OPEN_PAREN xp CLOSE_PAREN                                 { Parenthesed_xp $2 }
  | MINUS xp                                                  { Unary_op  (UOP_minus, $2) }       %prec UMINUS
  | PLUS xp                                                   { Unary_op  (UOP_plus, $2) }        %prec UPLUS
  | SINE OPEN_PAREN xp CLOSE_PAREN                            { Unary_op  (UOP_sine, $3) }
  | COSINE OPEN_PAREN xp CLOSE_PAREN                          { Unary_op  (UOP_cosine, $3) }
  | TANGENT OPEN_PAREN xp CLOSE_PAREN                         { Unary_op  (UOP_tangent, $3) }
  | COTANGENT OPEN_PAREN xp CLOSE_PAREN                       { Unary_op  (UOP_cotangent, $3) }
  | SECANT OPEN_PAREN xp CLOSE_PAREN                          { Unary_op  (UOP_secant, $3) }
  | COSECANT OPEN_PAREN xp CLOSE_PAREN                        { Unary_op  (UOP_cosecant, $3) }
  | ARCSINE OPEN_PAREN xp CLOSE_PAREN                         { Unary_op  (UOP_arcsine, $3) }
  | ARCCOSINE OPEN_PAREN xp CLOSE_PAREN                       { Unary_op  (UOP_arccosine, $3) }
  | ARCTANGENT OPEN_PAREN xp CLOSE_PAREN                      { Unary_op  (UOP_arctangent, $3) }
  | HYPSINE OPEN_PAREN xp CLOSE_PAREN                         { Unary_op  (UOP_hypsine, $3) }
  | HYPCOSINE OPEN_PAREN xp CLOSE_PAREN                       { Unary_op  (UOP_hypcosine, $3) }
  | HYPTANGENT OPEN_PAREN xp CLOSE_PAREN                      { Unary_op  (UOP_hyptangent, $3) }
  | SQUAREROOT OPEN_PAREN xp CLOSE_PAREN                      { Unary_op  (UOP_squareroot, $3) }
  | NATLOGARITHM OPEN_PAREN xp CLOSE_PAREN                    { Unary_op  (UOP_natlogarithm, $3) }
  | EXPONENTIAL OPEN_PAREN xp CLOSE_PAREN                     { Unary_op  (UOP_exponential, $3) }
  | xp PLUS xp                                                { Binary_op (BOP_plus, $1, $3) }
  | xp MINUS xp                                               { Binary_op (BOP_minus, $1, $3) }
  | xp TIMES xp                                               { Binary_op (BOP_times, $1, $3) }
  | xp DIVIDE xp                                              { Binary_op (BOP_divide, $1, $3) }
  | xp MODULUS xp                                             { Binary_op (BOP_modulus, $1, $3) }
  | xp POWER xp                                               { Binary_op (BOP_power, $1, $3) }
  | LOGARITHM OPEN_PAREN xp COMMA xp CLOSE_PAREN              { Binary_op (BOP_logarithm, $3, $5) }
;

vector:
  | OPEN_BRACKET vector_elements CLOSE_BRACKET                { Vector $2}
;

vector_elements:
  | xp vector_elements_tail                                   { [Xp $1] @> $2 }
;


vector_elements_tail:
  | /* empty */                                               { [] }
  | COMMA xp vector_elements_tail                             { Xp $2 :: $3 }
;

matrix:
  | OPEN_BRACKET matrix_elements CLOSE_BRACKET                { Matrix $2}
;
  
matrix_elements:
  | vector matrix_elements_tail                               { [$1] @> $2 }
;

matrix_elements_tail:
  | /* empty */                                               { [] }
  | COMMA vector matrix_elements_tail                         { $2 :: $3 }
;


vecfields:
  | VECTORFIELDS IN vf_fors                                   { Vector_fields $3 }
;

vf_fors:
  | FOR ID IN vf_spec vf_fors_tail                            { Vector_field_for (Id $2, $4) :: $5 }
;

vf_fors_tail: 
  | /* empty */                                               { [] }
  | SEMICOLON FOR ID IN vf_spec vf_fors_tail                  { Vector_field_for (Id $3, $5) :: $6 }
;


vf_spec:
  | cases_by_spaces                                           { Cases_by_spaces $1 }
  | unique_case                                               { Unique_case $1 }
;

cases_by_spaces:
  | BELONGS case_by_space cases_by_spaces_tail                { $2 :: $3 } 
;

cases_by_spaces_tail:
  | ELSE BELONGS case_by_space cases_by_spaces_tail           { $3 :: $4 } 
  | ELSE unique_case                                          { [Unique_case $2] } 
;


case_by_space:
  | ID THEN unique_case                                       { Case_by_space (Id $1, Unique_case $3) }
;

unique_case: 
  | simplified_or_expanded SEMICOLON unique_case_tail         { $1 :: $3 }
;


unique_case_tail:
  | /* empty */                                               { [] }
  | simplified_or_expanded SEMICOLON unique_case_tail         { $1 :: $3 }
;

simplified_or_expanded:
  | SIMPLIFIED WHEN ENUMFIELD DEFINITION ID EQUALS xp         { Simplified_vecfield (Enum_field $3, Id $5, $7) } 
  | EXPANDED WHEN ENUMFIELD DEFINITION expanded               { Expanded_vecfields (Enum_field $3, $5) }
;


expanded:
  | ID EQUALS xp expanded_tail                                { Expanded_vecfield (Id $1, $3) :: $4}
;

expanded_tail:
  | /* empty */                                               { [] }
  | COMMA ID EQUALS xp expanded_tail                          { Expanded_vecfield (Id $2, $4) :: $5}
;


sampling: 
  | SAMPLING IN sparams_list                                  { Sampling_params $3 } 
;

sparams_list:
  | time_sam SEMICOLON space_sam SEMICOLON finer SEMICOLON scale_minimal_dwell SEMICOLON    
                                                              { [$1 ; $3 ; $5; $7] }
  | time_sam SEMICOLON space_sam SEMICOLON finer SEMICOLON    { [$1 ; $3 ; $5] }
;

time_sam:
  | TAU EQUALS xp                                             { Time_sampling $3 }
;

space_sam:
  | ETA EQUALS xp                                             { Space_sampling $3 }
;

finer:
  | FINERSCALE EQUALS NATURAL                                 { Finer (Natural $3) }
;

scale_minimal_dwell:
  | SCALEMINDWELL EQUALS NATURAL                              { Scale_minimal_dwell (Natural $3)}
;

spaces:
  | ID EQUALS state_space SEMICOLON spaces_tail               { Space (Id $1,$3) :: $5}
  | ID EQUALS state_space SETMINUS state_space SEMICOLON spaces_tail
                                                              { SpaceMinusSpace (Id $1,$3,$5) :: $7}
;

spaces_tail:
  | /* empty */                                               { [] }
  | ID EQUALS state_space SEMICOLON spaces_tail               { Space (Id $1,$3) :: $5}
  | ID EQUALS state_space SETMINUS state_space SEMICOLON spaces_tail
                                                              { SpaceMinusSpace (Id $1,$3,$5) :: $7}
;

state_space: 
  | OPEN_BRACKET INTERVAL CLOSE_BRACKET st_space_tail         { (Spacei (VT_interval $2)) :: $4 }
;
 
st_space_tail: 
  | /* empty */                                               { [] }
  | SETIMES OPEN_BRACKET INTERVAL CLOSE_BRACKET st_space_tail { (Spacei (VT_interval $3)) :: $5 }
; 


synthesis: 
  | safety                                                    { $1 }
  | reachability                                              { $1 }
;


safety: 
  | SAFETY OPEN_PAREN safety_elems CLOSE_PAREN SEMICOLON      { $3 }
; 

safety_elems:
  | ID COMMA ID COMMA ID COMMA NATURAL                        { Safety (Id $1, Id $3, Id $5, Real 0.0, Natural $7) }
  | ID COMMA ID COMMA ID COMMA REAL COMMA NATURAL             { Safety (Id $1, Id $3, Id $5, Real $7, Natural $9) }
;

 
reachability: 
  | REACHABILITY OPEN_PAREN reach_elems CLOSE_PAREN SEMICOLON { $3 }
;

reach_elems:
  | ID COMMA ID COMMA ID COMMA ID COMMA REAL COMMA NATURAL    { Reachability (Id $1, Id $3, Id $5, Id $7, Real $9, Natural $11) }
;


plot:
  | PLOT2D OPEN_PAREN plot2d CLOSE_PAREN                      { $3 }
  | PLOT2DOF3D OPEN_PAREN plot2d3d CLOSE_PAREN                { $3 }
  | PLOT2DOF4D OPEN_PAREN plot2d4d CLOSE_PAREN                { $3 }
  | PLOT2D OPEN_PAREN plotarrows CLOSE_PAREN                  { $3 }
  | SIMULATION OPEN_PAREN plot_trajs CLOSE_PAREN              { $3 }
;


plotarrows:
  | REAL COMMA REAL COMMA ENUMFIELD                           { Plot_arrows (Real $1, Real $3, Some (Enum_field $5)) }
  | REAL COMMA REAL                                           { Plot_arrows (Real $1, Real $3, None) }
;

plot_trajs:
  | OPEN_BRACKET real_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA NATURAL COMMA REAL COMMA REAL
                                                              { Plot_trajectory ($2, Real $5, Real $7, Natural $9, Real $11, Real $13, None, None) }
  | OPEN_BRACKET real_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA NATURAL COMMA REAL COMMA REAL COMMA ENUMFIELD
                                                              { Plot_trajectory ($2, Real $5, Real $7, Natural $9, Real $11, Real $13, Some (Enum_field $15), None) }
  | OPEN_BRACKET real_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA NATURAL COMMA REAL COMMA REAL COMMA DISCRETE
                                                              { Plot_trajectory ($2, Real $5, Real $7, Natural $9, Real $11, Real $13, None, Some true) }
  | OPEN_BRACKET real_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA NATURAL COMMA REAL COMMA REAL COMMA ENUMFIELD COMMA DISCRETE
                                                              { Plot_trajectory ($2, Real $5, Real $7, Natural $9, Real $11, Real $13, Some (Enum_field $15), Some true) }
  | OPEN_BRACKET real_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA NATURAL COMMA REAL COMMA REAL COMMA CONTINUOUS
                                                              { Plot_trajectory ($2, Real $5, Real $7, Natural $9, Real $11, Real $13, None, Some false) }
  | OPEN_BRACKET real_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA NATURAL COMMA REAL COMMA REAL COMMA ENUMFIELD COMMA CONTINUOUS
                                                              { Plot_trajectory ($2, Real $5, Real $7, Natural $9, Real $11, Real $13, Some (Enum_field $15), Some false) }

;


plot2d:
  | ID COMMA ID COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA OPEN_BRACKET string_list CLOSE_BRACKET 
    COMMA REAL COMMA REAL COMMA OPEN_BRACKET labels_to_print CLOSE_BRACKET                
                                                              { Plot_2d ($1, $3, $6, $10, Real $13, Real $15, $18, None) }
  | ID COMMA ID COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA OPEN_BRACKET string_list CLOSE_BRACKET 
    COMMA REAL COMMA REAL COMMA OPEN_BRACKET labels_to_print CLOSE_BRACKET COMMA ENUMFIELD               
                                                              { Plot_2d ($1, $3, $6, $10, Real $13, Real $15, $18, 
                                                                         Some (Enum_field $21)) }
;


plot2d3d:
  | ID COMMA ID COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA 
    OPEN_BRACKET labels_to_print CLOSE_BRACKET COMMA ID COMMA ID COMMA REAL                  
                                                              { Plot_2d3d ($1, $3, $6, $10, Real $13, Real $15, $18, Id $21, Id $23, Real $25, None) }
  | ID COMMA ID COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA 
    OPEN_BRACKET labels_to_print CLOSE_BRACKET COMMA ID COMMA ID COMMA REAL COMMA ENUMFIELD                   
                                                              { Plot_2d3d ($1, $3, $6, $10, Real $13, Real $15, $18, Id $21, Id $23, Real $25, 
                                                                           Some (Enum_field $27)) }
;

plot2d4d:
  | ID COMMA ID COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA 
    OPEN_BRACKET labels_to_print CLOSE_BRACKET COMMA ID COMMA ID COMMA REAL COMMA REAL                 
                                                              { Plot_2d4d ($1, $3, $6, $10, Real $13, Real $15, $18, Id $21, Id $23, Real $25, Real $27, None) }
  | ID COMMA ID COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA OPEN_BRACKET string_list CLOSE_BRACKET COMMA REAL COMMA REAL COMMA 
    OPEN_BRACKET labels_to_print CLOSE_BRACKET COMMA ID COMMA ID COMMA REAL COMMA REAL COMMA ENUMFIELD                
                                                              { Plot_2d4d ($1, $3, $6, $10, Real $13, Real $15, $18, Id $21, Id $23, Real $25, Real $27, 
                                                                           Some (Enum_field $29)) }
;

real_list: 
  | REAL real_list_tail                                       { Real $1 :: $2 }
;

real_list_tail:
  | /* empty */                                               { [] }
  | COMMA REAL real_list_tail                                 { Real $2 :: $3 } 
;

string_list: 
  | ID string_list_tail                                       { $1 :: $2 }
;

string_list_tail:
  | /* empty */                                               { [] }
  | COMMA ID string_list_tail                                 { $2 :: $3 } 
;

labels_to_print:
  | OPEN_PAREN ENUMFIELD COMMA NATURAL CLOSE_PAREN 
    labels_to_print_tail                                      { Label (Enum_field $2, Natural $4) :: $6 }
;  
  
 
labels_to_print_tail:
  | /* empty */                                               { [] }
  | COMMA OPEN_PAREN ENUMFIELD COMMA NATURAL CLOSE_PAREN 
    labels_to_print_tail                                      { Label (Enum_field $3, Natural $5) :: $7 } 
;
