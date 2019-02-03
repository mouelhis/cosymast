{
(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Sebti Mouelhi                                                           *)
(**                                                                                  *)
(** Copyright (c) 2011-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)

open Toolkit
open Parser



(** Header section *)

(* Lexer error message. *)
let lexing_error c lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in 
  let line = curr.Lexing.pos_lnum in
  let column = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  failwith (spf "lexer failed on '%c' at line %d and column %d !" c line column)

(* Increment the line number. *)
let incr_linenum lexbuf = Lexing.new_line lexbuf

} 

let id_pat  = ['A'-'Z' 'a'-'z' '_'] [ '_' '-' '\'' 'A'-'Z' 'a'-'z' '0'-'9'] *

let digit = ['0'-'9']

let natural_pat = digit+

let integer_pat = ['-']? natural_pat

let real_pat = ['-']? natural_pat '.' digit*

let interval_pat = (real_pat as lb) " .. " (real_pat as ub)

let enumf_pat  = '`' (id_pat as x)


rule token = parse
  | [' ' '\t']               { token lexbuf }     (* skip blanks *)
  | '\n'                     { incr_linenum lexbuf; token lexbuf }  
  | "/*"                     { comment lexbuf ; token lexbuf }
  | "//"                     { line_comment lexbuf ; token lexbuf }
  | ','                      { COMMA }
  | ';'                      { SEMICOLON }
  | '{'                      { OPEN }
  | '}'                      { CLOSE }
  | '('                      { OPEN_PAREN }
  | ')'                      { CLOSE_PAREN }
  | '['                      { OPEN_BRACKET }
  | ']'                      { CLOSE_BRACKET }
  | ':'                      { IN }
  | '='                      { EQUALS } 
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { TIMES }
  | '/'                      { DIVIDE }
  | '%'                      { MODULUS }
  | '^'                      { POWER }
  | '#'                      { SETIMES }
  | '\\'                     { SETMINUS }
  | "real"                   { REALS } 
  | "sin"                    { SINE }
  | "cos"                    { COSINE }
  | "tg"                     { TANGENT }
  | "ctg"                    { COTANGENT }
  | "sec"                    { SECANT }
  | "csc"                    { COSECANT }
  | "asin"                   { ARCSINE }
  | "acos"                   { ARCCOSINE }
  | "atg"                    { ARCTANGENT }
  | "sinh"                   { HYPSINE }
  | "cosh"                   { HYPCOSINE }
  | "tanh"                   { HYPTANGENT }
  | "sqrt"                   { SQUAREROOT }
  | "ln"                     { NATLOGARITHM }
  | "exp"                    { EXPONENTIAL }
  | "log"                    { LOGARITHM }
  | "Matrix"                 { MATRIX }
  | "Vector"                 { VECTOR }
  | "Switched"               { SWITCHED }
  | "Dimension"              { DIMENSION }
  | "Modes"                  { MODES }
  | "Constants"              { CONSTANTS }
  | "Variables"              { VARIABLES }
  | "Trajectories"           { TRAJECTORIES }
  | "Coefficients"           { COEFFICIENTS }
  | "VectorFields"           { VECTORFIELDS }
  | "for"                    { FOR }
  | "in"                     { BELONGS }
  | "then"                   { THEN }
  | "else"                   { ELSE } 
  | "simplified"             { SIMPLIFIED  }
  | "expanded"               { EXPANDED }
  | "when"                   { WHEN }
  | "::"                     { DEFINITION }
  | "@t"                     { TIME }
  | "->"                     { MAP } 
  | "SamplingParameters"     { SAMPLING }
  | "time"                   { TAU }
  | "space"                  { ETA }
  | "finer"                  { FINERSCALE } 
  | "scale_min_dwell"        { SCALEMINDWELL }
  | "SafetySynthesis"        { SAFETY }
  | "ReachabilitySynthesis"  { REACHABILITY }
  | "Plot2D"                 { PLOT2D }
  | "Plot2Dof3D"             { PLOT2DOF3D }
  | "Plot2Dof4D"             { PLOT2DOF4D }
  | "discrete"               { DISCRETE }
  | "continuous"             { CONTINUOUS }
  | "Simulate"               { SIMULATION }
  | id_pat as x              { ID x }
  | enumf_pat                { ENUMFIELD x }
  | interval_pat             { INTERVAL (fos lb, fos ub) } 
  | natural_pat as x         { NATURAL (ios x) }
  | integer_pat as x         { INTEGER (ios x) }
  | real_pat as x            { REAL (fos x) }
  | "pi"                     { REAL (pi) }
  | eof                      { EOF }
  | _ as c                   { lexing_error c lexbuf }

and comment = parse
  | "/*"                     { comment lexbuf }
  | "\n"                     { incr_linenum lexbuf; comment lexbuf }
  | "*/"                     { }
  | eof                      { failwith "Unexpected end of file after '/*' !" }
  | _                        { comment lexbuf }
  
and line_comment = parse
  | "\n"                     { incr_linenum lexbuf }
  | _                        { line_comment lexbuf }
  | eof                      { } 
