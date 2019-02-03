(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Sebti Mouelhi                                                           *)
(**                                                                                  *)
(** Copyright (c) 2011-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)

open Toolkit
open Typecheck
open Ocamldec

exception Illegal_command

(** Build the parse tree from a source file *)
let parse_from_file path =
  let lexbuf = Lexing.from_channel (open_in path) in
  lexbuf.Lexing.lex_curr_p <- 
    { 
      lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = path
    };
  the_lexbuf := Some lexbuf;
  let r = Parser.specification Lexer.token lexbuf in
  Parsing.clear_parser(); r
  
  
(** Welcome message *) 
let welcome() = pf "CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions. 
Copyright (c) 2011-2015, INRIA.                                        

"

let () = 
  welcome ();
  let t = Global.empty "Checking Parse Tree" in
  if arrlen Sys.argv = 1 then 
    (error "main" "the authorized commands are './cosyma parse <file>.conf' or ./cosyma synthesis (missing arguments) !\n";
     raise Illegal_command) 
  else 
  (let arg1 = Sys.argv.(1) in 
   if arg1 = "parse" then  
    begin
      if arrlen Sys.argv = 2 then 
        (error "main" "the authorized commands are './cosyma parse <file>.conf' or ./cosyma synthesis (missing arguments) !\n";
         raise Illegal_command);
      if not (Sys.file_exists ("./" ^ Sys.argv.(2))) then
        (error "main" "the file '%s' doesn't exist in tool directory!\n" Sys.argv.(2);
         raise Illegal_command);
      let arg2 = Sys.argv.(2) in
      let lg = String.length arg2 in
      let ip = String.index arg2 '.' in
      if String.sub arg2 ip (lg - ip) = ".conf" then
        begin
          let f1 = open_out "./ocamldec.ml" in
          let buff = Buffer.create 0 in
          let add = Buffer.add_string buff in
          add @@ va "(** ocamldec.ml is generated automatically *) \n\n";
          add @@ va "open Toolkit \n";
          add @@ va "open Ode \n";
          add @@ va "open Symbolic \n"; 
          add @@ va "open Lts \n"; 
          add @@ va "open Safety\n";
          add @@ va "open Control\n";
          add @@ va "open Reachability\n";
          add @@ va "open Plot \n\n";
          output_string f1 (Buffer.contents buff) ;
          close_out f1;
          let add_empty_funcs () =
            (add @@ va "let synthesis () = ()\n\n" ;
            let f2 = open_out "./ocamldec.ml" in
            output_string f2 (Buffer.contents buff) ;
            close_out f2)
          in
          (try
            flush stdoutc; flush stderrc;
            Printf.printf "Parsing ... \n";
            Global.set t (parse_from_file ("./" ^ arg2));
            flush stdoutc; flush stderrc;
            Printf.printf "Typechecking ... \n";
            Typecheck.typecheck (Global.get t) ;
            (* Print_ast.print (Global.get t) ; *)
            ignore (Sys.command "make > /dev/null");
            Printf.printf "Done.\n\n";
            Printf.printf "Run \"cosyma synthesis\" to synthesize the abstraction.\n";
            ignore (Sys.command "ulimit -s unlimited > /dev/null")
           with
	   | Failure s -> (add_empty_funcs (); pf "%s\n" s)
	   | e -> add_empty_funcs (); raise e)
        end 
      else 
        begin 
          (error "main:()" "a configuration file must end with '.conf'!\n";
           raise Illegal_command)
        end
    end
  else if arg1 = "synthesis" then 
    begin
      Printf.printf "Synthesis ...\n"; 
      Ocamldec.synthesis (); 
      Printf.printf "Done.\n\n";
      Printf.printf "Run \"tikz2pdf plot.tikz\" to generate the plot.\n"
    end 
  else
    (error "main" "the authorized commands are './cosyma parse <file>.conf' or ./cosyma synthesis!\n";
     raise Illegal_command))
