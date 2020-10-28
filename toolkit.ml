(** **********************************************************************************)
(** CoSyMA: a tool for COntroller SYnthesis using Multi-scale Abstractions           *)   
(** @author: Vincent Hugot, Sebti Mouelhi                                            *)
(**                                                                                  *)
(** Copyright (c) 2007-2015, INRIA Rhône-Alpes                                       *)
(** All rights reserved                                                              *)
(** **********************************************************************************)

open ExtLib

(** Shortcut for oft-used hash table module *)
module H = Hashtbl


(** Remove all bindings for the given key *)



exception Found of int

(** {6 Miscellaneous} *)  

(** Functions composition operator *)
let (%) f g x = f (g x)

(** Function application operator (read "of"). This is used to avoid LISP-like parentheses creep;
  the point of using an [@...] operator is that it has just the right precedence and is right-associative *)
let (@@) f x = f x

(** Identity *)
let id s = s

(** Do nothing *)
let do_nothing x = () 

(** negation of predicate *)
let nega f = fun x -> not (f x)

(** Exception used purely for flow control in imperative style *)
exception Jump;;

(** Jump with boolean value *)
exception JumpB of bool;;

(** Jump with integer value *)
exception JumpI of int;;

(** Transform a function into a totally memoized version
    of itself. Note that recursive calls the function might make
    are not memoized *)
let memoize ?(sz=32) f = 
  let h = H.create sz in fun x -> 
  try H.find h x with Not_found -> 
    let res = f x in
    H.add h x res; res
    
(** Transform a recursive function into a totally memoized version
    of itself. In order to allow memoization of recursive calls,
    the function must take an extra "self" argument, and use
    it for recursive calls. (got that idea from code by Andrej Bauer) *)
let memoize_rec ?(sz=32) f =
  let h = H.create sz in
  let rec g x = 
    try H.find h x with Not_found -> 
      let res = f g x in
      H.add h x res; res
  in g
  
(** [finally tryme final failedval] will execute [tryme()], and then [final()] even if
an exception is raised, in which case it will return [failedval]. *)
let finally tryme final failedval =
  try let res = tryme() in final(); res
  with x -> final(); failedval
  


(** {6 About options and references types...} *)

(** Toggle an option on and off *)
let toggle br = br := not !br


let swallow obj = ()

exception Empty_option

(** Get the contents of an option, fail if None *)
let opt_get = function 
  | None -> raise Empty_option 
  | Some x -> x

(** Get the contents of an option, use a default value if None *)
let opt_getd d = function
  | None -> d
  | Some x -> x 
  
(** Find element in an Hashtable: returns None instead of raise Not_found *)
let hfind'o ht x =  
  try Some (H.find ht x)
  with Not_found -> None
  
  
let arrexists p xs =
  let n = DynArray.length xs in
  let rec loop i =
    if i = n then false
    else if p (DynArray.get xs i) then true
    else loop (succ i)
  in
  loop 0
  
let arrmem a xs =
  let n = DynArray.length xs in
  let rec loop i =
    if i = n then false
    else if a = (DynArray.get xs i) then true
    else loop (succ i)
  in
  loop 0


(** Modified Global module from Extlib library, Copyright (C) 2003 Nicolas Cannasse 

Mutable global variable.
 
  Often in OCaml you want to have a global variable, which is mutable 
    and uninitialized when declared. You can use a ['a option ref] but
  this is not very convenient. The Global module provides functions
  to easily create and manipulate such variables.
*)
module Global = struct

  (** Raised when a global variable is accessed without first having been
  assigned a value. The parameter contains the name of the global. *)  
  exception Global_not_initialized of string
  
  (** Abstract type of a global *)
  type 'a t = ('a option ref * string)
  
  (** Returns an new named empty global. The name of the global can be any
 string. It identifies the global and makes debugging easier. *)
  let empty name = ref None,name
  
  (** Retrieve the name of a global. *)
  let name = snd
  
  (** Set the global value contents. *)
  let set (r,_) v = r := Some v
  
  (** Get the global value contents - raise Global_not_initialized if not
 defined. *)
  let get (r,name) =
          match !r with
          | None -> raise (Global_not_initialized name)
          | Some v -> v

  (** Get the global value contents - return default value if not defined *)
  let getd d (r, name) = match !r with
    | None -> d
    | Some v -> v
  
  (** Reset the global value contents to undefined. *)
  let undef (r,_) = r := None
  
  (** Return [true] if the global value has been set. *)
  let isdef (r,_) = !r <> None
  
  (** Return [None] if the global is undefined, else [Some v] where v is the
  current global value contents. *)
  let opt (r,_) = !r
end

type 'a global = 'a Global.t

(** {6 Functions about strings and files } *)

(** Is this a blank ? *)
let is_blank_char = function ' '|'\t'|'\n'|'\r' -> true | _ -> false 

(** Trim white spaces  *)
let rec trim s =
  let l = String.length s in
  if l=0 then s 
  else if is_blank_char s.[0]   then trim (String.sub s 1 (l-1))
  else if is_blank_char s.[l-1] then trim (String.sub s 0 (l-1))
  else s

(** Note: The four next functions are taken from the Str library. 
  The reason why they are not in String is beyond me...

  I have modified the first two so that they always return the string instead of 
  raising exceptions, which is what I need most of the time.
*)

(** [first_chars s n] returns the first [n] characters of [s], and [s] itself if [n] is too big *)
let first_chars s n = try String.sub s 0 n with _ -> s
      
(** [last_chars s n] returns the last [n] characters of [s], and [s] itself if [n] is too big *)
let last_chars s n = try String.sub s (String.length s - n) n with _ -> s

(** [string_before s n] returns the substring of all characters of [s]
   that precede position [n] (excluding the character at
   position [n]). *)
let string_before s n = String.sub s 0 n        

(** [string_after s n] returns the substring of all characters of [s]
   that follow position [n] (including the character at
   position [n]). *)
let string_after s n = String.sub s n (String.length s - n)

(** Get an extract of a string, up to [n+3] chars, the extra three being the "..." if the string
  is trunctated *)
let snippet s n = let res = first_chars s n in
  res ^ if String.length s <= n then "" else "..."

(** Similar in purpose to [String.sub], extracts between two positions, inclusively *)
let subb s ps pe = String.sub s ps (pe-ps)

(** Add an 's' at the end of the string if [n] > 2 *)
let pluralise s n = match abs n with 0|1 -> s | _ -> s^"s"

(** Replace by an alternative if [n] > 2 *)
let pluralise_alt s ss n = match abs n with 0|1 -> s | _ -> ss

(** Plain text for numbers *)
let text_of_number = function
  | 0 -> "no" | 1 -> "one" | 2 -> "two" | 3 -> "three" | 4 -> "four" 
  | 5 -> "five" | 6 -> "six" | 7 -> "seven" | 8 -> "eight" 
  | 9 -> "nine" | 10 -> "ten" | 11 -> "eleven" | 12 -> "twelve" 
  | 13 -> "thirteen" 
  | n -> string_of_int n
    
(** Get rid of trailing \n *)
let rec no_newline s = let l = String.length s in match l with
  | 0 -> ""
  | _ -> if s.[l-1] = '\n' || s.[l-1] = (char_of_int 13) then no_newline (String.sub s 0 (l-1)) else s;;

(** Input a file into a list, tail-recursively, transforming each line through f (not entirely by me) *)
let lines_of_file ?(f=fun x -> x) file = 
  let chan  = (open_in file) in
  let rec input_lines_helper res =
  let sl = 
    try
      Some (input_line chan) 
    with
      End_of_file -> None 
  in match sl with
       None -> List.rev res
     | Some l -> input_lines_helper ((f l) :: res) 
  in input_lines_helper [];;

(** Run f for each line in the file *)
let run_on_file f path = let ch = open_in path in
  try while true do f (input_line ch)
  done with End_of_file -> ()

(** {6 Some shortcuts} *)

(** {7 Printing } *)

(** Print line to stdout *)
let pl  = print_endline
(** Print carriage return to stdout *)
let pnl = print_newline
(** Print line to stderr *)
let pe  = prerr_endline
(** Print string to stdout *)
let ps  = print_string
(** = [Printf.printf]: formated printing to stdout  *)
let pf  = Printf.printf
(** = [Printf.sprintf]: formated printing to string *)
let spf = Printf.sprintf  
(** = [Printf.eprintf]: formated printing to stderr  *)
let epf = Printf.eprintf
(** = [Printf.fprintf]: formated printing to output channel  *)
let fpf = Printf.fprintf
(** = [spf]: formated printing to stdout  *)
let va  = spf

(** {7 Lists, strings and such }*)

(** Map a function onto a pair *)
let map_pair f (x,y) = (f x, f y)

(** Map a function onto a triple *)
let map_triple f (x,y,z) = (f x, f y, f z)

(** Get the tail of list *)  
let tl = List.tl
(** Get the head of a list*)
let hd = List.hd

let is_empty l = List.length l = 0


(** Push a value as head of a list reference *)
let push x l = l := x :: !l

(** Get the first value on a "stack" without removing it *)
let peek stack = match !stack with
  | a::_ -> a
  | [] -> failwith "peek_stack"
  
(** Get the first value on a "stack" and remove it *)
let pop stack = match !stack with
  | a::l -> stack := l; a
  | [] -> failwith "pop_stack"

(** Remove the first value on a "stack"  *)
let pop_2 stack = match !stack with
  | a::l -> stack := l 
  | [] -> failwith "pop_stack"

(** Augment a hashset with the contents of a list *)
let hashset_augment set l = 
  let add x = H.replace set x () in
  List.iter add l

let hashset_augment_value set l v = 
  let add x = H.replace set x v in
  List.iter add l
  
(** Get a hashset containing all elements of a list. Linear in the length of the list *)
let hashset_of_list ?(sz=32) l  = 
  let set = H.create sz in hashset_augment set l; set
  
let hashset_of_list_value ?(sz=32) l v = 
  let set = H.create sz in hashset_augment_value set l v; set
  
(** Get the list of the keys of a hashtable *)
let hkeys h = let f k b l = k::l in Hashtbl.fold f h []


(** Get the list of the associated values of a hashtable *)
let hvalues h = let f k b l = b::l in Hashtbl.fold f h []

(** Get the association list equivalent to the hashtable *)
let hassoc h = let f k b l = (k, b)::l in Hashtbl.fold f h []

let assoc_list2hashtbl assoc_list = 
  let h = Hashtbl.create 0 in
  List.iter (fun (k,v) -> Hashtbl.replace h k v) assoc_list ;
  h

let hashtbl_merge h1 h2 = assoc_list2hashtbl (hassoc h1 @ hassoc h2)




(** Get the reversed asssociation list *)
let reverse_assoc_list l = 
  let rev (x, y) = y, x in List.map rev l
  

let print_hash hash = Hashtbl.iter (pf "%s => %s\n") hash 


(** Is a list included in another ? [O(sum of lengths of lists)].
 (more efficient if larger list is arg 1) *)
let list_included l l' = 
  let h' = hashset_of_list l' in
  List.for_all (H.mem h') l
  
(** Is there an intersection between two lists ? [O(sum of lengths of lists)] 
(more efficient if larger list is arg 1) *)
let lists_intersect l l' = 
  let h' = hashset_of_list l' in
  List.exists (H.mem h') l
  
(** get the intersection between two lists [O(sum of lengths of lists)] 
(more efficient if larger list is arg 1) *)
let lists_intersection l l' = 
  let h' = hashset_of_list l' in
  List.filter (H.mem h') l
  
(** Do the lists contain the same elements ? (seen as sets)
(more efficient if larger list is arg 1) *)
let lists_equivalent l l' = 
  let h' = hashset_of_list l' in
  let chk x = if H.mem h' x then (H.remove h' x; true) else false
  in List.for_all chk l && H.length h' = 0
  
(** get difference between two lists seen as sets: {x in l / x not in l'} 
(more efficient if larger list is arg 1) *) 
let lists_diff l l' = 
  let h' = hashset_of_list l' in
  List.filter (nega (H.mem h')) l
  
let array_diff a a' = 
  let h' = hashset_of_list (Array.to_list a') in
  Array.of_list (List.filter (nega (H.mem h')) (Array.to_list a))

(** Operator to concatenate lists tailrecursively, as opposed to [@].
 Use whenever order does not matter (especially for lists seen as sets) *)
let (@<) = List.rev_append
let (@>) = List.append





(** Remove duplicate elements in a list, without any regard for
the order in which they were. Linear time. *)
let remove_duplicates l = hkeys (hashset_of_list l)
  
(** Does the list have duplicated elements ?  *)
let has_duplicates ?(sz=32) l  = 
  let h = H.create sz in
  let chk x = H.mem h x || (H.add h x (); false) in
  List.exists chk l    
  
(** Get list of duplicated elements of a list (elements that have [n] occurences
  appear [n-1] times) *)
let list_duplicates ?(sz=32) l  = 
  let h = H.create sz in
  let chk x = H.mem h x || (H.add h x (); false) in
  List.filter chk l

(** get symmetric difference between two lists seen as sets 
l U l' - l' /\ l = l-l' U l'-l *)
let lists_symdiff l l' =
  let h, h' = hashset_of_list l, hashset_of_list l' in
  let amb l h = List.filter (nega (H.mem h)) l in
  amb l h' @< amb l' h
  
(** get a random list of ints of length n *) 
let random_list n = 
  let rec random_list_hlp n l =
    if n <= 0 then l
    else random_list_hlp (pred n) (Random.int max_int :: l)
  in random_list_hlp n []
  
let random_float (a:float) (b:float) =
  Random.self_init ();
  Random.float ( b -. a +. 1.0 ) +. a;;

(** "cartesian product" of two lists *)
let rec product_lists l = function
  | a :: l' -> List.map (fun x->(x, a)) l @> product_lists l l'
  | [] -> []
  
(** Generate the powerset of a list; that is to say, the list of all sublists.
 This implementation guarantees that the elements of the resulting sublists
 will be in the same order than in the original list *)
let list_powerset l =
  let increase a = List.map (fun l->a::l) in
  let rec hlp = function 
    | [] -> [[]]
    | a :: l -> let pl = hlp l in
      (increase a pl) @< pl
  in hlp l
  
(** Generate the list of all [k]-subsequences of a list, that is to say,
 of all subsequences of the list which are of length [k] *)
let rec k_subsequences k l = 
  let add x lists = List.map (fun l->x::l) lists
  in match k, l with
  | 0, _ | _, [] -> []
  | 1, l -> List.map (fun l->[l]) l
  | k, a::l -> (add a @@ k_subsequences (k-1) l) @< k_subsequences k l
   
let string_of_int_2 x = 
  begin if x < 0 then 
            "neg_" ^ (string_of_int (-x))
        else 
            string_of_int x
  end           
                 
(** = [string_of_float] *)
let sof = string_of_float
(** = [float_of_string] *)
let fos = float_of_string

(** = [string_of_int] *)
let soi = string_of_int
(** = [int_of_string] *)
let ios = int_of_string

(** = [int_of_float] *)
let iof = int_of_float
(** = [float_of_int] *)
let foi = float_of_int

(** The mathematical constant pi  *)
let pi = 4.0 *. atan 1.0  

(** = [Array.of_list] *)
let aol = Array.of_list
(** = [Array.to_list] *)
let loa = Array.to_list

let random_element l =
    List.nth l (Random.int (List.length l))

let min_of_int_list (l:int list) = 
  let min = ref (List.nth l 0) in 
  for i = 1 to List.length l - 1 do 
    let curr = List.nth l i in
    if curr < !min then 
      min := curr
  done;
  !min
    

let max_of_int_list (l:int list) = 
  let max = ref (List.nth l 0) in 
  for i = 1 to List.length l - 1 do 
    let curr = List.nth l i in
    if curr > !max then 
      max := curr
  done;
  !max


let afindi pred arr = 
  Array.iteri (fun i x -> if pred x then raise (Found i)) arr;
  raise Not_found

let lfindi pred lis = 
  List.iteri (fun i x -> if pred x then raise (Found i)) lis;
  raise Not_found

let aindex elt arr = 
try
  afindi (fun x -> x = elt) arr
with Found i -> i | Not_found -> failwith "ERROR in function aindex (toolkit.ml): index not found!!"  

let lindex elt lis = 
try lfindi (fun x -> x = elt) lis
with Found i -> i | Not_found -> failwith "ERROR in function lindex (toolkit.ml): index not found!!"  

(** Convert a [char] to a [string] *)
let string_of_char = String.make 1

(** = [string_of_char] *)
let soc  = string_of_char

(** Get a string representation of a float, but without the dot if it's an integer *)
let soff f = 
  let i = int_of_float f in
  if float_of_int i -. f = 0. then soi i else sof f
  

(**
 * Two versions in each direction -- one to deal with decimal strings,
 * and the other to deal with decimal integers.  Binary numbers will
 * always be strings 
 *)

let binStr_of_decInt i =
  let rec strip_bits i s =
    match i with
      0 -> s
    | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) in
  strip_bits i ""

let binStr_of_decStr i =
  let rec strip_bits i s =
    match i with
      0 -> s
    | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) in  
  strip_bits (int_of_string i) "";;

let decInt_of_binStr s =
  int_of_string ("0b" ^ s)

let decStr_of_binStr s = 
  string_of_int (int_of_string ("0b" ^ s))
 
let logB base x = (log x) /. (log base)


let divisible (x:int) (y:int) = 
  (x mod y = 0) 

(** Get length of a string *)
let strlen   = String.length
(** Get length of a list *)
let listlen  = List.length
(** Get length of an array *)
let arrlen   = Array.length
(** Get the range of an element in an array *)
let findi p xs = 
  let n = arrlen xs in
  let rec loop i =
    if i = n then raise Not_found
    else if p xs.(i) then i
    else loop (succ i)
  in
  loop 0

  
(** Get a pretty-printing of a list in a general way *)
let pretty_print_list tostr sep dl dr l = 
  let rec f = function
  | a::b::l ->  (tostr a) ^ sep ^ f (b::l) 
  | a::[] -> tostr a
  | [] -> ""
  in dl ^ (f l) ^ dr
  
(** Get a string representation of a list of strings *)
let string_of_string_list =
  let quote s = "\"" ^ s ^ "\"" in
  pretty_print_list quote "; " "[" "]"
  
let string_of_string_list_bis =
  let quote s = "\"" ^ s ^ "\"" in
  pretty_print_list quote "; " "[|" "|]"

let string_of_string_set =
  let quote s = "\"" ^ s ^ "\"" in
  pretty_print_list quote ", " "{" "}"
  
(** Get a string representation of a list of floats *)
let string_of_float_list =
  pretty_print_list sof ", " "(" ")"
  
let string_of_float_list_bis =
  pretty_print_list sof "; " "[|" "|]"
  
(** Get a string representation of a list of ints *)
let string_of_int_list =
  pretty_print_list soi ", " "(" ")"
  
let string_of_int_list_bis =
  pretty_print_list soi "; " "[" "]"
    
(** Get a string representation of an array of floats *)
let string_of_float_array =
  (pretty_print_list sof "; " "[" "]") % loa
  
(** Get a string representation of an array of ints *)
let string_of_int_array =
  (pretty_print_list soi "; " "[" "]") % loa
  
(** Get a string representation of a complex number *)
let string_of_complex z = (sof z.Complex.re) ^ " + i* " ^ (sof z.Complex.im)
  
(** Get a string representation of an array of complex numbers *)
let string_of_complex_array =
  (pretty_print_list string_of_complex "; " "[" "]") % loa
  
(** Get a string representation of an array of strings *)
let string_of_string_array =
  let quote s = "\"" ^ s ^ "\"" in
  (pretty_print_list quote "; " "[" "]") % loa
  
(** DEBUG: display codes for each char in a string *)
let break_string s = let n = strlen s in
  for i = 0 to n - 1 do pf "%d " (int_of_char s.[i]) done
   
(** Get a new hashtable from an association list,
 optionally with some extra space for new values *)  
let init_table ?(extra=0) init =  
  let tbl = H.create (extra + listlen init) in
  List.iter (fun (key, data) -> H.add tbl key data) init;
  tbl
  
let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let thd3 (_, _, z) = z


let fst4 (a, _, _, _) = a
let snd4 (_, b, _, _) = b
let thd4 (_, _, c, _) = c
let fth4 (_, _, _, d) = d

(** {7 standard channels} *)

(** [stdin] as input channel *)
let stdinc  = Pervasives.stdin 
(** [stdout]  as output channel *)
let stdoutc = Pervasives.stdout
(** [stderr]  as output channel *)
let stderrc = Pervasives.stderr
  
(** [stdin] as a file descriptor *)
let stdinf  = Unix.stdin
(** [stdout] as a file descriptor *)
let stdoutf = Unix.stdout
  
(** {6 Unix tricks and sockets} *)

(** Is this filename that of a directory ? This function is supposed to be included in [Sys], but
is not, so I've recoded it here *)
let is_directory f = 
  let s = Unix.stat f in
  s.Unix.st_kind = Unix.S_DIR

(** OS-dependant path separator *)
let path_separator = match Sys.os_type with
  | "Unix" -> "/"
  | _ -> "\\"

(** = [Unix.handle_unix_error] *)
let hue = Unix.handle_unix_error
  
(** Open for writing at end of file *)
let open_out_append path = 
  open_out_gen [Open_wronly; Open_creat ; Open_append; Open_text] 0o666 path


(** DEBUG: quickly display what processus I'm in *)
let pidd s = pf "I am %s (%d)\n%!" (String.uppercase s) (Unix.getpid())
  
(** Setup a pipe with channels instead of file_descr. Optionally specify if reading is non-blocking. Blocking by default. *)
let cpipe ?(nonblock=false) () = 
  let (rdfd, wrfd) = Unix.pipe () in
  if nonblock then Unix.set_nonblock rdfd;
  let rd = Unix.in_channel_of_descr rdfd in
  let wr = Unix.out_channel_of_descr wrfd in
  (rd, wr)
    
(**
  Piped double fork, leaving no zombie process behind.
 @param nb is reading non-blocking ?
 @param f  function the son will be executing. It takes two arguments: the channels reading from the son and writing to the son, respectively.
 @return couple of channels [(channel reading from the son, channel writing to the son)]
*)
let pdfork ?(nb=false) f = 
  let getchans() = cpipe ~nonblock:nb () in
  let srd, dwr = getchans() and drd, swr = getchans() in (* crossed pipes *)
  let close_dad ()= close_in drd; close_out dwr   in (* close the ends of the dad *)
  let close_son ()= close_in srd; close_out swr   in (* close the ends of the son *)
    match Unix.fork() with
      | 0 -> (if Unix.fork() <> 0 then () else begin close_dad(); f srd swr end); exit 0
      | n ->  ignore (Unix.waitpid [] n) ; close_son(); drd, dwr
  
(** Do an [input_line] for non-blocking channels. [None] is returned if nothing is there. *)
let peek_line ic = try Some(input_line ic) with _ -> None
  
(** Display a human-legible internet address in IP:port format*)
let string_of_sockaddr = function
  | Unix.ADDR_INET (a_ip, p) -> let ip = Unix.string_of_inet_addr a_ip in va "%s:%d" ip p
  | _ -> "???"
      
(** Build a string by repeating the same pattern n times. Useful for drawing terminal lines etc *)
let rec repeat_pattern s = function
  | n when n <= 0 -> ""
  | n -> s^(repeat_pattern s (n-1))
    

(** {6 Command-line} *)
 
(** Array of command-line arguments *)
let av = Sys.argv
(** Number of command-line arguments *)
let ac = Array.length av
(** List of command-line arguments *)
let al = Array.to_list av    

(** Incorrect command-line arguments routine: prints all arguments, insults user, and fails.  *)
let bad_args onarg = begin 
  if onarg <> "" then epf "Failed on argument `%s'!\n" onarg;
  epf "Incorrect runtime arguments ! Run '%s --help' to see help.\n\n" av.(0);  
  let i = ref 0 in Array.iter (function x -> (epf "argument %d: <%s>\n" !i x ; i := !i+1)) av ; 
end



(** {6 Other modules} *)

(** 
  Disjoint Set data structure. 

  A {i very} fast data structure to deal with the partitionning of a set into
  several, non-overlapping subsets. Can be seen as an equivalence relation.
  
  @author Vincent Hugot
*)
module DisjointSet = struct
  
  (** Internal record of rank and parent *)
  type 'a record = { mutable parent : 'a; mutable rank : int }
  
  (** Type of disjoint set of ['a] *)
  type 'a t = ('a, 'a record) H.t
  
  (** Create a new, empty disjoint set *)
  let create : int -> 'a t = H.create
  
  (** Add a new element in the disjoint set, originally on its own *)
  let add (s : 'a t) x = 
    if H.mem s x then failwith "DisjointSet: add(x) where x already in set";
    H.add s x {parent = x; rank = 0}
  
  (** Find the "representant" for a given element. Two elements are in the same subset iff
    their representants are the same *)
  let rec find (s : 'a t) x = 
    try let _x = H.find s x in
    if _x.parent = x then x
    else (_x.parent <- find s _x.parent ; _x.parent)
    with Not_found -> failwith "DisjointSet: find(x) where x not in set"
  
  (** Declare that elements [x] and [y] are in the same subset *)
  let union s x y = 
    let root_x, root_y = find s x, find s y in
    let _rx, _ry = H.find s root_x, H.find s root_y in
    if _rx.rank > _ry.rank then _ry.parent <- root_x
    else if _rx.rank < _ry.rank then _rx.parent <- root_y
    else if root_x <> root_y then (
      _ry.parent <- root_x; 
      _rx.rank <- succ _rx.rank )
      
  (** Declare that all elements of the list are in the same subset *)
  let union_l s = function x :: l ->
    List.iter (fun y-> union s x y) l | [] -> ()
    
  (** Determine whether two elements are in the same subset *)
  let equivalent s x y = find s x = find s y
  
  (** Number of elements in the set *)
  let cardinal (s : 'a t) = H.length s
  
  (** Get the list of elements in the set *)
  let elements (s : 'a t) = remove_duplicates @@ hkeys s
  
  (** Get the list of partitions of the set, in no particular order *)
  let export (s : 'a t) = 
    let h = H.create (cardinal s) in
    let add x _ = H.add h (find s x) x in
    H.iter add s;
    let reps = remove_duplicates @@ hkeys h in
    List.map (H.find_all h) reps
    
  (** Get a pretty string representation for the set *)
  let to_string s = 
    let pr = pretty_print_list string_of_int_list "; " "#{" "}#" in
    pr @@ export s
    
  (** Get a string representation of all the internals: tree structure and rank *)
  let internal_to_string atos (s : 'a t) = 
    let pr x {parent = p; rank = n}  = va "%s <-- %s [r=%2d]\n" (atos x) (atos p) n in 
    let b = Buffer.create 1024 in let write x r = Buffer.add_string b @@ pr x r in
    H.iter write s; Buffer.contents b
  

end


  
