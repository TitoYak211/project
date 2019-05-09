(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let free_vars (exp : expr) : varidset =
  let rec all_vars (xp : expr) (all : varidset) (used : varidset)
          : (varidset * varidset) =
    (match xp with
    | Var id -> SS.add id all, used
    | Num _ -> all, used
    | Bool _ -> all, used
    | Unop(_, xp1) -> all_vars xp1 all used 
    | Binop(_, xp1, xp2) ->
        let lall, lused = all_vars xp1 all used in
        let rall, rused = all_vars xp2 all used in
        (SS.union lall rall), (SS.union lused rused) 
    | Conditional(xp1, xp2, xp3) -> 
        let lall, lused = all_vars xp1 all used in
        let mall, mused = all_vars xp2 all used in
        let rall, rused = all_vars xp3 all used in
        (SS.union (SS.union lall mall) rall),
        (SS.union (SS.union lused mused) rused) 
    | Fun(id, xp1) -> all_vars xp1 all (SS.add id used)
    | Let(id, xp1, xp2) -> 
        let nused = SS.add id used in
        let lall, lused = all_vars xp1 all nused in
        let rall, rused = all_vars xp2 all nused in
        (SS.union lall rall), (SS.union lused rused)
    | Letrec(id, xp1, xp2) -> 
        let nused = SS.add id used in
        let lall, lused = all_vars xp1 all nused in
        let rall, rused = all_vars xp2 all nused in
        (SS.union lall rall), (SS.union lused rused)
    | Raise -> all, used
    | Unassigned -> all, used
    | App(xp1, xp2) ->
        let lall, lused = all_vars xp1 all used in
        let rall, rused = all_vars xp2 all used in
        (SS.union lall rall), (SS.union lused rused)) in 
  let all, used = all_vars exp SS.empty SS.empty in
  let free = SS.filter (fun x -> not (SS.mem x used)) all in free
;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  let counter = ref 0 in
    let v = "x" ^ string_of_int (!counter) in
      counter := !counter + 1;
  v ;;  
(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  (* use a helper function to keep track of free variables *)
  let free_var_set = free_vars exp in
  let rec subst_helper (var_name : varid) (repl : expr) (exp : expr) : expr =
    let part_subst = subst_helper var_name repl in
    match exp with
    | Var id -> if id = var_name && SS.mem id free_var_set then repl else Var id
    | Unop(u, xp1) -> Unop(u, part_subst xp1)
    | Binop(b, xp1, xp2) -> Binop(b, part_subst xp1, part_subst xp2 )
    | Conditional (xp1, xp2, xp3) ->
        Conditional (part_subst xp1, part_subst xp2, part_subst xp3)
    | Fun (id, xp1) -> 
        if id = var_name then Fun(id, xp1)
        else if not (SS.mem id (free_vars repl))
        then Fun(id, part_subst xp1)
        else let nvar = new_varname () in
        Fun(nvar, subst_helper id (Var nvar) (part_subst xp1))
    | Let (id, xp1, xp2) -> 
        if id = var_name 
        then Let(id, part_subst xp1, xp2)
        else if id <> var_name && not (SS.mem id (free_vars repl))  
        then Let(id, part_subst xp1, part_subst xp2)
        else let nvar = new_varname () in 
          Let(nvar, part_subst xp1, subst_helper id (Var nvar) (part_subst xp2))
    | Letrec (id, xp1, xp2) ->
        if id = var_name 
        then Letrec(id, part_subst xp1, part_subst xp2)
        else if id <> var_name && not (SS.mem id (free_vars repl)) 
        then Letrec(id, part_subst xp1, part_subst xp2) 
        else let nvar = new_varname () in
        Letrec(id, subst var_name repl xp1 , subst var_name repl 
        ((subst id (Var nvar) xp2))) 
    | App(xp1, xp2) -> App (part_subst xp1, part_subst xp2)
    | Raise -> Raise
    | Unassigned -> Unassigned  
    | Num n -> Num n
    | Bool b -> Bool b in
  subst_helper var_name repl exp
;;

let binop_to_abs_string (b : binop) : string =
  match b with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<"
;;

(*using match statement so that this is easier to extend later *)
let unop_to_abs_string (u : unop) : string =
    match u with
    | Negate -> "~" ;;

(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  (match exp with
  | Var id -> id
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop(u, xp1) -> unop_to_abs_string u ^ (exp_to_concrete_string xp1)
  | Binop(b, xp1, xp2) -> 
      exp_to_concrete_string xp1 ^ binop_to_abs_string b
      ^ exp_to_concrete_string xp2
  | Conditional(xp1, xp2, xp3) ->
      "if " ^ exp_to_concrete_string xp1 ^ " then " 
      ^ exp_to_concrete_string xp2 ^ " else " ^ exp_to_concrete_string xp3
  | Fun (id, xp) ->   "fun " ^ id ^ " -> " ^ (exp_to_concrete_string xp)
  | Let(id, xp1, xp2) ->
      "let " ^ id ^ " = " ^ exp_to_concrete_string xp1 ^ " in "
      ^ exp_to_concrete_string xp2
  | Letrec(id, xp1, xp2) ->
      "let rec " ^ id ^ " = " ^ exp_to_concrete_string xp1 ^ " in " 
      ^ exp_to_concrete_string xp2 
  | Raise -> "raise"
  | Unassigned -> "Unassigned"
  | App(xp1, xp2) -> exp_to_concrete_string xp1 ^ " " ^ exp_to_concrete_string xp2)
;; 

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var id -> "Var(" ^ id ^ ")"
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Bool b -> "Bool (" ^ string_of_bool b ^ ")"
  | Unop(u, xp) ->
      "(" ^unop_to_abs_string u ^ ", " ^ exp_to_abstract_string xp ^ ")"
  | Binop(b, xp1, xp2) -> 
      "Binop" ^ "(" ^ binop_to_abs_string b ^ ", " ^ exp_to_abstract_string xp1
        ^ ", " ^ exp_to_abstract_string xp2 ^ ")"
  | Conditional(xp1, xp2, xp3) -> 
      "Conditional(" ^ exp_to_abstract_string xp1 ^ ", " ^ 
        exp_to_abstract_string xp2 ^ ", " ^ exp_to_abstract_string xp3 ^ ")"
  | Fun(id, xp1) -> "Fun(" ^ id ^ ", " ^ exp_to_abstract_string xp1 ^")"
  | Let(id, xp1, xp2) -> 
      "Let(" ^ id ^ ", " ^ exp_to_abstract_string xp1 ^ ", " 
      ^ exp_to_abstract_string xp2 ^ ")"
  | Letrec(id, xp1, xp2) ->
      "Letrec(" ^ id ^ ", " ^ exp_to_abstract_string xp1 ^ ", " ^
      exp_to_abstract_string xp2 ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (xp1, xp2) -> 
      "App(" ^ exp_to_abstract_string xp1 ^ ", " ^ exp_to_abstract_string xp2 
      ^ ")"
;;
