(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)
    
open Expr ;;
  
(* Exception for eżaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime, generated by an explicit "raise" construct *)
exception EvalException ;;


(*......................................................................
  Environments and values 
 *)

module type Env_type = sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value =
    Closure (exp, env)
    ;;

    (* Looks up the value of a variable in the environment *)
    let lookup (env : env) (varname : varid) : value =
      try  
        let (_, valref) = List.find (fun (id, _) -> id = varname) env in
        !valref
      with
      Not_found -> raise (EvalError ("unbound variable " ^ varname))
    ;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      try 
        let _ = lookup env varname in
        List.map (fun (id, valref) -> 
            if id = varname then id, loc else id, valref) env
      with
        EvalError _ -> (varname, loc) :: env
    ;;

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
    match v with 
    | Closure(exp, env) -> 
        if printenvp then
          "value: " ^ (exp_to_concrete_string exp) ^ "; [ " ^ (List.fold_right 
          (fun (id, valref) c -> id ^ (value_to_string !valref) ^ c ) 
          env "") ^ "]"
        else exp_to_concrete_string exp 
    | Val e -> exp_to_concrete_string e 
    ;;

    (* Returns a printable string representation of an environment *)
    let env_to_string (env : env) : string =
      "[" ^ (List.fold_right (fun (id, valref) c -> "( " ^ id ^ ") ; ( " ^
      (value_to_string !valref ^ ")") ^ c ) env "") ;;
  end
;;

(*......................................................................
  Evaluation functions

  Each of the evaluation functions below, evaluates an expression exp
  in an enviornment env returning a result of type value. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a value and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type expr -> Env.env -> Env.value for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as eval_e below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)

(* helper function to evaluate binops *)
let eval_binop (b : binop) (e1 : expr) (e2 : expr) : expr =
  match b, e1, e2 with
  | Plus, Num n1, Num n2 -> Num (n1 + n2)
  | Minus, Num n1, Num n2 -> Num (n1 - n2)
  | Times, Num n1, Num n2 -> Num (n1 * n2)
  | Equals, e1, e2 -> Bool (e1 = e2)
  | LessThan, e1, e2 -> Bool (e1 < e2)
  | _ -> raise (EvalError "Binop used on incorrect types");;

(* helper function for implementing eval_s *)
let rec h_evaluate (exp : expr) : expr = 
  match exp with 
    | Var _ -> raise (EvalError "unbound variable")
    | Bool b -> Bool b
    | Unop(_, xp) -> 
        (match (h_evaluate xp) with
        | Num n -> Num (~- n)
        | _ -> raise (EvalError "attempted to negate a non-integer") )
    | Binop(b, e1, e2) -> eval_binop b (h_evaluate e1) (h_evaluate e2) 
    | Conditional(e1, e2, e3) -> 
        (match h_evaluate e1 with 
        | Bool true -> h_evaluate e2
        | Bool false -> h_evaluate e3
        | _ -> raise (EvalError ": is not of type bool ")) 
    (* if we find a unapplied function, just reuturn it as utop does *)
    | Fun(id, xp) -> Fun(id, xp)
    | Let(id, xp1, xp2)-> 
        (* first case covers aliasing, i.e let x = y in let y = ... *)
        (match xp2 with 
        | Let(id2, def, body) -> 
            if id = id2 then h_evaluate (Let(id, def, body))
            else h_evaluate (Let(id2, subst id xp1 def, subst id xp1 body))
        | _ -> h_evaluate (subst id xp1 xp2))
    | Letrec(id, recfun, xp2) -> 
        let (recfun_id, recfun_body) = 
          (match recfun with
          | Fun(recfun_id, recfun_body) -> recfun_id, recfun_body
          | _ -> raise (EvalError "cannot use letrec with a non function")) in
        let newvar = new_varname () in
        let newrecfun = Fun(newvar, subst recfun_id (Var newvar) recfun_body) in
        let newrec = subst id (Letrec(id, newrecfun, Var id)) recfun in
        h_evaluate (subst id newrec xp2)
    | Raise -> raise EvalException
    | Unassigned -> raise (EvalError "Unassigned variable")
    | App (f, xp1) ->
        (match h_evaluate f, h_evaluate xp1 with
        | (Fun(id, body), xp) -> 
        h_evaluate (subst id (h_evaluate xp) body )
        | _ -> raise (EvalError "this is not a function it cannot be applied"))
    | Num n -> Num n ;;

(* The substitution evaluator *)
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  Env.Val (h_evaluate exp) ;;
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)

(* helper function to find find value of var in env *)
let replace id env : expr = 
  match Env.lookup env id with
  | Env.Val(expr) -> expr
  | Env.Closure(expr, _) -> expr ;;

(* helper function for dynamically_scoped environment model evaluator *)
let rec heval_d (exp : expr) (env : Env.env): expr =
  match exp with 
  | Var id -> heval_d (replace id env) env
  | Bool b -> Bool b
  | Unop(_, xp) -> 
      (match (heval_d xp env) with
      | Num n -> Num (~- n)
      | _ -> raise (EvalError "attempted to negate a non-integer"))
  | Binop(b, xp1, xp2) -> eval_binop b (heval_d xp1 env) (heval_d xp2 env) 
  | Conditional(xp1, xp2, xp3) ->
      (match heval_d xp1 env with 
      | Bool true -> heval_d xp2 env
      | Bool false -> heval_d xp3 env
      | _ -> raise (EvalError (exp_to_concrete_string xp1 ^": is not of type bool "))) 
  | Fun(id, xp) -> Fun(id, xp)
  | Let(id, xp1, xp2)-> heval_d xp2 (Env.extend env id (ref (Env.Val xp1))) 
  | Letrec(id, recfun, xp2) ->
      let eval_recfun = (heval_d recfun (Env.extend env id (ref 
                        (Env.Val Unassigned)))) in
      heval_d xp2 (Env.extend env id (ref (Env.Val eval_recfun)))
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "Unassigned variable")
  | App(f, xp1) ->
      (match heval_d f env, heval_d xp1 env with
        | (Fun(id, body), xp) ->
        heval_d body (Env.extend env id (ref (Env.Val xp)))  
        | _ -> raise (EvalError "this is not a function it cannot be applied"))
  | Num n -> Num n;;

(* dynamically_scoped environment model evaluator *)
let eval_d (exp : expr) (_env : Env.env) : Env.value =
    Env.Val (h_evaluate exp) ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)

let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  let rec heval_l (inval : Env.value) (env : Env.env) : Env.value =   
    let exp =
      match inval with 
      | Env.Val xp -> xp
      | Env.Closure(xp, _) -> xp in
    match exp with 
    | Var id -> heval_l (Env.Val( replace id env)) env
    | Bool b -> (Env.Val (Bool b))
    | Unop(_, xp) -> 
        (match (heval_l (Env.Val xp) env) with
        | Env.Val(Num n) -> (Env.Val (Num (~- n)))
        | _ -> raise (EvalError "attempted to negate a non-integer"))
    | Binop(b, xp1, xp2) ->
        (match (heval_l (Env.Val xp1) env), (heval_l (Env.Val xp2) env) with
        | Env.Val xp1, Env.Val xp2 ->
              Env.Val (eval_binop b xp1 xp2)
        | _ -> raise (EvalError "Incompatabile types for binop"))
    | Conditional(xp1, xp2, xp3) ->
        (match heval_l (Env.Val xp1) env with 
        | Env.Val(Bool true) -> heval_l (Env.Val xp2) env
        | Env.Val(Bool false) -> heval_l (Env.Val xp3) env
        | _ -> raise (EvalError (exp_to_concrete_string xp1 ^": is not of type bool ")))
    | Fun(id, xp) -> (Env.close (Fun(id, xp)) env)
    | Let(id, xp1, xp2)->
          heval_l (Env.Val xp2) (Env.extend env id (ref (Env.close xp1 env)))  
    | Letrec(id, recfun, xp2) ->
        let eval_recfun = (eval_l recfun (Env.extend env id
                          (ref (Env.Val Unassigned)))) in
         (eval_l xp2  (Env.extend env id (ref eval_recfun)))
    | Raise -> raise EvalException
    | Unassigned -> raise (EvalError "Unassigned variable")
    | App(xp1, xp2) ->
        (match heval_l (Env.Val xp1) env with
        | Env.Closure(Fun(id, body), c_env ) ->
            heval_l (Env.Val body) (Env.extend c_env id 
            (ref (heval_l (Env.Val xp2) env)))
        | _ -> raise (EvalError "this is not a function it cannot be applied"))
    | Num n -> (Env.Val(Num n)) in
  match heval_l (Env.Val exp) env with
  | Env.Val exp -> Env.Val exp
  | Env.Closure (exp, _) -> Env.Val exp;;

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within eval_s, eval_d, or eval_l. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   miniml.ml uses a call to the single function evaluate defined
   here. Initially, evaluate is the trivial evaluator eval_t. But you
   can define it to use any of the other evaluators as you proceed to
   implement them. (We will directly unit test the four evaluators
   above, not the evaluate function, so it doesn't matter how it's set
   when you submit your solution.) *)

let evaluate = eval_l ;;
