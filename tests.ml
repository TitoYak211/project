(*
       CS 51 Final Project
      MiniML -- Expressions
           Spring 2019
*)

open Expr;;
open Evaluation;;

let abs_expr1 = Let("f", Fun("x", Binop(Plus, Var("x"), Var("x"))), App(Var("f"), App(Var("f"), Num(3))));; 
let abs_string_expr1 = "Let(f, Fun(x, Binop(+, Var(x), Var(x))), App(Var(f), App(Var(f), Num(3))))";;
let input2 = "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4 ;;";;
let abs_expr2 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1), Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),App(Var("f"), Num(4))) ;;
let abs_string_expr2 = "Letrec(f, Fun(x, Conditional(Binop(=, Var(x), Num(0)), Num(1), Binop(*, Var(x), App(Var(f), Binop(-, Var(x), Num(1)))))), App(Var(f), Num(4)))";;
let abs_expr3 = Fun("y", App(Var("f"), Binop(Plus, Var("x"), Var("y"))));;
let abs_expr4 = Let("double", Fun("x", Binop(Plus, Var("x"), Var("x"))),
                App(Var("double"), Num(4)));;
let abs_expr5 = Letrec("f", Fun("n", Conditional(Binop(Equals, Var("n"),
               Num(0)), Num(1), Binop( Times, Var("n"), App(Var("f"), 
               Binop(Minus, Var("n"), Num(1)))))), App(Var("f"), Num(3))) ;;
let abs_expr6 =  Fun("n", Conditional(Binop(Equals, Var("n"),
               Num(0)), Num(1), Binop( Times, Var("n"), App(Var("f"), 
               Binop(Minus, Var("n"), Num(1))))));;
let abs_expr7 = Let("f", Fun("x", Binop(Plus, Var("x"), Var("x"))),
                App(App(App(Var("f"),Var("f")), Var("f")), Num(3))) ;;
let abs_expr8 = Letrec("f", Fun("n", Conditional(Binop(Equals, Var("n"),
                Num(10)), Var("n"), Binop(Plus, Var("n"), App(Var("f"),
                Binop(Plus, Var("n"), Num(1)))))), App(Var("f"), Num(0))) ;; 
let abs_expr9 = Let("x", Var("y"), Let("y", Num(3), Binop(Plus, Var("x"), 
                Var("x"))));;
let abs_expr10 = Let("y", Num(3), Binop(Plus, Var("x"), 
                Var("x")));;
let abs_expr11 = Let("z", Num(1), Let("f", Fun("y", Binop(Plus, Var("z"),
                 Var("y"))), Let("z", Num(2), App(Var("f"), Num(1)))));;
let abs_expr12 = Let("twice", Fun("f", Fun("y", App(Var("f"), App(Var("f"),
                 Var("y"))))), Let("square", Fun("x", Binop(Times, Var("x"),
                 Var("x"))), App(App(Var("twice"), Var("square")), Num(3))));;

let test_exp_to_abstract_string () =
  assert(exp_to_abstract_string abs_expr1 = abs_string_expr1);
  assert(exp_to_abstract_string abs_expr2 = abs_string_expr2);;
  
let test_free_vars () =
  assert(free_vars abs_expr1 = vars_of_list []);
  assert(free_vars abs_expr2 = vars_of_list []);
  assert(free_vars abs_expr3 = vars_of_list ["f"; "x"]);
  assert(free_vars abs_expr4 = vars_of_list []);
  assert(free_vars abs_expr6 = vars_of_list ["f"]);
  assert(free_vars abs_expr10 = vars_of_list ["x"]);;
let test_subst () =
  assert(subst "x" (Var "z") abs_expr3 = 
   Fun("y", App(Var("f"), Binop(Plus, Var("z"), Var("y")))));
  assert(subst "x" (Binop(Plus, Var "u", Var "v")) abs_expr3 = 
   Fun("y", App(Var("f"), Binop(Plus, Binop(Plus, Var "u", Var "v"),
   Var("y")))));
  assert(subst "f" (Fun("u", Binop(Plus, Var "u", Var "u"))) abs_expr3 = 
         Fun("y", App((Fun("u", Binop(Plus, Var "u", Var "u"))),
         Binop(Plus, Var("x"), Var("y")))));
  assert(subst "x" (Num 3) abs_expr4 = abs_expr4);;

let test_lookextend () =
  let env = Env.create() in
  let env = Env.extend env "x" (ref (Env.Val (Num 5))) in
  assert(Env.lookup env "x" = Env.Val (Num 5));
  let env = Env.extend env "x" (ref (Env.Val (Num 10))) in
  assert(Env.lookup env "x" = Env.Val (Num 10));;

let test_eval_s () =
  assert(eval_s abs_expr1 (Env.create ()) = Env.Val (Num 12));
  assert(eval_s abs_expr4 (Env.create ()) = Env.Val (Num 8));
  assert(eval_s abs_expr5  (Env.create ()) = Env.Val (Num 6));
  assert(eval_s abs_expr8 (Env.create ()) = Env.Val (Num 55));
  assert(eval_s abs_expr9 (Env.create ()) = Env.Val (Num 6));
  assert(eval_s abs_expr12 (Env.create ()) = Env.Val (Num 81));;

let test_eval_d () =
  assert(eval_d abs_expr1 (Env.create()) = Env.Val (Num 12));
  assert(eval_d abs_expr4 (Env.create()) = Env.Val (Num 8));
  assert(eval_d abs_expr5 (Env.create()) = Env.Val (Num 6));
  assert(eval_d abs_expr8 (Env.create()) = Env.Val (Num 55));
  assert(eval_d abs_expr9 (Env.create()) = Env.Val (Num 6));;

let test_eval_l () =
  assert(eval_l abs_expr1 (Env.create()) = Env.Val (Num 12));
  assert(eval_l abs_expr4 (Env.create()) = Env.Val (Num 8));
  assert(eval_l abs_expr5 (Env.create()) = Env.Val (Num 6));
  assert(eval_l abs_expr8 (Env.create()) = Env.Val (Num 55));
  assert(eval_l abs_expr9 (Env.create()) = Env.Val (Num 6));
  assert(eval_l abs_expr11 (Env.create()) = Env.Val (Num 3));
  assert(eval_l abs_expr12 (Env.create()) = Env.Val (Num 81));;

let run_tests () =
  test_exp_to_abstract_string ();
  test_free_vars ();
  test_lookextend (); 
  test_subst ();
  test_eval_s ();
  test_eval_d ();
  test_eval_l ();;
run_tests ();;

print_string "all tests passed ";;