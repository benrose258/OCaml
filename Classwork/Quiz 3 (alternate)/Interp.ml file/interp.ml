(* Ben Rose *)
(* Quiz 3 *)
(* 3-11-2021 *)
(* Defined the "max" function below *)

open Ast
open Ds

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Max(e1,e2) ->
    (* Evaluate the given expression e1 *)
    eval_expr e1 >>=
    (* Bind (that's what >>= is) e1 to the integer n1 *)
    int_of_numVal >>= fun n1 ->
    (* Evaluate the given expression e2 *)
    eval_expr e2 >>=
    (* Bind e2 to the integer n2 *)
    int_of_numVal >>= fun n2 ->
    (* If n2 is greater than n1 *)
    if (n2 > n1)
    (* Then return n2 *)
    then return (NumVal (n2))
    (* Else, n1 >= n2, so return n1 *)
    else return (NumVal (n1))
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str;
    error "Debug called"
  | _ -> error "Not implemented yet!"



(** [parse s] parses string [s] into an ast *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_expr
  in run c
