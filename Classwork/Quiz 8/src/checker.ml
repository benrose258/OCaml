(* Ben Rose
Quiz 8
Exercise 4.1.7 from the notes: implementing the type checker for records
I pledge my honor that I have abided by the Stevens Honor System. *)

open Ast
open ReM
open Dst

let rec chk_expr : expr -> texpr tea_result = fun e ->
  match e with
  | Int(_n) -> return IntType
  | Var(id) -> apply_tenv id
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2) | Div(e1,e2)->
    chk_expr e1 >>= fun te1 ->
    chk_expr e2 >>= fun te2 ->
    if te1=IntType && te2=IntType
    then return IntType
    else error "binop: arguments not ints"
  | IsZero(e) ->
    chk_expr e >>= fun te ->
    if te=IntType
    then return BoolType
    else error "zero?: argument not an int"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun te1 ->
    if te1=BoolType
    then (chk_expr e2 >>= fun te2 ->
          chk_expr e3 >>= fun te3 ->
          if te2=te3
          then return te2
          else error "ite: both branches must have same type")
      else error "ite: condition not a bool"
  | Let(id,e1,e2) ->
    chk_expr e1 >>=
    extend_tenv id >>+
    chk_expr e2
  | Proc(id,t1,e)  ->
    extend_tenv id t1 >>+
    chk_expr e >>= fun tRange ->
    return (FuncType(t1,tRange))
  | App(e1,e2)  ->
    chk_expr e1 >>= fun te1 ->
    chk_expr e2 >>= fun te2 ->
    (match te1 with
     | FuncType(l,r) ->
       (if l=te2
        then return r
        else error "app: argument type incorrect")
     | _ -> error "app: LHS is not a function")
  | Tuple(es) ->
    sequence (List.map chk_expr es) >>= fun ts ->
    return (TupleType(ts))
  | Untuple(ids,def,target) ->
    chk_expr e >>=
    list_of_tupleType >>= fun ts ->
    if List.length ids=List.length ts
    then
      extend_list_tenv ids ts >>+
      chk_expr target
    else error "untuple: incorrect length for ids"
  (* Quiz 8: Record and Projection *)
  | Record(es) ->
    return (RecordType (
    let rec check_expression_records list_of_records final_list =
      (match list_of_records with
        | [] -> final_list
        | list_head::list_tail ->
          (match list_head with
            | (record_name, record_id) ->
              (record_name (chk_expr record_id))::final_list
            ))
    in es []
    ))
  | Proj(e, id) ->
    chk_expr e >>=
    list_of_recordType >>= fun ts ->
    (let rec find_id records_to_search id_to_find =
      (match records_to_search with
        | [] -> error "Projection: ID not found"
        | list_head::list_tail ->
          (match list_head with
            | (record_id, record_data) ->
              if (record_id = id_to_find)
              then (
                chk_expr record_data >>= fun record_data_type ->
                return record_data_type
                )
              else (find_id list_tail id_to_find)
        )
      )
    in find_id ts id
    )

  | Debug(_e) ->
     string_of_tenv >>= fun str_env ->
      (print_endline str_env;
       error "Reached breakpoint")
  | _ -> error ("Not implemented: "^string_of_expr e)

(* Parse a string into an ast *)

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf


(* Interpret an expression *)
let chk (s:string)  =
  let c = s |> parse |> chk_expr
  in
  (match runt c with
  | Error s -> s
  | Ok t -> string_of_texpr t)
