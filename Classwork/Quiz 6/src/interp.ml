(* Ben Rose *)
(* Quiz 6 *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

(* This quiz's description comes from Page 58 of the PLaF GitHub notes document. *)

open Ast
open Ds

let g_store = Store.empty_store 20 (NumVal 0)



let rec addIds fs evs =
  match fs,evs with
  | [],[] -> []
  | (id,(is_mutable,_))::t1, v::t2 -> (id,(is_mutable,v)):: addIds t1 t2
  | _,_ -> failwith "error: lists have different sizes"

let rec apply_proc : exp_val -> exp_val -> exp_val ea_result =
  fun f a ->
  match f with
  | ProcVal (id,body,env) ->
    return env >>+
    extend_env id a >>+
    eval_expr body
  | _ -> error "apply_proc: Not a procVal"
and
  eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) -> return @@ NumVal n
  | Var(id) -> apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(v,def,body) ->
    eval_expr def >>=
    extend_env v >>+
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
    return @@ BoolVal (n = 0)
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return @@ PairVal(ev1,ev2)
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ fst p
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ snd p
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Letrec(id,par,e,target) ->
    extend_env_rec id par e >>+
    eval_expr target
  | NewRef(e) ->
    eval_expr e >>= fun ev ->
    return @@ RefVal (Store.new_ref g_store ev)
  | DeRef(e) ->
    eval_expr e >>=
    int_of_refVal >>=
    Store.deref g_store
  | SetRef(e1,e2) ->
    eval_expr e1 >>=
    int_of_refVal >>= fun l ->
    eval_expr e2 >>=
    Store.set_ref g_store l >>= fun _ ->
    return UnitVal
  | BeginEnd([]) ->
    return UnitVal
  | BeginEnd(es) ->
    sequence (List.map eval_expr es) >>= fun vs ->
    return (List.hd (List.rev vs))
  | Record(fs) ->
    sequence (List.map process_field fs) >>= fun evs ->
    return (RecordVal (addIds fs evs))

  (* Exercise 3.5.1 Part 1: Proj *)
  (* Called "Projection".
  This function returns the value of the expressed value at a certain ID.
  If no ID is specified, then just the contents of the record are returned.
  If an ID is specified, the function fetches the contents from the Store (i.e. g_store (i.e. global store))
      and returns it. *)

  (* Overview of variables:
      e: The record that we are provided
      id: The subfield that we want to display *)

  (* Commands to see if it's working right and their results when using utop:

  utop # interp "
  let p = {ssn = 10; age <= 30}
  in begin
      p.age <= 31;
      p.age
    end"
    - : Ok (NumVal 31)

    utop # interp "
    let p = {ssn = 10; age <= 30}
    in begin
      p.age <= 31;
      p
    end"
    - : Ok (RecordVal [("ssn", (false, NumVal 10)); ("age", (true, RefVal 1))])

      *)
  | Proj(e,id) ->
    (* Evaluate the expression e and store its results in myrecord *)
    eval_expr e >>=
    (* Using my record_of_recordVal function,
      convert the expression e into the recordVal object's data (i.e. a list of "(string, (bool, exp_val))"s).
      The record_of_recordVal function will return an error if e1 is not a record. *)
    record_of_recordVal >>= fun myrecord ->
    (* Return the result of executing the following function: *)
      (
      (* Outer helper function: get_projection *)

          (* Helper function: projection_of_record_or_field_of_record:
              Parses the contents of the RecordVal myrecord, which is a list of "(string, (bool, exp_val))"s,
              then, if id == any of the fields, then return the field's exp_val, else return original_myrecord
            Variables: record_to_search, specified_field_to_display, original_myrecord *)
            let rec projection_of_record_or_field_of_record (record_to_search:(string*(bool*exp_val)) list)
                                                            (specified_field_to_display:string)
                                                            (original_myrecord:(string*(bool*exp_val)) list) =
            (* Begin matching on the list record_to_search *)
              match record_to_search with
              (* If record_to_search is empty
                      (i.e. we've searched the whole list for the specified_field_to_display and came up empty),
                    then return the RecordVal original_myrecord *)
              | [] -> return @@ RecordVal(original_myrecord)
              (* Else, record_to_search has at least one more field left to evaluate,
                  so break record_to_search into a head (i.e. first_field) and a tail (i.e. rest_of_the_fields) *)
              | first_field::rest_of_the_fields ->
                (* Begin matching on first_field *)
                match first_field with
                (* For the case where the field has a name (or "id") field_name,
                                                a boolean is_mutable, and a value field_data (i.e. every case) *)
                | (field_name, (is_mutable, field_data)) ->
                  (* If field_name == specified_field_to_display *)
                  if field_name = specified_field_to_display
                  (* Then execute the sub-if statement return field_data *)
                  then (
                    (* If the variable is mutable (i.e. if field_data is a RefVal) *)
                    if is_mutable
                    (* Then get the value of the RefVal field_data from the global store and return that value.
                        This is done by using the deref function in Store.ml on the Global_Store (i.e. g_store)
                        and the RefVal obtained as result of the following helper function and its function call. *)
                    then (
                        int_of_expRefVal field_data >>= fun first_field_data_reference_number ->
                        (
                          let call_dereference (reference_no:int) =
                            Store.deref g_store reference_no
                          in call_dereference first_field_data_reference_number
                          )
                      )
                    (* Else the value of the field is static, so return field_data *)
                    else return @@ field_data
                  )
                  (* Else, we have more fields to look at, so recursively search through the rest_of_the_fields *)
                  else projection_of_record_or_field_of_record rest_of_the_fields specified_field_to_display original_myrecord
            (* Call projection_of_record_or_field_of_record using
                                  the record "myrecord", the string "specified_field_to_display", and the record "myrecord". *)
            in projection_of_record_or_field_of_record myrecord id myrecord
        (* Call get_projection with the RecordVal myrecord and the string "id" *)
      )

  (* Exercise 3.5.1 Part 2: SetField *)
  (* SetField preliminary guess definition from "For example," context clues:
      Sets the subfield of a variable if the field is mutable. *)

  (* Overview of variables:
      e1: The record that has the subfield we will edit
      id: The subfield name that we're going to edit
      e2: The new value of the subfield *)
  | SetField(e1,id,e2) ->
  (* Strategy: Each record is a list, so maybe just use a helper function to go through the list
              and if name of the field you're looking at == id, check if it can be changed.
                                                              If it can, alter the value of it to be the data in e2.
                                                              Else, throw the error listed in the Exercise Spec.
              If the name of the field != id, then just add that field as it currently is to the final record.
          Return the resulting RecordVal. *)
    (* Evaluate the expression e1 *)
    eval_expr e1 >>=
    (* Using my record_of_recordVal function,
      convert the expression e into the recordVal object's data (i.e. a list of "(string, (bool, exp_val))"s).
      The record_of_recordVal function will return an error if e1 is not a record. *)
    record_of_recordVal >>= fun myrecord ->
    (* Evaluate the expression e2 and store the result in new_field_value *)
    eval_expr e2 >>= fun new_field_value ->
    (* Return the RecordVal that will result from calling the following helper function: *)
    return @@ RecordVal (
        (* The function alter_recordVal takes in the recordVal specified in e1, the name of the subfield that we're altering,
                                        and the new value for the subfield specified in e2
                              and outputs the modified record with the specified modifications. *)
        let rec alter_recordVal (recordVal_to_alter:(string*(bool*exp_val)) list) (original_myrecord:(string*(bool*exp_val)) list)
                                (name_of_field_to_be_altered:string) (new_field_data:exp_val) : (string*(bool*exp_val)) list =
          (* Begin matching on recordVal *)
          match recordVal_to_alter with
          (* If the recordVal_to_alter is empty,
                        i.e. we've successfully parsed the entire RecordVal and created the modified recordVal through recursion,
                then return the empty list, as that's all that needs to be @ppended to the resulting (string, (bool, exp_val)) list. *)
          | [] -> original_myrecord
          (* Else, the recordVal_to_alter has at least one more field left to modify,
                so split recordVal_to_alter into a head (i.e. first_field) and a tail (i.e. rest_of_the_fields) *)
          | first_field::rest_of_the_fields ->
            (* Begin matching on the first_field *)
            match first_field with
            (* For the case where the field has a name field_name, a boolean is_mutable indicating if the field can be altered,
                and the reference field_data_refval that contains the reference to the current data for the field
                        (assuming that it is a RefVal, but if it isn't an error will be thrown and )(i.e. every case) *)
            | (field_name, (is_mutable, field_data_refval)) ->
              (* If the field_name == name_of_field_to_be_altered *)
              if field_name = name_of_field_to_be_altered
              (* Then enter the following sub-if statement to alter the subfield or return an error if the field cannot be altered. *)
              then (
                  (* If the field is mutable *)
                  if (is_mutable)
                  (* Then current_field_data is a reference to the current data the field contains.
                      So use the Store.set_ref function with the global_store to set the reference of the field to new_field_data
                      and @ppend the modified field to the recursive call of alter_recordVal. *)
                  then (
                      (* Obtain the integer value that
                                the RefVal current_field_data contains and bind it to first_field_data_reference_number *)

                      int_of_expRefVal field_data_refval >>= fun first_field_data_reference_number ->
                      (* Set the value of the reference at first_field_data_reference_number in the global_store
                                                                                to the value new_field_data *)
                      (let call_store (reference_no:int) (replacement_expVal:exp_val) =
                        Store.set_ref g_store reference_no replacement_expVal
                      in call_store first_field_data_reference_number new_field_data)
                    )
                  (* Else return the error Error "Field not mutable" *)
                  else (error "Field not mutable.")
                )
              (* Else @ppend the list containing the first_field with the recursive call of alter_recordVal.  *)
              alter_recordVal rest_of_the_fields name_of_field_to_be_altered new_field_data

        (* Now call alter_recordVal using the RecordVal 'myrecord', the field name 'id', and the new_field_value. *)
        in alter_recordVal myrecord myrecord id new_field_value
    )

  | Unit -> return UnitVal
  | Debug(_e) ->
    string_of_env >>= fun str_env ->
    let str_store = Store.string_of_store string_of_expval g_store
    in (print_endline (str_env^"\n"^str_store);
    error "Reached breakpoint")
  | _ -> failwith ("Not implemented: "^string_of_expr e)
and
  process_field (_id,(is_mutable,e)) =
  eval_expr e >>= fun ev ->
  if is_mutable
  then return (RefVal (Store.new_ref g_store ev))
  else return ev


(* Parse a string into an ast *)

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf


(* Interpret an expression *)
let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_expr
  in run c
