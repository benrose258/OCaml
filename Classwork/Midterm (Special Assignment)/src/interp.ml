(* Ben Rose *)
(* Special Assignment 1 (Midterm) *)
(* 4-6-2021 *)

open Ast
open Ds

let rec apply_proc : exp_val -> exp_val -> exp_val ea_result =
  fun f a ->
  match f with
  |  ProcVal (id,body,env) ->
    return env >>+
    extend_env id a >>+
    eval_expr body
  | _ -> error "apply_proc: Not a procVal"
and
  eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) -> return (NumVal n)
  | Var(id) -> apply_env id
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
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return (fst p)
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return (snd p)
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Letrec(id,par,e1,e2) ->
    extend_env_rec id par e1 >>+
    eval_expr e2
  (* Exercise 1: EmptyTable
      Creates an empty hash table. *)

  (* Commands to see if it's working right and their results when using utop:
      utop # interp "emptytable";;
      - : exp_val Recht.Ds.result = Ok (TableVal [])
  *)
  | EmptyTable ->
    (* Call the return function applied to an empty hash table
          i.e. return the empty hash table *)
    return @@ TableVal ([])

  (* Exercise 2: Insert
      Adds a key-value pair into the hash table. *)

      (* Overview of variables:
      e1: An expression with the "key" part of the key-value pair that we're inserting into the hash table
      e2: An expression with the "value" part of the key-value pair that we're inserting into the hash table
      e3: Any additional insertions you want to add into the table using the "insert" function
                  OR the hash table "emptytable" if you've finished inserting things into the table.
  *)

  (* Commands to see if it's working right and their results when using utop:

      utop # interp "insert (0, 1234, emptytable)";;
      - : exp_val Recht.Ds.result = Ok (TableVal [(NumVal 0, NumVal 1234)])

      utop # interp "insert (emptytable, emptytable, emptytable)";;
      - : exp_val Recht.Ds.result = Ok (TableVal [(TableVal [], TableVal [])])

      utop # interp "insert (emptytable, emptytable, insert
            (emptytable, emptytable, emptytable))";;
      - : exp_val Recht.Ds.result =
      Ok (TableVal [(TableVal [], TableVal []); (TableVal [], TableVal [])])]

      utop # interp "insert (0, 1234, insert (1, 5678, insert (2, 9101, insert
            (3, 1121, emptytable))))";;
      - : exp_val Recht.Ds.result =
      Ok
        (TableVal
          [(NumVal 0, NumVal 1234); (NumVal 1, NumVal 5678); (NumVal 2, NumVal 9101);
           (NumVal 3, NumVal 1121)])

      utop # interp "insert (emptytable, emptytable, remove (emptytable, insert
            (emptytable, emptytable, insert (emptytable, emptytable, emptytable))))";;
      - : exp_val Recht.Ds.result = Ok (TableVal [(TableVal [], TableVal [])])

  *)
  | Insert(e1,e2,e3) ->
    (* Evaluate the expression "e3".
        This is done first because the values of e1 and e2 don't matter if e3 isn't a TableVal
            and since the point of a hash table is to improve running time,
            the marginal improvement by evaluating e3 first is consistent with that theme. *)
    eval_expr e3 >>=
    (* Using the table_of_tableVal function, attempt to bind the information in e3 to the variable "my_hash_table".
            If the expression "e3" is NOT a hash table, an error will be thrown by the table_of_tableVal function. *)

      (* Note: A hash table (i.e. a TableVal) is defined in ds.ml to have the type "(exp_val*exp_val) list",
            which means that a hash table is a list of two-item tuples.
            Each of the tuples have two "exp_val"s in them. *)
    table_of_tableVal >>= fun my_hash_table ->
    (* Evaluate the expression "e1" and bind whatever information it has to the variable "key" *)
    eval_expr e1 >>= fun key ->
    (* Evaluate the expression "e2" and bind whatever information it has to the variable "value" *)
    eval_expr e2 >>= fun value ->
    (* Return the TableVal resulting from
          Adding the tuple "(key, value)" to the front of the TableVal my_hash_table. *)
    return @@ TableVal((key, value)::my_hash_table)

  (* Exercise 3: Lookup
        Given a key, retrieve the value associated with the key in the hash table. *)

  (* Overview of variables:
      e1: The key that we are seeking the corresponding value for
      e2: The hash table that the Lookup function is searching in. *)

  (* Note from assignment spec:
     If the key is not found,
        the message "Lookup: Key not found." should be returned by request of the professor in the assignment spec.
      Presumably any error or similar message would work for a failed lookup,
        but the professor requested this message to expedite grading. *)

  (* Commands to see if it's working right and their results when using utop:

      utop # interp "lookup(0, emptytable)";;
      Exception: Failure "Lookup: Key not found.".

      utop # interp "lookup(6, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))";;
      Exception: Failure "Lookup: Key not found.".

      utop # interp "lookup(emptytable, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))";;
      Exception: Failure "Lookup: Key not found.".

      utop # interp "lookup(0, remove(0, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable))))))";;
      Exception: Failure "Lookup: Key not found.".


      utop # interp "lookup(2, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))";;
      - : exp_val Recht.Ds.result = Ok (NumVal 9101)

  *)
  | Lookup(e1,e2) ->
    (* Evaluate the expression "e2".
      This is done first because the value of e1 doesn't matter if e2 isn't a TableVal
          and since the point of a hash table is to improve running time,
          the marginal improvement by evaluating e2 first is consistent with that theme. *)
    eval_expr e2 >>=
    (* Using the table_of_tableVal function, attempt to bind the information in e2 to the variable "my_hash_table".
        If the expression "e2" is NOT a hash table, an error will be thrown by the table_of_tableVal function. *)

      (* Note: A hash table (i.e. a TableVal) is defined in ds.ml to have the type "(exp_val*exp_val) list",
            which means that a hash table is a list of two-item tuples.
            Each of the tuples have two "exp_val"s in them. *)
    table_of_tableVal >>= fun my_hash_table ->
    (* Evaluate the expression e1 and store whatever exp_val it contains in the variable "lookup_key". *)
    eval_expr e1 >>= fun lookup_key ->

    (* Return the result of the following helper function: *)
    (* Define the helper function find_associated_value that takes as input:
            A key to search for
            and
            A hash table to look for the corresponding key-value pair in (not in TableVal form, the (exp_val * exp_val) list form)
        and returns either the associated value to the specified key
                    or throws an error (using the failwith function) stating "Lookup: Key not found.". *)

    return (
    let rec find_associated_value (mykey:exp_val) (hash_table_to_search:(exp_val * exp_val) list) : exp_val =
      (* Begin matching on hash_table_to_search *)
      match hash_table_to_search with
      (* If the hash_table_to_search is empty, then return the error (in this case using failwith) "Lookup: Key not found." *)
      | [] -> failwith "Lookup: Key not found."
      (* Else, the hash_table_to_search has at least one element left in it,
          so we split up the hash_table_to_search into a head (i.e. first_key_value_pair) and a tail (i.e. other_key_value_pairs),
          which can be done because hash tables are implemented as lists of tuples in this language (i.e. the language Recht) *)
      | first_key_value_pair::other_key_value_pairs ->
        (* Begin matching on the first_key_value_pair *)
        match first_key_value_pair with
        (* Match it with the tuple (key, value), i.e. every case *)
        | (key, value) ->
          (* If key == mykey (i.e. this key-value pair is the one we're looking for) *)
          if (key = mykey)
          (* Then call the return function applied to "value",
              i.e. return the corresponding value *)
          then value
          (* Else, the first key-value pair in the hash_table_to_search is not what we're looking for,
                  so call find_associated_value using mykey over the other_key_value_pairs. *)
          else find_associated_value mykey other_key_value_pairs
    (* Use the variables lookup_key and my_hash_table as input to attempt to find the value associated to the lookup_key *)
    in find_associated_value lookup_key my_hash_table
    )

  (* Exercise 4: Remove
      Given a key, remove the key-value pair from the hash table and display the new hash table. *)

  (* Overview of variables:
      e1: The Key of the Key-Value pair that you want to remove
      e2: The hash table you want to remove the instances of the key from
  *)

  (* Commands to see if it's working right and their results when using utop:

      utop # interp "remove(emptytable, insert (emptytable, emptytable, emptytable))";;
      - : exp_val Recht.Ds.result = Ok (TableVal [])

      utop # interp "remove(emptytable, insert (emptytable, emptytable, insert
            (emptytable, emptytable, emptytable)))";;
      - : exp_val Recht.Ds.result = Ok (TableVal [])

      utop # interp "remove(2, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))";;
      - : exp_val Recht.Ds.result =
      Ok
        (TableVal
          [(NumVal 0, NumVal 1234); (NumVal 1, NumVal 5678); (NumVal 3,
                NumVal 1121)])



      utop # interp "empty?(remove(3, remove(2, remove(1, remove(0, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))))))";;
      - : exp_val Recht.Ds.result = Ok (BoolVal true)

  *)
  | Remove(e1,e2) ->
    (* Evaluate the expression "e2".
        This is done first because the value of e1 doesn't matter if e2 isn't a TableVal,
              and since the point of a hash table is to improve running time,
              the marginal improvement by evaluating e2 first is consistent with that theme. *)
    eval_expr e2 >>=
    (* Using the table_of_tableVal function, attempt to bind the information in e2 to the variable "my_hash_table".
              If the expression "e2" is NOT a hash table, an error will be thrown by the table_of_tableVal function. *)
    table_of_tableVal >>= fun my_hash_table ->
    (* Evaluate the expression e1 and bind whatever information it has to the variable "key_to_remove" *)
    eval_expr e1 >>= fun key_to_remove ->
    (* Return the TableVal resulting from executing the following helper function: *)
    return @@ TableVal(
      (* The function remove_associated_values takes as input:
            A removal_key (a key to search for and remove instances of)
            A hash_table_for_removal (the hash table whose keys we are looking to remove) in non-TableVal form
        and returns the resulting hash table from removing all key-value pairs that have the key == removal_key
      *)
      let remove_associated_values (removal_key:exp_val) (hash_table_for_removal:(exp_val * exp_val) list) : (exp_val * exp_val) list =

          (* Call the helper function remove_associated_values_brain to actually remove the associated values.
                  This helper function exists for the sake of cleanliness in the outermost function call,
                    but remove_associated_values could have been coded to perform this as well.
                    It just would have required more variables in its function call. *)
          (* The function remove_associated_values_brain takes as input:
                A removal_key (a key to search for and remove instances of)
                A hash_table_for_removal (the hash table whose keys we are looking to remove) in non-TableVal form
                The unedited_hash_table_for_removal (the original hash_table_for_removal that is going to be used
                                                                                            to verify that key-value pairs were
                                                                                            removed from the hash_table_for_removal)
                A blank hash table that will be filled and returned with the key-value pairs in hash_table_for_removal
                                                                            where the keys != removal_key in non-TableVal form
            and returns the resulting_hash_table.
            However, if the length of the resulting_hash_table == length of the hash_table_for_removal,
                      then the key was not found, so the error "Remove: Key not found." will be thrown (as requested by
                                                                                                        the assignemnt spec)
          *)

          let rec remove_associated_values_brain (removal_key:exp_val) (hash_table_for_removal:(exp_val * exp_val) list)
                                               (unedited_hash_table_for_removal:(exp_val * exp_val) list)
                                               (resulting_hash_table:(exp_val * exp_val) list) : (exp_val * exp_val) list =
            (* Begin matching on hash_table_for_removal *)
            match hash_table_for_removal with
            (* If the hash_table_for_removal is empty (i.e. we've iterated over the entire hash_table_for_removal) *)
            | [] ->
              (* If the unedited_hash_table_for_removal is not empty
                        && the list_length of unedited_hash_table_for_removal != list length of resulting_hash_table,
                                  which will be determined by using the recursively defined list_length function
                                                                        (originally from the General Trees assignment
                                                                        (i.e. gt.ml (i.e. Homework 2)))*)
              if (
                (* Given any list my_list, this function recursively finds and returns the length of the list. *)
                let rec list_length (my_list:'a list) : int =
                (* Begin matching on the list my_list *)
                match my_list with
                (* If my_list is empty, which means the function was either fed an empty list or we have finished the recursive calls,
                                return 0 *)
                | [] -> 0
                (* Else my_list has at least one element in it. So break my_list into a head (i.e. first_element)
                                                                                    and a tail (i.e. rest_of_my_list) *)
                | _first_element::rest_of_my_list ->
                  (* Return 1 + a recursive call of list_length over the rest_of_my_list *)
                  1 + (list_length rest_of_my_list)
                (* If the list length of the resulting hash table is not equal to
                                                                  the list length of the unedited_hash_table_for_removal,
                                Note that "<>" is a "structural not equal" in OCaml, where "!=" is a "physical not equal",
                                                                                          which is essentially whether or not
                                                                                          the two objects you're comparing are
                                                                                          identical.
                                I'm using structural so that the contents of the two sides can be compared
                                rather than the exact literal values of the two sides. *)
                in
                  ((list_length unedited_hash_table_for_removal) <> 0)
                  &&
                  (list_length resulting_hash_table <> list_length unedited_hash_table_for_removal)
                (* By the way, this close parentheses two lines below the end of this comment is the end of the "if" condition's
                                                                                                      definition, just in case it's
                                                                                                      been a while and you forgot *)
                )
                (* Then, the resulting_hash_table has had at least one element removed,
                    so the key was found and the resulting_hash_table should be returned. *)
                then resulting_hash_table
                (* Else, return the error message "Remove: Key not found.",
                    as no key-value pairs were detected to have the key removal_key and were removed because of it.
                    This error also trivially occurs if the hash_table_for_removal is initially empty. *)
                else failwith "Remove: Key not found."
            (* Else, the hash_table_for_removal has at least one element left in it,
                so we split up the hash_table_for_removal into a head (i.e. first_key_value_pair)
                                                              and a tail (i.e. other_key_value_pairs).
              This can be done for the same reason explained in the "find_associated_value" function
                                                                  within the Lookup function
                                                                  when defining the identical line as we are doing below. *)
            | first_key_value_pair::other_key_value_pairs ->
              (* Begin matching on the first_key_value_pair *)
              match first_key_value_pair with
              (* For the case that the first_key_value_pair is a tuple containing a key and a value (i.e. every case) *)
              | (key, value) ->
                (* If the key != removal_key *)
                if (key <> removal_key)
                (* Then @ppend the list containing the tuple (key, value) to the end of the resulting_hash_table
                    and recursively call remove_associated_values_brain over the other_key_value_pairs.
                           We are @ppending as specified above instead of @ppending to the front or using the :: operator at the front
                                                                                                      (as required by the :: operator)
                                     because this method preserves the original order of the hash_table_for_removal.
                            Adding the key-value pair to the front would reverse the order. *)
                then remove_associated_values_brain removal_key other_key_value_pairs
                                                    unedited_hash_table_for_removal (resulting_hash_table @ [(key, value)])
                (* Else, the key of this key-value pair == removal_key, so it must not be included in the resulting_hash_table.
                    So call remove_associated_values_brain using the removal_key, other_key_value_pairs,
                                                                    the unedited_hash_table_for_removal,
                                                                    and the unchanged resulting_hash_table. *)
                else remove_associated_values_brain removal_key other_key_value_pairs
                                                    unedited_hash_table_for_removal resulting_hash_table

          (* Call remove_associated_values_brain using the removal_key, hash_table_for_removal,
                                                            hash_table_for_removal, and a blank hash table that will be populated. *)
          in remove_associated_values_brain removal_key hash_table_for_removal hash_table_for_removal []
      (* Call remove_associated_values using the key_to_remove and my_hash_table,
          both of which were specified in the function call of remove. *)
      in remove_associated_values key_to_remove my_hash_table
      )

  (* Exercise 5: Size
      Returns the amount of elements in the hash table. *)

  (* Overview of variables:
      e: The hash table that we examining to determine whether it is empty *)

  (* Commands to see if it's working right and their results when using utop:

      utop # interp "size(emptytable)";;
      - : exp_val Recht.Ds.result = Ok (NumVal 0)

      utop # interp "size(insert (0, 1234, insert (1, 5678, insert (2, 9101, insert
            (3, 1121, emptytable)))))";;
      - : exp_val Recht.Ds.result = Ok (NumVal 4)

      utop # interp "size(remove(2, insert(2, 9101, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))))";;
      - : exp_val Recht.Ds.result = Ok (NumVal 3)

      utop # interp "size(remove(3, remove(2, remove(1, remove(0, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))))))";;
      - : exp_val Recht.Ds.result = Ok (NumVal 0)

  *)
  | Size(e) ->
    (* Evaluate the expression "e" *)
    eval_expr e >>=
    (* Using the table_of_tableVal function, attempt to bind the information in "e" to the variable "my_hash_table".
            If the expression "e" is NOT a hash table, an error will be thrown by the table_of_tableVal function. *)
    table_of_tableVal >>= fun my_hash_table ->
    (* Now return the NumVal resulting from calling the list_length function (brought over from gt.ml) on my_hash_table,
        since hash tables in the Recht language are of type (exp_val * exp_val) list *)
    return @@ NumVal(
        (* Given any list my_list, this function recursively finds and returns the length of the list. *)
        let rec list_length (my_list:'a list) : int =
        (* Begin matching on the list my_list *)
        match my_list with
        (* If my_list is empty, which means the function was either fed an empty list or we have finished the recursive calls,
                        return 0 *)
        | [] -> 0
        (* Else my_list has at least one element in it. So break my_list into a head (i.e. first_element)
                                                                            and a tail (i.e. rest_of_my_list) *)
        | _first_element::rest_of_my_list ->
          (* Return 1 + a recursive call of list_length over the rest_of_my_list *)
          1 + (list_length rest_of_my_list)
      (* Call list_length on my_hash_table
          and let the result be returned in this whole larger parentheses bubble, which ends after the "in" statement *)
      in list_length my_hash_table
      )

  (* Exercise 6: Empty
      Returns a boolean indicating whether the hash table is empty or not. *)

  (* Overview of variables:
      e: The hash table that we examining to determine whether it is empty *)

  (* Commands to see if it's working right and their results when using utop:

      utop # interp "empty?(insert (0, 1234, insert (1, 5678, insert (2, 9101, insert
            (3, 1121, emptytable)))))";;
      - : exp_val Recht.Ds.result = Ok (BoolVal false)

      utop # interp "empty?(remove(2, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable))))))";;
      - : exp_val Recht.Ds.result = Ok (BoolVal false)

      utop # interp "empty?(emptytable)";;
      - : exp_val Recht.Ds.result = Ok (BoolVal true)

      utop # interp "empty?(remove(3, remove(2, remove(1, remove(0, insert (0, 1234, insert (1, 5678, insert
            (2, 9101, insert (3, 1121, emptytable)))))))))";;
      - : exp_val Recht.Ds.result = Ok (BoolVal true)

  *)
  | IsEmpty(e) ->
    (* Evaluate the expression "e" *)
    eval_expr e >>=
    (* Using the table_of_tableVal function, attempt to bind the information in "e" to the variable "my_hash_table".
            If the expression "e" is NOT a hash table, an error will be thrown by the table_of_tableVal function. *)
    table_of_tableVal >>= fun my_hash_table ->
    (* Call the Return function applied to the BoolVal resulting from comparing my_hash_table to the empty hash table
          i.e. return whether or not my_hash_table is empty *)
    return @@ BoolVal(TableVal(my_hash_table) = TableVal([]))
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
let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_expr
  in run c
