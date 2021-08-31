(* Ben Rose *)
(* Homework 3 *)
(* 3-20-2021 *)

open Ast
open Ds


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
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
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
    return @@ BoolVal (n = 0)
  (* Begin function from alternate Quiz 3 *)
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
    (* End function from alternate Quiz 3 *)
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2

  (* Begin specified extensions from the homework spec *)

  (* Exercise 1: Absolute Value (i.e. abs)
      Note: All negative numbers must be written in parethesis *)
  (* Commands to see if it's working right and their results when using utop:
      utop # interp "abs((-5)) - 6"
      - : exp_val Proc.Ds.result = Ok (NumVal (-1))
      utop # interp "abs(7) - 6"
      - : exp_val Proc.Ds.result = Ok (NumVal 1)
    *)
  | Abs(e1) ->
    (* Evaluate the given expression e1 *)
    eval_expr e1 >>=
    (* Bind e1 to its converted integer value n1 *)
    int_of_numVal >>= fun n1 ->
    (* If n1 is less than zero, i.e. it is negative *)
    if (n1 < 0)
    (* Then call the return function applied to the now positive version of n1. *)
    then return @@ (NumVal ((n1 * (-1))))
    (* Else n1 is already positive, so call the return function applied to the NumVal n1 *)
    else return @@ (NumVal (n1))

  (* Exercise 2: Lists *)

  (* Exercise 2 Part 1: EmptyList
      Creates and returns an empty list. *)
  (* Commands to see if it's working right and their results when using utop:
      utop # interp "emptylist"
      - : exp_val Proc.Ds.result = Ok (ListVal [])
    *)
  | EmptyList ->
    (* Call the return function applied to the ListVal [] (i.e. the empty list) *)
    return @@ ListVal ([])

  (* Exercise 2 Part 2: Cons
      Cons is a constructor, appending a value (not a list)
                              to the front of a list of elements,
                              where elements of the list of the same type as the value you're appending.
      If the second element passed to Cons is not a list, return an error.
    The "cons" operator is equivalent to "::" outside of this interpreter. *)

  (* Commands to see if it's working right and their results when using utop:
    utop # interp "cons(0, 1)";;
    - : exp_val Proc.Ds.result = Error "Expected a list!"

    utop # interp "cons (1, emptylist)";;
    - : exp_val Proc.Ds.result = Ok(ListVal[NumVal 1])

    utop # interp "cons(cons (1, emptylist), emptylist)";;
    - : exp_val Proc.Ds.result = Ok(ListVal[ListVal[NumVal 1]])

    utop # interp "cons(emptylist, emptylist)";;
    - : exp_val Proc.Ds.result = Ok (ListVal [ListVal []])

    utop # interp "let x = 4
              in cons (x,
                  cons (cons (x-1,
                        emptylist),
              emptylist))";;
    - : exp_val Proc .Ds. result = Ok(ListVal[NumVal 4; ListVal[NumVal 3]])
      *)
  | Cons(e1, e2) ->
    (* The is_ListVal functiion apparently takes exp_val as an input instead of an expression.

          Note: Pseudocode:
              If e2 is a list
                  Convert e1 to its corresponding value, then
                    @ppend the list containing the converted e1 to the front of the converted list e2. *)

    (* Begin evaluating the expression e2 *)
    eval_expr e2 >>=
    (* Convert e2 into the list mylist, or throw an error if e2 is not a list *)
    list_of_listVal >>= fun mylist ->
    (* Evaluate the expression e1 and store whatever value it has in new_head_element. Line achieved with the professor's help. *)
    eval_expr e1 >>= fun new_head_element ->
      (* Return the ListVal composed of the new_head_element @ppended to the front of mylist. *)
      return @@ ListVal(new_head_element::mylist)

  (* Exercise 2 Part 3: Hd
      Returns the head (i.e. the first element) of a list.
      If e1 is not a list or if the list is empty, an error will be produced. *)
  (* Commands to see if it's working right and their results when using utop:

    utop # interp "hd(0)";;
    - : exp_val Proc.Ds.result = Error "Expected a list!"

    utop # interp "hd(emptylist)";;
    - : exp_val Proc.Ds.result = Error "Cannot get the head of an empty list!"

    utop # interp "hd(cons(emptylist, emptylist))";;
    - : exp_val Proc.Ds.result = Ok (ListVal [])

    utop # interp "hd(cons(1,emptylist))";;
    - : exp_val Proc.Ds.result = Ok (NumVal 1)

    utop # interp "hd(cons(cons(1,emptylist),emptylist))";;
    - : exp_val Proc.Ds.result = Ok (ListVal [NumVal 1])

    utop # interp "let x = 4
              in hd(cons (x,
                  cons (cons (x-1,
                        emptylist),
              emptylist)))";;
    - : exp_val Proc.Ds.result = Ok (NumVal 4)

  *)
  | Hd(e1) ->
    (* Begin evaluating the expression e1 *)
    eval_expr e1 >>=
    (* Convert e1 into the list mylist, or produce an error if e1 is not a list *)
    list_of_listVal >>= fun mylist ->
      (
      (* Begin matching on mylist *)
      match mylist with
      (* If mylist is empty *)
      | [] ->
        (* Then produce an error, as there is no first element in an empty list *)
        error "Cannot get the head of an empty list!"
      (* Else mylist has at least one element in it,
            so split mylist into its first_element (i.e. the head) and the rest_of_mylist (i.e. the tail) *)
      | first_element::rest_of_mylist ->
        (* Call the return function applied to the first_element (i.e. the head) of mylist,
            i.e. return the first_element of mylist*)
        return @@ first_element
      )

  (* Exercise 2.4: Tl
    Returns the tail (i.e. the list of everything but the first element) of a list.
    If e1 is not a list or if the list is empty, an error will be produced. *)

  (* Commands to see if it's working right and their results when using utop:

    utop # interp "tl(0)";;
    - : exp_val Proc.Ds.result = Error "Expected a list!"

    utop # interp "tl(emptylist)";;
    - : exp_val Proc.Ds.result = Error "Cannot get the tail of an empty list!"

    utop # interp "tl(cons(emptylist, emptylist))";;
    - : exp_val Proc.Ds.result = Ok (ListVal [])

    utop # interp "tl(cons(1,emptylist))";;
    - : exp_val Proc.Ds.result = Ok (ListVal [])

    utop # interp "tl(cons(cons(1,emptylist),emptylist))";;
    - : exp_val Proc.Ds.result = Ok (ListVal [])

    utop # interp "let x = 4
              in tl(cons (x,
                  cons (cons (x-1,
                        emptylist),
              emptylist)))";;
    - : exp_val Proc.Ds.result = Ok (ListVal [ListVal [NumVal 3]])
  *)
  | Tl(e1) ->
    (* Begin evaluating the expression e1 *)
    eval_expr e1 >>=
    (* Convert e1 into the list mylist, or produce an error if e1 is not a list *)
    list_of_listVal >>= fun mylist ->
      (
      (* Begin matching on mylist *)
      match mylist with
      (* If mylist is empty *)
      | [] ->
        (* Then produce an error, as an empty list has no first element and can therefore not be divided into a head and a tail *)
        error "Cannot get the tail of an empty list!"
      (* Else, mylist has at least one element in it,
          so split mylist into its first_element (i.e. the head) and the rest_of_mylist (i.e. the tail) *)
      | first_element::rest_of_mylist ->
        (* Call the return function applied to the ListVal of rest_of_mylist (i.e. the tail of mylist),
            i.e. return the ListVal(rest_of_mylist) *)
        return @@ ListVal(rest_of_mylist)
      )

  (* Exercise 2 Part 5: Empty
      Input: An expression
      Output: An error if the input expression doesn't evaluate to a list,
              else the function returns whether the input is an empty list *)
  (* Commands to see if it's working right and their results when using utop:
      utop # interp " empty?(emptylist)";;
      - : exp_val Proc.Ds.result = Ok(BoolVal true )
      utop # interp " empty ?( tl(cons(cons (1, emptylist), emptylist )))";;
      - : exp_val Proc.Ds.result = Ok(BoolVal true) *)
  | Empty(e1) ->
    (* Evaluate the expression e1 *)
    eval_expr e1 >>=
    (* Convert (or attempt to convert) the expression e1 into the list named mylist.
      An error is thrown by the list_of_listVal function if e1 is not a list. *)
    list_of_listVal >>= fun mylist ->
    (* Return the BoolVal indicating whether or not mylist == an empty list *)
    return @@ (BoolVal(mylist = []))

  (* Exercise 3: Trees *)

  (* Exercise 3 Part 1: EmptyTree
      This function creates and returns an empty tree. *)

  (* Note: The values displayed in utop when running this implementation are effectively the same as are in the PDF.
  In an announcement titled "HW3 - On fully qualified names", the professor said:

  "
  When you type:

  # interp "tl(cons(1, emptylist))";;

  you will most likely get:

  - : exp_val Proc.Ds.result = Ok (ListVal [])

  This is not exactly the same as what is stated in the pdf:

  - : Ds.exp_val Ds.result = Ds.Ok (Ds.TreeVal Ds.Empty)

  That is ok. You may *ignore* this difference.
  "

  I see this to mean that the differences between the PDF's example result for the EmptyTree function, which is:

  utop # interp "emptytree";;
  - : exp_val Proc.Ds.result = Ok (TreeVal Empty)


  and my implementation's results:

  utop # interp "emptytree";;
  - : exp_val Proc.Ds.result = Ok (TreeVal Proc.Ds.Empty)

  to be negligable. They both appear to be pointing to the potential value "Empty" in the definition of 'a tree in the ds.ml file.
  I believe mine is just a fully qualified reference to it, since the "Proc.Ds.Empty" in all likelyhood refers to
                                                            looking in the "Proc" language,
                                                                  in the file "Ds",
                                                                      for the definition of "Empty".
  I've also tried implementing it in every other way I could think of with no results (the failed implementations were listed below),
      which also helped me arrive at the above conclusion. *)

   (* By the explanation above, the command to see if it's working right and their results when using utop is:
       utop # interp "emptytree";;
       - : exp_val Proc.Ds.result = Ok (TreeVal Proc.Ds.Empty)
    *)
  | EmptyTree ->
    (* Call the return function applied to the TreeVal of Empty (i.e. the Empty tree),
        i.e. return the empty tree*)
    return @@ (TreeVal(Empty))

  (* Exercise 3 Part 2: Node(e1, lte, rte)
      Creates a new tree with the root_data expressed as e1,
                          the left subtree expressed as lte,
                          and the right subtree expressed as rte.
      If either lte or rte does not evaluate to a tree, the function should produce an error. *)

  (* Commands to see if it's working right and their results when using utop:

    utop # interp "node(5, node (6, emptytree, emptytree), emptytree)";;
    - : exp_val Proc.Ds.result =
    Ok
     (TreeVal
       (Proc.Ds.Node (NumVal 5,
         Proc.Ds.Node (NumVal 6, Proc.Ds.Empty, Proc.Ds.Empty), Proc.Ds.Empty)))

  Note that these are not identical to the results in the assignment PDF, but I believe are perfectly acceptable by the logic
  explained in the comments for the EmptyTree function (right above the "Commands to see if it's working right in utop" section).
  *)
  | Node(e1,lte,rte) ->

    (* First evaluate the expressions lte and rte, as if either of them are not trees, there is no point in evaluating e1,
                                                                                  since an error would have to be thrown anyways. *)
    (* Evaluate the expression lte *)
    eval_expr lte >>=
    (* Convert (or attempt to convert) the expression lte into the tree named left_subtree.
        An error is thrown by the tree_of_treeVal function if lte is not a tree. *)
    tree_of_treeVal >>= fun left_subtree ->

    (* Evaluate the expression rte *)
    eval_expr rte >>=
    (* Convert (or attempt to convert) the expression rte into the tree named right_subtree.
        An error is thrown by the tree_of_treeVal function if rte is not a tree. *)
    tree_of_treeVal >>= fun right_subtree ->

    (* Evaluate the expression e1 and convert whatever data it contains into the variable root_data. *)
    eval_expr e1 >>= fun root_data ->

    (* Call the return function applied to the TreeVal with the root node's data = root_data,
                                                                the root node's left subtree = left_subtree,
                                                                and the root node's right subtree = right_subtree,
      i.e. return the node we just created with data root_data,
                            the node's left subtree being left_subtree,
                            and the node's right subtree being right_subtree. *)
    return @@ (TreeVal(Node(root_data, left_subtree, right_subtree)))

  (* Exercise 3 Part 3: CaseT
      CaseT has... a lot to unpack. It appears to be the combination of everything in the PROC language,
                                                  or at least able to combine everything in the PROC language.

      So, going variable by variable and defining them (and pulling their concrete syntax definitions):

      target: The tree that you want to use the CaseT function on. (This is an expression)
      emptycase: Instructions on what to do in the case that the tree is empty. (This is of type emptytree -> expression,
                                                                                abstract-syntaxly of type expression)
      id1: The root_data of the target tree (This is of type ID)
      id2: The left_subtree of the target tree (This is of type ID)
      id3: The right_subtree of the target tree (This is of type ID)
      nodecase: Instructions on what to do in the case that the tree is made up of Node(root_data, left_subtree, right_subtree)
      ^^^ nodecase is of type expression

      *)

  (* So what exactly is CaseT and what does the function do?
      CaseT appears to be like a cross between a hand trace of recursion and a series of if-then-else statements.

      CaseT is not recursive (nor is such a thing possible in the PROC language since
                              recursive function definitions have not been implemented in the PROC language),
              but every call of CaseT requires the "emptycase", which seems almost identical to the base case in a recursive function,
              and a "nodecase", which seems similar to the inductive (i.e. haven't reached the empty) case.

      However, CaseT branches off from this recursive function structure when you look at the details in what can be done with it.

      Unlike recursive functions, each call of CaseT can have different instructions than the previous call,
      as shown in the last example utop case.
      So each definition of CaseT for the emptycase and nodecase are only used once,
      and when you call CaseT again (for example on a subtree), you can define its functionality differently.

      So CaseT is essentially an "executor" of whatever instructions you specify on the target tree
      and returns the result of running your instructions.

      What CaseT does in each of its function calls is dynamic and not tied to one specific definition.
      The only static thing about it is that it executes whatever instructions you pass to it.
      *)

  (* Commands to see if it's working right and their results in utop:

    utop # interp "
      caseT emptytree of {
          emptytree -> emptytree,
          node (a,l,r) -> l
      }";;

    - : exp_val Proc.Ds.result = Ok(TreeVal Proc.Ds.Empty)

    Note: This kept giving the error message "Exception: Proc.Parser.MenhirBasics.Error.",
          so it doesn't really work. The other cases do, though.
    utop # interp "
      caseT emptytree of {
          emptytree -> NumVal((3 * 5) * (19 * 281))
          node (a,l,r) -> a
      }";;

    - : exp_val Proc.Ds.result = Ok(NumVal 80085)


    utop # interp "
      let t = node(emptylist,
                    node(cons (5, cons (2, cons (1, emptylist))),
                        emptytree,
                        node(emptylist,
                              emptytree,
                              emptytree
                        )
                    ),
                    node(tl(cons (5, emptylist)),
                          node(cons (10, cons (9, cons (8, emptylist))),
                                emptytree,
                                emptytree
                          ),
                          node(emptylist,
                                node(cons (9, emptylist),
                                      emptytree,
                                      emptytree
                                ),
                                emptytree
                          )
                    )
              )
      in
      caseT t of {
          emptytree -> 10,
          node (a,l,r) ->
              if empty?(a)
              then caseT l of {
                      emptytree -> 21,
                      node (b, ll, rr) -> if empty?(b)
                                          then 4
                                          else if zero?(hd (b))
                                               then 22
                                               else 99
                   }
              else 5
      }";;

      - : exp_val Proc.Ds.result = Ok(NumVal 99)
   *)
  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->
    (* return @@ App(nodecase, target) *)
    (* Evaluate the expression "target" *)
    eval_expr target >>=
    (* Convert (or attempt to convert) the expression "target" into the tree named "target_tree".
        An error is thrown by the tree_of_treeVal function if "target" is not a tree. *)
    tree_of_treeVal >>= fun target_tree ->
    (* Then we evaluate the expression "nodecase" and bind whatever instructions it has in it to the variable nodecase_instructions.
        We do this because the expression "nodecase" indicates what to do in the case where the target_tree is not empty. *)
      eval_expr nodecase >>= fun nodecase_instructions ->
      (* And apply the return function to the nodecase_instructions,
          i.e. execute the nodecase_instructions.*)
      return @@ nodecase *)
      eval_expr emptycase >>= fun emptycase_instructions ->
      match target_tree with
      | Empty ->
        return @@ emptycase_instructions
      | Node(root_data, left_subtree, right_subtree) ->
        eval_expr nodecase >>= fun nodecase_instructions ->
        return @@ nodecase_instructions
and
  eval_prog (AProg e) = eval_expr e

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf

(* Interpret an expression *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
