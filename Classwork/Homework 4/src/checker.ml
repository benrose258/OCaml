(* Ben Rose *)
(* Homework 4 *)
(* April 28, 2021 *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

open Ast
open ReM
open Dst


let rec type_of_expr : expr -> texpr tea_result = function
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    type_of_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    type_of_expr e1 >>= fun t1 ->
    type_of_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    type_of_expr e1 >>= fun t1 ->
    type_of_expr e2 >>= fun t2 ->
    type_of_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    type_of_expr e >>= fun t ->
    extend_tenv id t >>+
    type_of_expr body
  | Proc(var,t1,e) ->
    extend_tenv var t1 >>+
    type_of_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | App(e1,e2) ->
    type_of_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    type_of_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec(id,param,tParam,tRes,body,target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     type_of_expr body >>= fun t ->
     if t=tRes
     then type_of_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")

  (* Exercise 1 (i.e. Section 3): References *)
  (* references *)
  | BeginEnd(es) ->
    List.fold_left (fun r e -> r >>= fun _ -> type_of_expr e) (return UnitType) es

  (* Exercise 1 Part 1: Unit
      If a Unit is received, return the UnitType. *)
  | Unit -> return UnitType

  (* Exercise 1 Part 1: NewRef
      Type checks the creation of a new reference. *)

  (* Overview of variables:
      e: An expression that we must check the type of, if it turns out to be typeable at all. *)

  (* Commands to see if it's working right and their results when using utop:

      utop # chk "let x = newref(0) in deref(x)";;
      - : texpr Checked.ReM.result = Ok IntType

      utop # chk "let x = newref(0) in x";;
      - : texpr Checked.ReM.result = Ok (RefType IntType)

      utop # chk "let x = newref(0) in setref(x, 4)";;
      - : texpr Checked.ReM.result = Ok UnitType

      utop # chk "newref(newref(zero?(0)))";;
      - : texpr Checked.ReM.result = Ok (RefType(RefType BoolType))

      utop # chk "let x = 0 in setref(x, 4)";;
      - : texpr Checked.ReM.result = Error "setref: Expected a reference type"

  *)
  | NewRef(e) ->

    (* Evaluate the Type Expression e and store whatever type it is in the variable *)
    type_of_expr e >>= fun type_of_reference_value ->
    (* Return the RefType of whatever type we are supplied to create a reference to *)
    return (RefType(type_of_reference_value))
  (* Exercise 1 Part 2: DeRef
      Retrieves the value associated with the reference, such as 0 in newref(0). *)

  (* Overview of variables:
      e: An expression that, if typed properly, is a reference. If it is, we'll extract the value associated with it.
  *)

  (* Commands to see if it's working right and their results when using utop:

      utop # chk "let x = newref (0) in deref (x)";;
      - : texpr Checked.ReM.result = Ok IntType

      utop # chk "let x = newref (0) in x";;
      - : texpr Checked.ReM.result = Ok (RefType IntType)

      utop # chk "let x = newref (0) in setref (x, 4)";;
      - : texpr Checked.ReM.result = Ok UnitType

      utop # chk "newref (newref (zero?(0)))";;
      - : texpr Checked.ReM.result = Ok (RefType (RefType BoolType))

      utop # chk "let x = 0 in newref (x, 4)";;
      - : texpr Checked.ReM.result = Error "setref: Expected a reference type"

  *)
  | DeRef(e) ->
    (* Evaluate the expression e *)
    type_of_expr e >>=
    (* Using the arg_of_refType function, determine whether or not e is a RefType.
        If e is a RefType, then store the type of the argument inside of the reference in the variable reference_argument_type.
        Else, an error will be thrown by the arg_of_refType function.
    *)
    arg_of_refType "The DeRef function says: " >>= fun reference_argument_type ->
    (* Now return reference_argument_type, as this is the type of the value associated with the provided reference. *)
    return reference_argument_type
  (* Exercise 1 Part 3: SetRef
      Checks if we are setting a valid reference, then returns the result of setting a reference *)

  (* Overview of variables:
      e1: The reference that we will attempt to set a new value to
      e2: The new value for the reference in e1
  *)

  (* Commands to see if it's working right and their results when using utop:

      utop # chk "let x = newref (0) in deref (x)";;
      - : texpr Checked.ReM.result = Ok IntType

      utop # chk "let x = newref (0) in x";;
      - : texpr Checked.ReM.result = Ok (RefType IntType)

      utop # chk "let x = newref (0) in setref (x, 4)";;
      - : texpr Checked.ReM.result = Ok UnitType

      utop # chk "newref (newref (zero?(0)))";;
      - : texpr Checked.ReM.result = Ok (RefType (RefType BoolType))

      utop # chk "let x = 0 in newref (x, 4)";;
      - : texpr Checked.ReM.result = Error "setref: Expected a reference type"

  *)
  | SetRef(e1,e2) ->
    (* Evaluate the expression e1 *)
    type_of_expr e1 >>=
    (* Using the arg_of_refType function, verify that e1 is indeed a RefType.
        If it is, then store the type of the argument inside of the reference in the variable reference_argument_type.
        Else, an error will be thrown by the arg_of_refType function.
    *)
    arg_of_refType "setref: " >>= fun reference_argument_type ->
    (* Evaluate the expression e2 and bind whatever type it is to the expression new_reference_argument_type *)
    type_of_expr e2 >>= fun new_reference_argument_type ->
    (* If the new_reference_argument_type != the original reference_argument_type *)
    if (reference_argument_type <> new_reference_argument_type)
    (* Then return an error, as the original reference_argument_type cannot be different from the new_reference_argument_type. *)
    then error "The SetRef function says: The new reference value does not have the same type as the original reference value!"
    (* Else, the original reference_argument_type == new_reference_argument_type, which means that the new reference can be set.
            Setting the reference using SetRef will always return a UnitType indicating that this function was successful,
              so return UnitType. *)
    else return UnitType
  (* Exercise 2: Pairs *)
  (* pairs *)
  | Pair(e1,e2) ->
    type_of_expr e1 >>= fun s ->
    type_of_expr e2 >>= fun t ->
    return (PairType(s,t))
  | Unpair(id1,id2,e1,e2) ->
    type_of_expr e1 >>=
    pair_of_pairType "unpair: " >>= fun (r,s) ->
    extend_tenv id1 r >>+
    extend_tenv id2 s >>+
    type_of_expr e2

  (* Exercise 3: Lists *)
  (* lists *)

  (* Exercise 3 Part 1: EmptyList
      Type checks the creation of an empty list of type t *)

  (* Overview of variables:
      t: A texpr that is the type of the elements that the empty list will be able to contain *)

  (* Commands to see if it's working right and their results when using utop:

      utop # chk "emptylist int";;
      - : texpr Checked.ReM.result = Ok (ListType IntType)

      utop # chk "emptylist bool";;
      - : texpr Checked.ReM.result = Ok (ListType BoolType)

      utop # chk "emptylist unit";;
      - : texpr Checked.ReM.result = Ok (ListType UnitType)

  *)

  | EmptyList(t) ->
    (* Return the ListType that contains "t"s, like items of type t. *)
    return (ListType(t))
  (* Exercise 3 Part 2: Cons
      Type checks using :: (i.e. Cons) on the head h and the tail t.
      If the type of h is equivalent to the type of elements that are allowed in the t,
          then return the type of the list t and the type of its elements.
      Else, return the error "cons: type of head and tail do not match" *)

  (* Overview of variables:
      h: An expression that contains the following: The head of the list that is going to be inserted at the front of the list.
      t: An expression that contains the following: The tail of the list. This should be of type ListType.
            If the type of elements in t are the same type as h, then the list can be "cons"d together. *)

  (* Commands to see if it's working right and their results when using utop:

  utop # chk "cons(null?(emptylist int), emptylist int)";;
  - : texpr Checked.ReM.result = Error "cons: type of head and tail do not match"

    utop # chk "cons(1, emptylist int)";;
    - : texpr Checked.ReM.result = Ok (ListType IntType)

  *)

  | Cons(h, t) ->
    (* Get the type of the head element, converting expr to texpr (i.e. an expression to a type_expression)
            and bind that texpr to head_type *)
    type_of_expr h >>= fun head_type ->
    (* Get the type of the tail, converting expr to texpr (i.e. an expression to a type expression)
            and bind that texpr to head_type *)
    type_of_expr t >>= fun tail_type ->
    (* If an emptylist of the type head_type has the same type as tail_type *)
    if (ListType(head_type) = tail_type)
    (* Then return what would be the type of using the cons operation on the head and tail (i.e. the type of h::t) *)
    then return (tail_type)
    (* Else, the type of the head (i.e. h) is not the type of elements in the tail (i.e. t), so the head and tail cannot be combined.
          So return the error "cons: type of head and tail do not match" *)
    else error "cons: type of head and tail do not match"

  (* Exercise 3 Part 3: Null
      Returns the type of the result of checking if the list provided is empty. *)

  (* Overview of variables:
      e: An expression. It should be of type ListType, but other bad types can be fed to the function. *)

  (* Commands to see if it's working right and their results when using utop:

    utop # chk "null?(zero?(1))";;
    - : texpr Checked.ReM.result =
    Error "The Null function says: List type expected!"

    utop # chk "null?(cons(1, emptylist int))";;
    - : texpr Checked.ReM.result = Ok BoolType

    utop # chk "null?(emptylist int)";;
    - : texpr Checked.ReM.result = Ok BoolType

    utop # chk "null?(cons(zero?(1), emptylist bool))";;
    - : texpr Checked.ReM.result = Ok BoolType

  *)

  | Null(e) ->
    (* Get the type of the expression e *)
    type_of_expr e >>=
    (* Check if the expression e is indeed a list *)
    arg_of_listType "The Null function says: " >>= fun _type_of_mylist_elements ->

    (* Now disregard whether it's actually empty or not,
        as the Null function will always return a true or false value,
        and only true or false, both of which are of type BoolType,
            so the type of calling Null on a list is BoolType.
            So we'll just return a BoolType. *)
    return BoolType
    (* Exercise 3 Part 4: Head
        Returns the type of the head of the list. *)

    (* Overview of variables:
          e: The list that we're using to get its head element's type *)

    (* Commands to see if it's working right and their results when using utop:

      utop # chk "hd(null?(emptylist int))";;
      - : texpr Checked.ReM.result =
      Error "The Head function says: List type expected!"

      utop # chk "hd(cons(zero?(1), emptylist bool))";;
      - : texpr Checked.ReM.result = Ok BoolType

      utop # chk "hd(cons(1, emptylist int))";;
      - : texpr Checked.ReM.result = Ok IntType

    *)

  | Hd(e) ->

    (* Get the type of the expression e *)
    type_of_expr e >>=
    (* Attempt to convert the expression e into a ListType.
        If the expression e is not a of type ListType,
          then the string provided after "arg_of_listType" will be concatenated with the error "List type expected!"
              The example of this working, as it's coded in the Hd function,
              would produce the following message: Error "The Head function says: List type expected!"
        Else, e is a ListType of some type.
            So bind the type of the elements in the list provided in e to the variable type_of_mylist_elements. *)

    list_of_listType "The Head function says: " >>= fun type_of_mylist_elements ->
    (* Now return type_of_mylist_elements,
        as the type of the head of the list an element of the list and is therefore of the type type_of_mylist_elements. *)
    return (type_of_mylist_elements)
  (* Exercise 3 Part 5: Tail
      Returns the type of the tail of the list *)

  (* Overview of variables:
      e: An expression that should corresponding to a list. Error checking will determine if it is in fact a list. *)

  (* Commands to see if it's working right and their results when using utop:

    utop # chk "tl(null?(emptylist int))";;
    - : texpr Checked.ReM.result =
    Error "The Tail function says: List type expected!"

    utop # chk "tl(cons(zero?(1), emptylist bool))";;
    - : texpr Checked.ReM.result = Ok ListType(BoolType)

    utop # chk "tl(cons (1, emptylist int))";;
    - : texpr Checked.ReM.result = Ok (ListType IntType)

  *)

  | Tl(e) ->
    (* Add a check to make sure e is a list like you did in Null *)

    (* Check the type of the expression e *)
    type_of_expr e >>=
    (* Attempt to convert e into a ListType and bind the result to mylist.
        If e is not a list, an error will be thrown by the arg_of_listType function
            indicating that the expression provided to the tail function is not a list.
        Else, bind e to the variable mylist. *)
    arg_of_listType "The Tail function says: " >>= fun type_of_mylist_elements ->

    (* Return the ListType whose elements have the type type_of_mylist_elements,
          as the tail of any list will retain the same type of elements that the original list had. *)
    return (ListType(type_of_mylist_elements))

  (* Exercise 4: Trees *)
  (* trees *)


  (* Exercise 4 Part 1: EmptyTree
      Returns the type of an empty tree of type t *)

  (* Overview if variables:
      t: A texpr that is the type of the elements that the empty tree will be able to contain *)

  (* Commands to see if it's working right and their results when using utop:

    utop # chk "emptytree int";;
    - : texpr Checked.ReM.result = Ok (TreeType IntType)

    utop # chk "emptytree bool";;
    - : texpr Checked.ReM.result = Ok (TreeType BoolType)

    utop # chk "emptytree unit";;
    - : texpr Checked.ReM.result = Ok (TreeType UnitType)

  *)

  | EmptyTree(t) ->
    (* Return the TreeType(t) *)
    return (TreeType t)

  (* Exercise 4 Part 2: Node
      Creates a new node in the tree made up of the type of the data, the type of the left subtree, and the type of the right subtree. *)

  (* Overview of variables:
      de: An expressed value that contains the data for the node we are defining
      le: An expressed value that contains (or rather SHOULD contain) the left subtree for the node we are defining
      re: An expressed value that contains (or rather SHOULD contain) the right subtree for the node we are defining
   *)

   (* Commands to see if it's working right and their results when using utop:

     utop # chk "node(zero?(0), zero?(1), emptytree bool)";;
     - : texpr Checked.ReM.result =
     Error "Referring to the provided 'left subtree', the Node function says: Tree type expected!"

     utop # chk "node(1, node(2, emptytree int, node(3, emptytree bool, emptytree int)), emptytree int)";;
     - : texpr Checked.ReM.result =
     Error "The Node function says: Elements of the provided do not have the same type!"

     utop # chk "node(0, emptytree unit, emptytree unit)";;
     - : texpr Checked.ReM.result =
     Error "The Node function says: Elements of the provided do not have the same type!"

     utop # chk "node(zero?(1), emptytree bool, emptytree bool)";;
     - : texpr Checked.ReM.result = Ok (TreeType BoolType)

     utop # chk "node(1, node(2, emptytree int, emptytree int), emptytree int)";;
     - : texpr Checked.ReM.result = Ok (TreeType IntType)

     utop # chk "node(1, node(2, emptytree int, node(3, emptytree int, emptytree int)), emptytree int)";;
     - : texpr Checked.ReM.result = Ok (TreeType IntType)

     utop # chk "nullT?(node(1, emptytree int, emptytree int))";;
     - : texpr Checked.ReM.result = Ok BoolType

     utop # chk "nullT?(node(1, node(2, emptytree int, emptytree int), emptytree int))";;
     - : texpr Checked.ReM.result = Ok BoolType

     utop # chk "nullT?(node(zero?(1), emptytree bool, emptytree bool))";;
     - : texpr Checked.ReM.result = Ok BoolType

   *)

  (* Important thing to Note: All elements, subtrees,
      and everything in a given tree MUST have the same type.
      You CANNOT mix types of elements in a tree. *)
  | Node(de, le, re) ->
    (* Get the type of the expression de and store whatever type it has in the variable type_of_root_data *)
    type_of_expr de >>= fun type_of_root_data ->
    (* Now begin type checking the left subtree *)
    type_of_expr le >>=
    (* If le is indeed a tree, then retrieve the type of its elements and bind the type to type_of_left_subtree_elements.
          Else, an error will be thrown by the arg_of_treeType function indicating that le is not a tree. *)
    arg_of_treeType "Referring to the provided 'left subtree', the Node function says: " >>= fun type_of_left_subtree_elements ->
    (* Now begin type checking the right subtree *)
    type_of_expr re >>=
    (* If re is indeed a tree, then retrieve the type of its elements and bind the type to type_of_right_subtree_elements.
          Else, an error will be thrown by the arg_of_treeType function indicating that re is not a tree. *)
    arg_of_treeType "Referring to the provided 'right subtree', the Node function says: " >>= fun type_of_right_subtree_elements ->
    (* Now, since all elements of a binary tree must have the same type, check
        if this condition does not hold for the type_of_root_data, the type_of_left_subtree_elements, and the type_of_right_subtree_elements,
          i.e. if the type_of_root_data != type_of_left_subtree_elements != type_of_right_subtree_elements,
            or, for syntactical purposes,
            if ((type_of_root_data != type_of_left_subtree_elements) || (type_of_root_data != type_of_right_subtree_elements))),
              since this should yield (false || false), since we're looking for whether or not ANY of these are not equal to each other.
              Only if all of the types are equal can we proceed to the else statement, which will create the node.
                So it's basically the logical negation of the statement (true && true), since that would be if all of the types are equal.
     *)
     if ((type_of_root_data <> type_of_left_subtree_elements) || (type_of_root_data <> type_of_right_subtree_elements))
     (* Then at least one of the data types are not equal to the others, so an error must be thrown. *)
     then (error "The Node function says: The elements provided do not all have the same type!")
     (* Else, all of the types in the elements of the binary tree are the same.
          So we can create and return this tree,
            which will have the type_of_root_data,
                              a left subtree of type TreeType of the type_of_left_subtree_elements,
                              and a right subtree of type TreeType of the type_of_right_subtree_elements. *)
     else return (TreeType(type_of_root_data))

  (* Exercise 4 Part 3: Null Tree
      Returns the type of the result of checking if the tree provided is empty *)

  (* Overview of variables:
      t: An expression. It should be of type TreeType, but other bad types can be fed to the function. *)

  (* Commands to see if it's working right and their results when using utop:

    utop # chk "nullT?(zero?(1))";;
    - : texpr Checked.ReM.result =
    Error "The NullT function says: Tree type expected!"

    utop # chk "nullT?(emptytree int)";;
    - : texpr Checked.ReM.result = Ok BoolType

    utop # chk "nullT?(emptytree bool)";;
    - : texpr Checked.ReM.result = Ok BoolType

    utop # chk "nullT?(emptytree unit)";;
    - : texpr Checked.ReM.result = Ok BoolType

    utop # chk "nullT?(node(1, emptytree int, emptytree int))";;
    - : texpr Checked.ReM.result = Ok BoolType

    utop # chk "nullT?(node(1, node(2, emptytree int, emptytree int), emptytree int))";;
    - : texpr Checked.ReM.result = Ok BoolType

    utop # chk "nullT?(node(zero?(1), emptytree bool, emptytree bool))";;
    - : texpr Checked.ReM.result = Ok BoolType

  *)

  | NullT(t) ->
    (* Get the type of the expression t *)
    type_of_expr t >>=
    (* Check if the expression t is indeed a tree *)
    arg_of_treeType "The NullT function says: " >>= fun _type_of_mytree_elements ->

    (* Now disregard whether it's actually empty or not,
        as the NullT function will always return a true or false value,
        and only true or false, both of which are of type BoolType,
            so the type of calling NullT on a tree is BoolType.
            So we'll just return a BoolType. *)
    return BoolType

  (* Exercise 4 Part 4: GetData
      Retrieves the type of elements that the tree provided can hold,
          as the root_data is among those elements and they must all be the same type. *)

  (* Overview of variables:
      t: An expression that holds the tree to get data from.
            It should be of type TreeType, but it should be checked for erroneous input just in case.
  *)

  (* Commands to see if it's working right and their results when using utop:

    utop # chk "getData(emptylist int)";;
    - : texpr Checked.ReM.result =
    Error "The GetData function says: Tree type expected!"

    utop # chk "getData(node(1, node(2, emptytree int, emptytree int), emptytree int))";;
    - : texpr Checked.ReM.result = Ok IntType

  *)

  | GetData(t) ->
    (* Get the type of the expression t *)
    type_of_expr t >>=
    (* Check if the expression is indeed a tree and, if it is, store the type of elements that the tree can hold in type_of_mylist_elements *)
    arg_of_treeType "The GetData function says: " >>= fun type_of_mytree_elements ->
    (* Now return the type of elements that the tree can hold,
        again because the root_data is among those elements and all elements of a binary tree must have the same type. *)
    return type_of_mytree_elements

  (* Exercise 4 Part 5: Get Left SubTree
      Retrieves the type of the left subtree. *)

  (* Overview of variables:
      t: An expression that holds the tree to get the left subtree from.
        It should be of type TreeType, but it should be checked for erroneous input just in case.
  *)

  (* Commands to see if it's working right and their results when using utop:

    utop # chk "node(zero?(0), zero?(1), emptytree bool)";;
    - : texpr Checked.ReM.result =
    Error "Referring to the provided 'left subtree', the Node function says: Tree type expected!"

    utop # chk "node(zero?(1), emptytree bool, emptytree bool)";;
    - : texpr Checked.ReM.result = Ok (TreeType BoolType)

    utop # chk "node(unit, emptytree unit, emptytree unit)";;
    - : texpr Checked.ReM.result = Ok (TreeType UnitType)

    utop # chk "node(1, node(2, emptytree int, emptytree int), emptytree int)";;
    - : texpr Checked.ReM.result = Ok (TreeType IntType)

    utop # chk "node(1, node(2, emptytree int, node(3, emptytree int, emptytree int)), emptytree int)";;
    - : texpr Checked.ReM.result = Ok (TreeType IntType)

    utop # chk "getLST(node (1, node (2, emptytree int, emptytree int), emptytree int))";;
    - : texpr Checked.ReM.result = Ok (TreeType IntType)

  *)

  | GetLST(t) ->
    (* Get the type of the expression t *)
    type_of_expr t >>=
    (* Check if the expression is indeed a tree and, if it is, store the type of elements that the tree can hold in type_of_mylist_elements *)
    arg_of_treeType "The GetLST function says: " >>= fun type_of_mytree_elements ->
    (* Now return the TreeType that of type_of_mytree_elements,
        as every subtree of a binary tree can only contain the same type of elements as the parent tree. *)
    return (TreeType(type_of_mytree_elements))

    (* Exercise 4 Part 6: Get Right SubTree
        Retrieves the type of the right subtree. *)

    (* Overview of variables:
        t: An expression that holds the tree to get the right subtree from.
          It should be of type TreeType, but it should be checked for erroneous input just in case.
    *)

    (* Commands to see if it's working right and their results when using utop:

      utop # chk "node(zero?(0), zero?(1), emptytree bool)";;
      - : texpr Checked.ReM.result =
      Error "Referring to the provided 'left subtree', the Node function says: Tree type expected!"

      utop # chk "node(1, node(2, emptytree int, node(3, emptytree bool, emptytree int)), emptytree int)";;
      - : texpr Checked.ReM.result =
      Error "The Node function says: Elements of the provided do not have the same type!"

      utop # chk "node(zero?(1), emptytree bool, emptytree bool)";;
      - : texpr Checked.ReM.result = Ok (TreeType BoolType)

      utop # chk "node(unit, emptytree unit, emptytree unit)";;
      - : texpr Checked.ReM.result = Ok (TreeType UnitType)

      utop # chk "node(1, node(2, emptytree int, emptytree int), emptytree int)";;
      - : texpr Checked.ReM.result = Ok (TreeType IntType)

      utop # chk "node(1, node(2, emptytree int, node(3, emptytree int, emptytree int)), emptytree int)";;
      - : texpr Checked.ReM.result = Ok (TreeType IntType)

      utop # chk "getRST(node (1, node (2, emptytree int, emptytree int), emptytree int))";;
      - : texpr Checked.ReM.result = Ok (TreeType IntType)


    *)
  | GetRST(t) ->
    (* Get the type of the expression t *)
    type_of_expr t >>=
    (* Check if the expression is indeed a tree and, if it is, store the type of elements that the tree can hold in type_of_mylist_elements *)
    arg_of_treeType "The GetRST function says: " >>= fun type_of_mytree_elements ->
    (* Now return the TreeType that of type_of_mytree_elements,
        as every subtree of a binary tree can only contain the same type of elements as the parent tree. *)
    return (TreeType(type_of_mytree_elements))

  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> error "type_of_expr: implement"

let type_of_prog (AProg e) =  type_of_expr e



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> type_of_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> type_of_prog
  in run_teac (c >>= fun t -> return @@ Ast.string_of_texpr t)


let ex1 = "
let x = 7
in let y = 2
   in let y = let x = x-1
              in x-y
      in (x-8)- y"

let ex2 = "
   let g =
      let counter = 0
      in proc(d:int) {
         begin
           set counter = counter+1;
           counter
         end
         }
   in (g 11)-(g 22)"

let ex3 = "
  let g =
     let counter = newref(0)
     in proc (d:int) {
         begin
          setref(counter, deref(counter)+1);
          deref(counter)
         end
       }
  in (g 11) - (g 22)"

let ex4 = "
   let g =
      let counter = 0
      in proc(d:int) {
         begin
           set counter = counter+1;
           counter
         end
         }
   in (proc (x:int) { x + x }
(g 0))"
(* 3 in call-by-name *)
(* 2 in call-by-need *)

let ex5 = "
let a = 3
in let p = proc(x) { set x = 4 }
in begin
         (p a);
         a
       end"

let ex6 = "let p = proc(x:int) { 5-x } in (p 3)"
(* 2 *)

let ex7 = "
let a = 3
in let f = proc(x:int) { proc(y:int) { set x = x-y }}
in begin
((f a) 2);
a
end"
(* 1 *)

let ex8 = "
let swap = proc (x:int) { proc (y:int) {
                      let temp = x
                      in begin
                          set x = y;
                          set y = temp
                         end
                      }
            }
         in let a = 33
         in let b = 44
         in begin
             ((swap a) b);
             a-b
            end"
(* 11 *)

let ex9 = "
letrec fact (x) = if zero?(x) then 1 else x*(fact (x-1))
in (fact 7)"
(* 5040 *)

let ex10 = "
letrec infiniteLoop (x) = (infiniteLoop (x+1))
in let f = proc (z) { 11 }
in (f (infiniteLoop 0))"
