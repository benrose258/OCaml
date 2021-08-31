(*

(* Ben Rose *)
(* CS 496 Homework 2 *)
(* "General Trees" Implementation *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)
(* March 3, 2021 *)

   Stub for HW2
   Please
   1. Rename to gt.ml
   2. Place the names of the group members here:

    Name1: Ben Rose
    Name2: Ben Rose

*)



type 'a gt = Node of 'a*('a gt) list

(* Sample function that given some n builds a general tree that is a leaf holding n as its data. *)
let mk_leaf (n:'a) : 'a gt =
  Node(n,[])

(* Note: General Trees appear to be defined in a pre-order fashion,
   starting with the root, then the leftmost node, its children, second leftmost, ... last leftmost (i.e. rightmost).
   Follow that procedure as well for the definition of the child trees. *)
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77,
              [Node (37,
                     [Node (14, [])]);
               Node (48, []);
               Node (103, [])])
       ])

(* Begin my defined trees *)

let t2 : int gt =
Node (33,
      [Node (12,[]);
       Node (77,
             [Node (37, []);
              Node (48, []);
              Node (103, [])])
      ])

let perfect_t : int gt =
 Node (33,
       [Node (12,
              [Node (6,
                     [Node (3, [])])]);

        Node (77,
              [Node (37,
                     [Node (14, [])]);
               Node (48,
                     [Node (128, [])]);
               Node (103,
                     [Node (256, [])])])
       ])

 let imperfect_rt : int gt =
  Node (33,
        [Node (12,
               [Node (6,
                      [Node (3, [])])]);

         Node (77,
               [Node (37, []);
                Node (48,
                      [Node (128, [])]);
                Node (103, [])])
        ])

(* Checks if the node that is currently visited is a leaf.
    Meant to be used recursively to find out if a tree has no children, making it a leaf.
  Made by me, but not a part of any single exercise. *)
let is_leaf (mytree:('a gt)) : bool =
  (* Match on the tree mytree *)
  match mytree with
  (* If mytree = Node(some_data, []), i.e. the tree has root data but no child nodes,
                    return true, since that's the definition of a leaf *)
  | Node(root_data,[]) -> true
  (* If mytree = Node(some_data, child_nodes), then there's some child nodes,
            so return false since this node is not a leaf *)
  | Node(root_data,child_node_list) -> false

(* Exercise 1: height *)


(* Recursively searches the rest of an int list to find its max element and returns current_max when rest_of_my_list is empty, as we
                                                                                                      have then searched every element in it. *)
let rec max_of_int_list_helper (rest_of_my_list:int list) (current_max:int) : int =
  (* Begin matching on the 'int list' rest_of_my_list *)
  match rest_of_my_list with
  (* If the list is empty, then the current_max is the maximum element of the list, so return it *)
  | [] -> current_max
  (* Else the list has a head (i.e. potential_new_max) and a tail (i.e. rest_of_list),
        so we need to check if the head's value is greater than current_max *)
  | potential_new_max::rest_of_list ->
    (* If the potential_new_max > current_max *)
    if (potential_new_max > current_max)
    (* Then call max_of_int_list_helper over the rest_of_list with the new max being potential_new_max *)
    then max_of_int_list_helper rest_of_list potential_new_max
    (* Else the potential_new_max < current_max, so the current_max is still the maximum value in the list,
                so call max_of_int_list_helper over the rest_of_list with the max continuing to be current_max *)
    else max_of_int_list_helper rest_of_list current_max

(* Gets the max of a list of integers.
   NOTE: This function assumes you have at least one element in the list and fails otherwise. *)
let max_of_int_list (my_list:int list) : int =
  (* Begin matching on the 'int list' my_list *)
  match my_list with
  (* If there are no elements in my_list, return an error, since there were no elements to start with. *)
  | [] -> failwith "No elements in the list."
  (* Else, the list has a head (i.e. first_int) and a tail (i.e. rest_of_ints) *)
  | first_int::rest_of_ints ->
    (* So call the helper function that uses the first element of my_list as the current max
                                                  and compares it with the rest of the integers in rest_of_ints
                                                  to determine if the first_int is the max.
                                                      If the first_int is the maximum value in my_list
                                                          after recursively comparing it to every element in rest_of_ints,
                                                      then return first_int, as it is the maximum.
                                                      Else, return the max item in the rest of the list through recursive checks. *)
    max_of_int_list_helper rest_of_ints first_int

(* (NOTE: BECAUSE THIS FUNCTION (i.e. height_child_caller) CALLS THE height FUNCTION
                                            AND THE height FUCTION CALLS THIS FUNCTION (i.e. height_child_caller),
                                            THE TWO FUNCTIONS MUST BE DEFINED RECURSIVELY (i.e. ONE NESTED IN THE OTHER),
                                            BECAUSE DEFINING HELPER FUNCTIONS MUST EITHER BE DEFINED RECURSIVELY OR
                                            THE HELPER FUNCTION MUST BE DEFINED ABOVE THE MAIN FUNCTION. BECAUSE THE HELPER FUNCTION
                                            AND THE MAIN FUNCTION BOTH CALL EACH OTHER, THE TWO BEING ABOVE EACH OTHER IS IMPOSSIBLE
                                            AND THEY MUST BE DEFINED RECURSIVELY (i.e. ONE NESTED IN THE OTHER). *)
(* Calls height for every child_node in the child_node_list and returns the maximum of the height of all child nodes. *)
(* let rec height_child_caller (child_node_list:'a gt list) : int list =
  (* Begin matching on the list child_node_list *)
  match child_node_list with
  (* If the list is empty (i.e. we have called the "height" function on all of the child nodes)
          then return the empty list, since we have constructed the list of child node heights.*)
  | [] -> []
  (* Else, we still have at least one child_node,
     so we split up the list into a head (i.e. first_child_node) and the tail (i.e. other_child_nodes) *)
  | first_child_node::other_child_nodes ->
    (* Concatenate the height of first_child_node with a recursive call of height_child_caller over the other_child_nodes *)
    (height first_child_node)::(height_child_caller other_child_nodes) *)


(* Input: A general tree of any type (i.e. ('a gt)) *)
(* Output: the height of that tree, i.e. the length of the longest path (in terms of number of nodes) from the root to the leaf *)

(* Commands to see if it's working right and their results when using utop:
    utop # height t;;
    - : int = 4
    utop # height (mk_leaf "testing");;
    - : int = 1
    utop # height t2;;
    - : int = 3
    utop # height perfect_t;;
    - : int = 4
  *)
let rec height (t:('a gt)) : int =
(* let rec height t : int = *)
  (* Begin matching on the tree t *)
  match t with
  (* For the case where t is a node with the value root_data and children child_node_list (i.e. every case) *)
  | Node(root_data, child_node_list) ->
    (* If we've reached a leaf *)
    if (is_leaf t)
    (* Then return 1, indicating that we have reached the end of this path through the tree *)
    then 1
    (* Else, there are still elements in the tree, so return 1 + the maximum height of every child_node using height_child_caller.
        Note: you might need to make a max function for the type 'int list'
        ^^^ This was right. The function to do so is called max_of_int_list. *)
    (* else 1 + max_of_int_list(height_child_caller(child_node_list)) *)
    (* else 1 + max (height_child_caller(child_node_list)) *)
    (* (NOTE: BECAUSE THIS FUNCTION (i.e. height) CALLS THE height_child_caller FUNCTION
                                                AND THE height_child_caller FUCTION CALLS THIS FUNCTION (i.e. height),
                                                THE TWO FUNCTIONS MUST BE DEFINED RECURSIVELY (i.e. ONE NESTED IN THE OTHER),
                                                BECAUSE DEFINING HELPER FUNCTIONS MUST EITHER BE DEFINED RECURSIVELY OR
                                                THE HELPER FUNCTION MUST BE DEFINED ABOVE THE MAIN FUNCTION. BECAUSE THE HELPER FUNCTION
                                                AND THE MAIN FUNCTION BOTH CALL EACH OTHER, THE TWO BEING ABOVE EACH OTHER IS IMPOSSIBLE
                                                AND THEY MUST BE DEFINED RECURSIVELY (i.e. ONE NESTED IN THE OTHER). *)
    (* So, now this statement still says the same thing as I mentioned above the "NOTE", it's defined slightly differently.
       The first part, adding 1 to the max_of_int_list of all the child nodes is the same, but inside the parentheses
       has to be (I think) the definition of the helper function,
       since the helper function must be immidiately followed by "in <helper function name> helper_arg1 helper_arg2 ...".
       In this case, the command is "in height_child_caller child_node_list", but obviously written without the quotes.
       So it's doing the same thing as the previous draft of this line,
                                                          i.e. (* else 1 + max_of_int_list(height_child_caller(child_node_list)) *),
       but since the function (i.e. height) and the helper function (i.e. height_child_caller) call each other, they must be nested
       like this. *)
    else 1 + max_of_int_list(

    let rec height_child_caller (child_node_list:'a gt list) : int list =
      (* Begin matching on the list child_node_list *)
      match child_node_list with
      (* If the list is empty (i.e. we have called the "height" function on all of the child nodes)
              then return the empty list, since we have constructed the list of child node heights.*)
      | [] -> []
      (* Else, we still have at least one child_node,
         so we split up the list into a head (i.e. first_child_node) and the tail (i.e. other_child_nodes) *)
      | first_child_node::other_child_nodes ->
        (* Concatenate the height of first_child_node with a recursive call of height_child_caller over the other_child_nodes *)
        (height first_child_node)::(height_child_caller other_child_nodes)
    (* Now from the parent function (i.e. height) call the nestedly defined helper function (i.e. height_child_caller)
                                                            with the argument "child_node_list", obviously without the quotes. *)
    in height_child_caller child_node_list)
  (* failwith "implement" *)

(* Exercise 2 *)

(* Returns the number of child_nodes in the list child_node_list. *)
(* let rec count_child_nodes (child_node_list:'a gt list) : int =
  (* Begin matching on the list child_node_list *)
  match child_node_list with
  (* If the list child_node_list is empty (i.e. we've checked every child node in this branch),
            then return 0, since there are no more nodes to check *)
  | [] -> 0
  (* Else, we have at least one child node left,
     so we split up the list into a head (i.e. first_child_node) and the tail (i.e. other_child_nodes) *)
  | first_child_node::other_child_nodes ->
  (* Add 1 to the recursive call of count_child_nodes using the list other_child_nodes.
     This is because the "1" is counting the first_child_node in the list
                    and the recursive call will do the same for all of the other_child_nodes *)
    (size first_child_node) + (count_child_nodes other_child_nodes) *)

(* Input: A general tree of any type (i.e. ('a gt)) *)
(* Output: the size of that tree, i.e. the number of nodes in the general tree 't'. *)

(* Commands to see if it's working right and their results when using utop:
    utop # size t;;
    - : int = 7
    utop # size (mk_leaf "testing");;
    - : int = 1
    utop # size t2;;
    - : int = 6
    utop # size perfect_t;;
    - : int = 11
  *)

let rec size (t:('a gt)) : int =
  (* Begin matching on the tree t *)
  match t with
  (* For the case where t is a node with the value root_data and children child_node_list (i.e. every case) *)
  | Node(root_data, child_node_list) ->
  (* Return 1 + the number of child_nodes, computed using the count_child_nodes function over the child_node_list
    The "1" denotes us counting the root of the tree, and the count_child_nodes function will count all of the child_nodes, if any exist. *)
    (* 1 + (count_child_nodes child_node_list) *)
    1 + (
       (* Returns the number of child_nodes in the list child_node_list using a recursive call to itself and its parent function "size."
          It is defined within the "size" function since the function "count_child_nodes" and the function "size" both make recursive
          calls to one another. Because OCaml syntax requires that all external helper functions be defined above,
          and since the "count_child_nodes" function can't be both defined above the "size" function and below the "size" function,
          the "count_child_nodes" function must be defined within the "size" function. *)
      let rec count_child_nodes (child_node_list:'a gt list) : int =
        (* Begin matching on the list child_node_list *)
        match child_node_list with
        (* If the list child_node_list is empty (i.e. we've checked every child node in this branch),
                 then return 0, since there are no more nodes to check *)
        | [] -> 0
        (* Else, we have at least one child node left,
          so we split up the list into a head (i.e. first_child_node) and the tail (i.e. other_child_nodes) *)
        | first_child_node::other_child_nodes ->
        (* Call the size function on the first_child_node, which will effectively
           add 1 to the recursive call of count_child_nodes using the list of the other_child_nodes.
           This is because calling "size" on the first_child_node is counting the first_child_node in the list
                         and the recursive call will do the same for all of the other_child_nodes *)
          (size first_child_node) + (count_child_nodes other_child_nodes)
    in count_child_nodes child_node_list)
  (* failwith "implement" *)

(* Exercise 3 *)


(* Iterate over the different nodes in child_node_list.
    If the child_node_list is empty, return the empty list.
    If you move from left to right but stay on the same depth level, increment the child number with each recursive call.
    If you are moving one level down in terms of depth,
          create a list for that branch by appending the current child_number to the front of the list containing *)
let rec paths_to_leaf_children (child_node_list:'a gt list) (child_number:int) (current_path:int list) : int list list =
  (* Begin matching on the list child_node_list *)
  match child_node_list with
  (* If the child_node_list is empty, then return the current_path,
                        as we've recursively called child_node_list over the entire branch of the tree *)
  | [] -> []
  (* Else, we still have at least one child node (the head of the list of ('a gt)s)
                                                              and a list of other_child_nodes (the tail of the list of ('a gt)s) *)
  | first_child_node::other_child_nodes ->
    (* Now begin matching on the first_child_node, which as you remember is of type ('a gt) *)
    match first_child_node with
    (* Split the first_child_node into its root_data (i.e. first_child_data) and its own child_node_list (i.e. first_childs_children) *)
    | Node(first_child_data, first_childs_children) ->
      (* If the first_child_node is a leaf *)
      if (is_leaf first_child_node)
      (* Then construct a list by doing the following steps:
          Create a list containing the current_path @ppended to a list containing the current child_number
                                                                                        as we have reached the end of the path to this leaf,
          @ppended with
          Calling paths_to_leaf_children on the other_child_nodes (i.e. moving to any other children on the same level),
                                  with (child_number + 1), since we're moving one node over,
                                        and the current_path, since first_child_node is NOT a parent of a node next to it.
             *)
      then [current_path @ [child_number]]
           @
           (paths_to_leaf_children other_child_nodes (child_number + 1) current_path)
      (* Else, construct a list by doing the following function calls:
            Call paths_to_leaf_children on the first_childs_children,
                          starting at the 0th child (i.e. child_number = 0),
                                    using the (current_path @ppended with a list containing the current child_number,
                                    since the path to the leaf will be using the first_child_node as the path's parent.
            @ppended with
            Calling paths_to_leaf_children on the other_child_nodes (i.e. moving to any other children on the same level),
                                    with (child_number + 1), since we're moving one node over,
                                          and the current_path, since first_child_node is NOT a parent of a node next to it.
              *)
      else (paths_to_leaf_children first_childs_children 0 (current_path @ [child_number]))
           @
           (paths_to_leaf_children other_child_nodes (child_number + 1) current_path)

(* Input: A general tree of any type (i.e. ('a gt)) *)
(* Output: A list of int lists that denote all of the paths to leaves of the tree from the tree's root. *)

(* Commands to see if it's working right and their results when using utop:
    utop # paths_to_leaves t;;
    - : int list list = [[0]; [1; 0; 0]; [1; 1]; [1; 2]]
    utop # paths_to_leaves (mk_leaf "testing");;
    - : int list list = [[]]
    utop # paths_to_leaves t2;;
    - : int list list = [[0]; [1; 0]; [1; 1]; [1; 2]]
    utop # paths_to_leaves perfect_t;;
    - : int list list = [[0; 0; 0]; [1; 0; 0]; [1; 1; 0]; [1; 2; 0]]
  *)

let rec paths_to_leaves (t:'a gt) : int list list =
  (* Begin matching on the tree t *)
  match t with
  (* For the case where t is a node with the value root_data and children child_node_list (i.e. every case) *)
  | Node(root_data, child_node_list) ->
  (* NOTE: Disregard the following note, because the tree with one node should return [[]],
          just as it does in the btree implementation from class.
    NOTE: The if-then-else that was formerly here has been handled in the paths_to_leaf_children function already,
                          so only the line directly below this note should be needed. *)
    (* paths_to_leaf_children child_node_list 0 [] *)

    (* If the tree that we're starting with is itself a leaf *)
    if (is_leaf t)
    (* Then return the empty list of list, since you don't have to go anywhere to get to a leaf,
                                                            as you're already at the one leaf in the tree.
                    The decision to make this case [[]] versus [] came down to the implementation of paths_to_leaves in
                        class, where we used binary trees and paths_to_leaves of a tree with a root but no left or right children
                                                        (i.e. a leaf) returned [[]], and that's how the roots of general trees work. *)
    then [[]]
    (* Else begin going down the different paths to the child leaves with the starting branch number being 0, as it's the first branch. *)

    (* final_list = []
      for (int i = 0; i < length(child_node_list); i++) {
        final_list.append(paths_to_leaf_children(child_node_list[i], [], i));
      } *)
    else paths_to_leaf_children child_node_list 0 []
  (* failwith "implement" *)

(* Exercise 4 *)

(* list_length Version 1.1:
    Changed the definition of "first_element" to "_first_element" to avoid warnings, as the
                                          variable "first_element" is never used. *)
(* Given any list my_list, this function recursively finds and returns the length of the list. *)
let rec list_length (my_list:'a list) : int =
  (* Begin matching on the list my_list *)
  match my_list with
  (* If my_list is empty, which means the function was either fed an empty list or we have finished the recursive calls,
                  return 0 *)
  | [] -> 0
  (* Else my_list has at least one element in it. So break my_list into a head (i.e. first_element) and a tail (i.e. rest_of_my_list) *)
  | _first_element::rest_of_my_list ->
    (* Return 1 + a recursive call of list_length over the rest_of_my_list *)
    1 + (list_length rest_of_my_list)

(* NOTE: This function has been updated. The following version is list_length 1.0 *)
(* Given any list my_list, this function recursively finds and returns the length of the list. *)
(* let rec list_length (my_list:'a list) : int =
  (* Begin matching on the list my_list *)
  match my_list with
  (* If my_list is empty, which means the function was either fed an empty list or we have finished the recursive calls,
                  return 0 *)
  | [] -> 0
  (* Else my_list has at least one element in it. So break my_list into a head (i.e. first_element) and a tail (i.e. rest_of_my_list) *)
  | first_element::rest_of_my_list ->
    (* Return 1 + a recursive call of list_length over the rest_of_my_list *)
    1 + (list_length rest_of_my_list) *)

(* Determines if the length of every sublist in my_list has length equal to "required_sublist_length" *)
let rec all_sublists_same_length_helper (my_list:'a list list) (required_sublist_length:int) : bool =
  (* Begin matching on the list my_list *)
  match my_list with
  (* If my_list is empty, i.e. we've checked every sublist or there were none to check in the first place, return true *)
  | [] -> true
  (* Else, my_list has a head (i.e. first_sublist) and a tail (i.e. other_sublists) *)
  | first_sublist::other_sublists ->
    (* If the length of the first_sublist is equal to the required_sublist_length *)
    if ((list_length first_sublist) = required_sublist_length)
    (* Then do a recursive call of all_sublists_same_length_helper using "other_sublists" and "required_sublist_length" as arguments *)
    then all_sublists_same_length_helper other_sublists required_sublist_length
    (* Else, the length of all sublists are not equal to the required_sublist_length (i.e. the length of the first sublist if called
                                                                                          from its parent function),
                                                            so return false. *)
    else false

(* Determines if, in a list containing lists, every sublist is the same length. *)
let all_sublists_same_length (my_list:'a list list) : bool =
  (* Begin matching on the list my_list *)
  match my_list with
  (* If the list is empty to begin with, return true, because all 0 lists are the same length. *)
  | [] -> true
  (* Else, the list has a head (i.e. first_sublist) and a tail (i.e. other_sublists) *)
  | first_sublist::other_sublists ->
    (* Call the helper function all_sublists_same_length_helper with the parameters:
                                          other_sublists
                                          list_length(first_sublist)
                      Note that having the first argument being my_list would work as well, but it is redundant in checking the first element.
                      The "other_sublists" part checks the length of the first_sublist against the length of the rest of my_list.
                      The "list_length(first_sublist)" will be the length to check against when looking at the length of the other_sublists. *)
    all_sublists_same_length_helper other_sublists (list_length first_sublist)

(* Input: A general tree of any type (i.e. ('a gt)) *)
(* Output: A boolean value where, if all of the leaves of the tree have the same depth (i.e. the length of the path to each leaf is the same)
                                          the tree is perfect and the value "true" is returned,
                                  else the tree is not perfect, so return false. *)

(* Commands to see if it's working right and their results when using utop:
    utop # is_perfect t;;
    - : bool = false
    utop # is_perfect t2;;
    - : bool = false
    utop # is_perfect imperfect_rt;;
    - : bool = false
    utop # is_perfect (mk_leaf "testing");;
    - : bool = true
    utop # is_perfect perfect_t;;
    - : bool = true
  *)
let rec is_perfect (t:'a gt) : bool =
  (* See if the length of the list of lists returned by paths_to_leaves "t" are all equal.
    If they are, return true. Else, return false. *)
  all_sublists_same_length(paths_to_leaves t)
  (* failwith "implement" *)

(* Exercise 5 *)

(* Input: A general tree of any type (i.e. ('a gt)) *)
(* Output: The preorder traversal of the input tree. For example,
                                                      the preorder traversal of a binary tree goes in the order root, left, right
            The preorder traversal follows a similar concept, first visiting the root, then the leftmost node, then the second leftmost node,
                                                                                    and so on until you reach the rightmost node.
            This process is done recursively and returned as a single list of type "'a", which is the type that the general tree has.
            Also, the preorder traversal will go in order of how the general tree "t" and other trees are defined,
                                                        when you look at the nodes one line number at a time. *)

(* NOTE: THE FUNCTION "f" IN "ex -- Updated new Canvas version 2-19-2021" THAT TAKES A BINARY TREE AS AN INPUT IS THE
BINARY TREE VERSION OF PREORDER TRAVERSALS *)

(* Commands to see if it's working right and their results when using utop:

    utop # preorder t;;
    - : int list = [33; 12; 77; 37; 14; 48; 103]
    utop # preorder t2;;
    - : int list = [33; 12; 77; 37; 48; 103]
    utop # preorder perfect_t;;
    - : int list = [33; 12; 6; 3; 77; 37; 14; 48; 128; 103; 256]
    utop # preorder imperfect_rt;;
    - : int list = [33; 12; 6; 3; 77; 37; 48; 128; 103]
    utop # preorder (mk_leaf "testing");;
    - : string list = ["testing"]
  *)
(* let rec preorder (Node(d,ch)) = *)
let rec preorder (t:'a gt) : 'a list =
  (* Begin matching on the tree t *)
  match t with
  (* For the case where t is a node with the value root_data and children child_node_list (i.e. every case) *)
  | Node(root_data, child_node_list) ->
    (* If the tree t is a leaf *)
    if (is_leaf t)
    (* Then we have finished recursive calls on this branch, so concatenate the root_data to the rest of the list *)
    then [root_data]
    (* Else, there is at least one child node in the tree, so create a list as follows:
       Make a list containing the root_data (i.e. [root_data])
       @ppended to
       a recursive call of preorder on the leftmost node
       @ppended to a function that is defined below.
            checks if the rest of the child_node_list is empty,
                      If it is, then return the empty list to be appended to the list at the beginning of this "else" case.
                  Otherwise, call preorder on the first element of the rest of the child_node_list
                  @ppended to a call of the preorder helper function over the remaining part of the child_node_list. *)
      else [root_data] @ (
        (* NOTE that this function must be defined within the preorder function
                    because preorder_child_caller calls preorder and itself and preorder would call preorder_child_caller
                    and, since OCaml syntax dictates that any helper functions must be defined above the main function
                          and two functions can each be defined above each other, they must be nested. *)
          (* preorder_child_caller first checks if the rest of the child_node_list is empty.
                      If it is, then return the empty list to be appended to the list at the beginning of this "else" case.
              Otherwise, there is at least one element in the child_node_list.
                    So call preorder on the first element of the rest of the child_node_list and
              @ppend that to
              a call of the preorder helper function over the other_child_nodes. *)
          let rec preorder_child_caller (child_node_list:'a gt list) : 'a list =
            (* Begin matching on the list child_node_list *)
            match child_node_list with
            (* If the list is empty (i.e. we've called preorder_child_caller on all of the nodes in the child_node_list),
                            return the empty list *)
            | [] -> []
            (* Else, there is at least one element in the child_node_list *)
            | first_child_node::other_child_nodes ->
              (* So construct a list as follows:
                 Do a recursive call of the preorder function on the first_child_node
                 @ppended to
                 A recursive call of preorder_child_caller on the list other_child_nodes. *)
              (preorder first_child_node)
              @
              (preorder_child_caller other_child_nodes)
        (* Now from the parent function (i.e. preorder)
              call the nestedly defined helper function (i.e. preorder_child_caller)
              using the argument child_node_list, as we defined at the beginning of the preorder function. *)
        in preorder_child_caller child_node_list)
    (* failwith "implement" *)

(* Exercise 6 *)

(* Defining a function to get the nth element of the list "my_list" and return that element *)
let rec get_nth_element (my_list:'a list) (index:int) : 'a =
  (* Begin matching on my_list *)
    match my_list with
    (* If my_list == [], then return an error, as the index provided is out of the bounds of the list *)
    | [] -> failwith "Error: The provided index is out of bounds!"
    (* Else, my_list has at least one element, so divide my_list into a head (i.e. head_of_my_list) and a tail (i.e. rest_of_my_list) *)
    | head_of_my_list::rest_of_my_list ->
      (* If the index == 0 and the list is not empty, then we have reached the element we are trying to access. *)
      if (index = 0)
      (* Since index is now == 0 either by starting off that way or through recursive calls,
              we can return the first element in my_list, as we are now at the element we were searching for. *)
      then head_of_my_list
      (* Else we must keep searching the rest_of_my_list to see if we can find the item at our predefined index.
         Since we're searching one less element of my_list by searching the rest_of_my_list,
              we must also decrement the index that we're searching for by 1. *)
      else get_nth_element rest_of_my_list (index - 1)


(* Input: A general tree of any type (i.e. 'a gt) *)
(* Output: A mirror image of the general tree t *)

(* NOTE: THE FUNCTION "m" IN "q2a" THAT TAKES A BINARY TREE AS AN INPUT IS THE
BINARY TREE VERSION OF MIRROR *)

(* Commands to see if it's working right and their results when using utop:
    utop # mirror t;;
    - : int gt =
        Node (33,
          [Node (77, [Node (103, []); Node (48, []); Node (37, [Node (14, [])])]);
           Node (12, [])])

    utop # mirror t2;;
    - : int gt =
        Node (33,
          [Node (77, [Node (103, []); Node (48, []); Node (37, [])]); Node (12, [])])
    utop # mirror imperfect_rt;;
    - : int gt =
        Node (33,
          [Node (77, [Node (103, []); Node (48, [Node (128, [])]); Node (37, [])]);
           Node (12, [Node (6, [Node (3, [])])])])
    utop # mirror (mk_leaf "testing");;
    - : string gt = Node ("testing", [])
    utop # mirror perfect_t;;
    - : int gt =
        Node (33,
          [Node (77,
            [Node (103, [Node (256, [])]); Node (48, [Node (128, [])]);
             Node (37, [Node (14, [])])]);
           Node (12, [Node (6, [Node (3, [])])])])
  *)

(* let rec mirror (Node(d,ch)) = *)
let rec mirror (t:'a gt) : 'a gt =
  (* Begin matching on the tree t *)
  match t with
  (* For the case where t is a node with the value root_data and children child_node_list (i.e. every case) *)
  | Node(root_data, child_node_list) ->
    (* If the tree itself is a leaf *)
    if (is_leaf t)
    (* Then return t, since every leaf is the reverse of itself because of how leaves are defined.
          It's like if you were to take the reverse of an empty list. The reverse of that list == the same list.
          Because a leaf has no children, and literally an empty list in the place where its children are listed,
              the reverse of a leaf == the leaf. *)
    then t
    (* Else, return a new tree with data = root_data
            that uses the nested helper function defined below to reverse the leaves. *)
    else Node(root_data, (
        (* NOTE that this function must be defined within the mirror function
                    because mirror_child_nodes calls itself and its parent function "mirror" and "mirror" calls mirror_child_nodes
                    and they cannot both be above the other as is required with external helper functions in OCaml *)
        (* Input to this function is the child_node_list,
                  the final list (i.e. mirrored_child_node_list),
                  and a counter variable (i.e. backwards_counter) that will count backwards from the length of the child_node_list.
                  The mirrored_child_node_list will initially be empty
                  and the backwards counter will initially be set to ((list_length child_node_list) - 1),
                                    as the first element of the mirrored_child_node_list is the last element of the child_node_list.
                  backwards_counter will be decremented by 1 until it reaches -1, where the function will stop.
                                  backwards_counter will stop at -1 instead of 0 because
                                              the 0th element is still needed as the last element of the mirrored_child_node_list. *)
        let rec mirror_child_nodes (child_node_list:'a gt list) (mirrored_child_node_list:'a gt list) (backwards_counter:int) : 'a gt list =
          (* If we HAVE NOT finished fully creating the mirrored_child_node_list *)
          if (backwards_counter != -1)
          (* Then call mirror_child_nodes with the following arguments:
                child_node_list
                (mirrored_child_node_list @ [get_nth_element child_node_list backwards_counter])
                (backwards_counter - 1)
            This is @ppending the "backwards_counter"th element of the child_node_list to the end of the mirrored_child_node_list,
                    which will create the reverse of the child_node_list (well, a shallow reverse)
            And decrementing backwards_counter by 1 so we can keep moving down to the base case,
                                    where we've constructed the shallow entire mirrored_child_node_list *)

          then mirror_child_nodes child_node_list
                                  (mirrored_child_node_list @ [get_nth_element child_node_list backwards_counter])
                                  (backwards_counter - 1)
          (* Else (make a function in here to get the mirror of every mirrored_child_node_list's children) *)
          else (

            let rec call_mirror_on_mirrored_lists_elements (mirrored_child_node_list:'a gt list) : 'a gt list =
              (* Begin matching on the list mirrored_child_node_list *)
              match mirrored_child_node_list with
              (* If the list mirrored_child_node_list is empty,
                 then we have called call_mirror_on_mirrored_lists_elements and will return the empty list *)
              | [] -> []
              (* Else, there is still at least one mirrored_child_node left,
                    so divide the list into a head (i.e. first_mirrored_child_node) and a tail (i.e. other_mirrored_child_nodes) *)
              | first_mirrored_child_node::other_mirrored_child_nodes ->
                (* Construct the final list as follows:
                   [mirror first_mirrored_child_node]
                   @ppended with
                   (call_mirror_on_mirrored_lists_elements other_mirrored_child_nodes) *)
                [mirror first_mirrored_child_node] @ (call_mirror_on_mirrored_lists_elements other_mirrored_child_nodes)

          (* Now from call_mirror_on_mirrored_lists_elements's parent function (i.e. mirror_child_nodes)
                  Call mirror_child_nodes's nestedly defined helper function (i.e. call_mirror_on_mirrored_lists_elements)
                  using the argument mirrored_child_node_list, as we created using the mirror_child_nodes function. *)
          in call_mirror_on_mirrored_lists_elements mirrored_child_node_list)

        (* Now from mirror_child_nodes's parent function (i.e. mirror)
                  Call mirror's nestedly defined helper function (i.e. mirror_child_nodes)
                  using the arguments
                        child_node_list
                        []
                        (child_node_list.length) - 1 *)
        in mirror_child_nodes child_node_list [] ((list_length child_node_list) - 1)
      )
    )
  (* failwith "implement" *)

(* Exercise 7 *)

(* Input: a <function f> to be applied to the <general tree t> *)
(* Output: a general tree with the function f applied to the data of every node *)

  (* Commands to see if it's working right and their results when using utop:
      utop # mapt (fun i -> i > 20) t;;
      - : bool gt =
      Node (33,
             [Node (false, []);
              Node (true,
               [Node (true, [Node (false, [])]); Node (true, []); Node (true,
               [])])])

FIX THIS CASE IF TIME PERMITS, REMEMBER THESE ARE MIRRORED CURRENTLY      utop # mirror t2;;
      - : int gt =
          Node (33,
            [Node (77, [Node (103, []); Node (48, []); Node (37, [])]); Node (12, [])])
FIX THIS CASE IF TIME PERMITS, REMEMBER THESE ARE MIRRORED CURRENTLY      utop # mirror imperfect_rt;;
      - : int gt =
          Node (33,
            [Node (77, [Node (103, []); Node (48, [Node (128, [])]); Node (37, [])]);
             Node (12, [Node (6, [Node (3, [])])])])
FIX THIS CASE IF TIME PERMITS, REMEMBER THESE ARE MIRRORED CURRENTLY      utop # mirror (mk_leaf "testing");;
      - : string gt = Node ("testing", [])
FIX THIS CASE IF TIME PERMITS, REMEMBER THESE ARE MIRRORED CURRENTLY      utop # mirror perfect_t;;
      - : int gt =
          Node (33,
            [Node (77,
              [Node (103, [Node (256, [])]); Node (48, [Node (128, [])]);
               Node (37, [Node (14, [])])]);
             Node (12, [Node (6, [Node (3, [])])])])
    *)
(* let rec mapt f (Node(d,ch)) = *)
(* let rec mapt f t = *)
let rec mapt (f:'a -> 'b) (t:'a gt) : 'b gt =
  (* Begin matching on the tree t *)
  match t with
  (* For the case where t is a node with the value root_data and children child_node_list (i.e. every case) *)
  | Node(root_data, child_node_list) ->
    (* t *)
    (* Node((f root_data), child_node_list) *)

    (* NOTE: The following if-then-else statement that is commented out
                                        is not only unnecessary but was causing the function to fail on inputs like "mapt (fun i -> i>20) t",
                                even though a statement like "mapt (fun i -> i + 1) t" would succeed.
                So we restart at the uncommented Node(.....) *)
    (* If the tree t is a leaf *)
    (*if (is_leaf t)
    (* Then return a new node with the root_data replaced with (f root_data) and the same child_node_list, which is empty. *)
    then Node((f root_data), child_node_list)
    (* Else, create a new node with the root_data replaced with (f root_data) and then using the helper function on child_node_list *)
    else *)

    (* Create a new node with the root_data replaced with (f root_data) and have the Node's children be created using the helper function. *)
    Node((f root_data), (
      (* NOTE that this function must be defined within the mapt function
                  because mapt_child_nodes calls itself and its parent function "mapt" and "mapt" calls mapt_child_nodes
                  and they cannot both be above the other as is required with external helper functions in OCaml *)
        let rec mapt_child_nodes (function_to_apply:'a -> 'b) (child_node_list:'a gt list) : 'b gt list =
          (* Begin matching on the list child_node_list *)
          match child_node_list with
          (* If the list child_node_list is empty, then we've called mapt on every child, so return the empty list. *)
          | [] -> []
          (* Else, we still have at least one child to call mapt on
                      so divide the list into a head (i.e. first_child_node) and a tail (i.e. other_child_nodes) *)
          | first_child_node::other_child_nodes ->
            (* Construct the final list as follows:
                Calling mapt using the <function_to_apply> and the <first_child_node>
                @ppended to
                Calling mapt_child_nodes using the <function_to_apply> and the <other_child_nodes> *)
            [mapt function_to_apply first_child_node] @ (mapt_child_nodes function_to_apply other_child_nodes)
        (* Now from mapt_child_nodes's parent function (i.e. mapt)
                  Call mapt's nestedly defined helper function (i.e. mapt_child_nodes)
                  using the arguments
                          f
                          child_node_list *)
        in mapt_child_nodes f child_node_list
      )
    )
  (* failwith "implement" *)

(* Exercise 8 *)

(* The acquire_root_data function creates a list of the root_data of each child_node in the child_node_list and returns that list *)
let rec acquire_root_data (child_node_list:'a gt list) : 'b list =
  (* Begin matching on the child_node_list *)
  match child_node_list with
  (* If the list is empty, i.e. we've iterated over the entire list or it was empty to begin with *)
  | [] -> []
  (* Else the list isn't empty, so divide the list into a head (i.e. first_child_node) and a tail (i.e. other_child_nodes) *)
  | first_child_node::other_child_nodes ->
    (* Begin matching on the first_child_node *)
    match first_child_node with
    (* For the case where the first_child_node is a node with the value root_data and children first_childs_children (i.e. every case) *)
    | Node(first_child_data, first_childs_children) ->
      (* Construct a list as follows:
          Create a list containing the first_child_data (i.e. the root_data for the first_child_node)
          @ppended to
          acquire_root_data other_child_nodes         <----- i.e. a recursive call over the other_child_nodes
          @ppended to
          acquire_root_data first_childs_children     <----- i.e. a recursive call over the first_childs_children *)
      [first_child_data]
      @
      acquire_root_data other_child_nodes
      @
      acquire_root_data first_childs_children

(* Input: a <function f> to be applied to the elements of the <general tree t> *)
(* Output: the result of applying the function to the tree t *)
let rec foldt (f:'a -> 'b list -> 'b) (t:'a gt) : 'b =
(* let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b = *)
  (* fun f (Node(d,ch)) -> *)
  (* fun f (t:'a gt) -> *)
  (* failwith "implement" *)
  (* Begin matching on the tree t *)
  match t with
  (* For the case where t is a node with the value root_data and children child_node_list (i.e. every case) *)
  | Node(root_data, child_node_list) ->
    (* f root_data child_node_list *)
    (* If t is a leaf *)
    (* if (is_leaf t)
    (* Then apply the function f to root_data and the empty child_node_list and do not continue *)
    then f root_data child_node_list
    (* Else, apply the function f to the root_data
              and, using the helper function, to the root_data of all of the nodes in the child_node_list *)
    else  *)

    (* Apply the function f over the root's root_data and over the root_data of the rest of the tree. *)
    f root_data (acquire_root_data child_node_list)
    (* NOTE: I redesigned the helper function so it no longer has to be caged inside the foldt function,
        so everything below here is no longer necessary *)
    (* f root_data (
    (* The acquire_root_data function creates a list of the root_data of each child_node in the child_node_list and returns that list *)
    let rec acquire_root_data (child_node_list:'a gt list) : 'b list =
      (* Begin matching on the child_node_list *)
      match child_node_list with
      (* If the list is empty, i.e. we've iterated over the entire list or it was empty to begin with *)
      | [] -> []
      (* Else the list isn't empty, so divide the list into a head (i.e. first_child_node) and a tail (i.e. other_child_nodes) *)
      | first_child_node::other_child_nodes ->
        (* Begin matching on the first_child_node *)
        match first_child_node with
        (* For the case where the first_child_node is a node with the value root_data and children first_childs_children (i.e. every case) *)
        | Node(first_child_data, first_childs_children) ->
          (* Construct a list as follows:
              Create a list containing the first_child_data (i.e. the root_data for the first_child_node)
              @ppended to
              acquire_root_data other_child_nodes         <----- i.e. a recursive call over the other_child_nodes
              @ppended to
              acquire_root_data first_childs_children     <----- i.e. a recursive call over the first_childs_children *)
          [first_child_data]
          @
          acquire_root_data other_child_nodes
          @
          acquire_root_data first_childs_children
      in acquire_root_data child_node_list) *)
      (* NOTE: I redesigned the helper function so it no longer has to be caged inside the foldt function,
          so everything below here is no longer necessary and wasn't fully converted anyways.
      (* Note that this function must be defined within the foldt function
                  because foldt_child_nodes calls itself and its parent function "foldt" and "foldt" calls foldt_child_nodes
                  and they cannot both be above the other as is required with external helper functions in OCaml *)
      (* foldt_child_nodes creates a list of the root_data of each child_node in the child_node_list and returns that list *)
      let rec foldt_child_nodes (function_to_apply:'a -> 'b list -> 'b) (child_node_list:'a gt list) : 'b list =
        (* Begin matching on the list child_node_list *)
        match child_node_list with
          (* If the list is empty,
              i.e. we have either recursively called foldt_child_nodes to when we reach an empty list or the list was empty to begin with,
                  then return the empty list. *)
          | [] -> []
        (* By getting to this point we have at least one child, since t is not a leaf,
              so divide the list into a head (i.e. first_child_node) and a tail (i.e. other_child_nodes) *)
          | first_child_node::other_child_nodes ->
            (* Now begin matching on the  *)
            (* If other_child_nodes is empty *)
            if (other_child_nodes = [])
            (* Then call foldt f first_child_node, but don't recursively call anything *)
            then foldt function_to_apply first_child_node
            (* Else, there are more child_nodes to check, so recursively call foldt_child_nodes function_to_apply other_child_nodes *)
            else foldt_child_nodes function_to_apply other_child_nodes
      (* Now from foldt_child_nodes's parent function (i.e. foldt)
        Call foldt's nestedly defined helper function (i.e. foldt_child_nodes)
        using the arguments
                f
                child_node_list *)
      in foldt_child_nodes f child_node_list
      ) *)

let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

(* memt IS NOT FULLY FUNCTIONAL, WE'RE ONLY CONTINUING BECAUSE OF TESTING *)
let memt t e =
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

(* Exercise 9 *)
let mirror' (t:'a gt) : 'a gt  =
  (* foldt (fun root_data child_node_data_list -> Node(root, [List.fold_right (fun )])) *)
  failwith "implement"
