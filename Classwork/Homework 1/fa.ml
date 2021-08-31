(* Ben Rose *)
(* CS 496 Homework 1 *)
(* Finite Automaton Implementation *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)
(* February 18, 2021 *)

(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

(* Form of each automata's structure: *)

(* {Set of states}, where each state is a string *)
(* {Start state}, where, as above, the start state is a string *)
(* {Transition function}, i.e. Moving from [starting state for this transition]
                                  when encountering the [input symbol for the transition]
                                                      to the [ending state for this transition] *)
(* {Final State, i.e. the Accept State}, where, as above, the accept state is a string *)

(* Example automatons *)
let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }

let a3 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q3";"q2";"q4"]
         }

let accept_only_empty_string = {states = ["q0";"q1";"q2";"q3";"q4"];
         start = "q0";
         tf = [];
         final= ["q0"]
        }

let non_deterministic_1 = {states = ["q0";"q1";"q2";"q3"];
                           start = "q0";
                           tf = [("q0",'a',"q1"); ("q0",'a',"q3"); ("q1",'b',"q1"); ("q1",'c',"q2")];
                           final = ["q2";"q3"]
                          }

let non_deterministic_2 = {states = ["q0";"q1";"q2";"q3";"q4"];
        start = "q0";
        tf = [("q0",'a',"q1"); ("q1",'b',"q1");
              ("q1",'c',"q2"); ("q1",'b',"q3");
              ("q3",'a',"q2"); ("q4",'c',"q4")];
        final= ["q2";"q4"]
       }

let non_deterministic_2 = {states = ["q0";"q1";"q2";"q3";"q4"];
       start = "q0";
       tf = [("q0",'a',"q1"); ("q1",'b',"q1");
             ("q1",'c',"q2"); ("q2",'b',"q3");
             ("q2",'b',"q4"); ("q4",'c',"q4")];
       final= ["q2";"q4"]
      }

let invalid_start_state = {states = ["q0";"q1";"q2"];
                           start = "q3"; (* automata "a", except the start state is changed to q3 from q0,
                                                                    which is a problem because q3 is not in the set of states *)
                           tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
                           final = ["q2"]}

let invalid_end_state = {states = ["q0";"q1";"q2"];
                         start = "q0";
                         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
                         final = ["q9"]} (* automata "a", except the end state is changed to q9 from q2,
                                                                    which is a problem because q9 is not in the set of states *)

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0
    then l
    else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)


(* Define State Options --- didn't work*)
(* type state option = None | Some of state *)

(* Exercise 1: apply_transition_function *)
(* Input: The [transition function to be applied], the [current state], and the [symbol for the transition] *)

(* Output: A {'state' "option"}, which is an "option", which can either be the value "None",
                                                        meaning that what you're searching for wasn't found,
                                                           or "Some 'x'",
                                                              where x is of the type listed before the "option" term, in this case 'state',
                                                              which will be something like "q0", "q1", etc., since those are states in the
                                                              finite automaton.             /\    /\
                                                           So, in the two 'x' values up here^^    ^^,
                                                                        the first printed return value when using utop would be:
                                                                        state option = Some "q0"

                                                                        and the second printed return value when using utop would be:
                                                                        state option = Some "q1" *)
(* match state st to the first value of the transition tuples.
                Make a recursive search for going over the transition tuples, i.e. the transition function "f" in the "let" statement.
                      If you find a transition tuple that matches in the head of the list,
                              check if the symbol sym is a valid next symbol.
                                  If the symbol sym is valid to transition on in this transition tuple,
                                        then return the state that has been transitioned to (i.e. the third element of the transition tuple)
                                  Else, go to the below else statement
                      Else, ditch this transition tuple and search the rest of the transition tuples
                      for a valid state/transition tuple match using a recursive call over the rest of the transition function*)
      (* Remember we are under the assumption that the Finite Automaton is a Deterministic Finite Automaton *)

(* Commands to see if it's working right and their results:
        utop # apply_transition_function a.tf "q0" 'a';;
        - : state option = Some "q1"
        utop # apply_transition_function a.tf "q0" 'b';;
        - : state option = None
        utop # apply_transition_function a.tf "q0" 'c';;
        - : state option = None
 *)
let rec apply_transition_function (f:tf) (st:state) (sym:symbol)(* : tf -> state -> symbol -> state option *) =
  match f with
  | [] -> None (* No valid transitions in the transition function f,
                i.e. the starting state st does not contain any transitions with the symbol sym *)
  (* A thought was to do head::tail for the head and tail of the list, didn't get to test it. Do Nested If and Nested function
                                                                                  calls and stuff tomorrow. And helper functions and stuff

   head::tail *)

   (* Now, the list is not empty, so grab the head of the list, which will be the form of (origin_state, transition_symbol, new_state)
                                                                  and the tail of the list which will be other transition tuples like
                                                                  the head, i.e. the rest of the list i.e. the tail of the list*)
  | (origin_state, transition_symbol, new_state)::other_transition_tuples ->
    (* If the origin_state == st, i.e. the origin state we want to transition from exists and is in the transition tuple at the head of the list
        and the transition symbol in the transition tuple at the head of the list == sym, i.e. our specified symbol input*)
    if (origin_state = st && transition_symbol = sym)
    (* Then return the state that you arrive at when you transition to the new_state from the origin_state st using the transition_symbol sym.*)
    then Some new_state

    (* Else, recurively call apply_transition_function over the rest of the list *)
    else apply_transition_function other_transition_tuples st sym

(* Exercise 2: accept *)

(* Checks if the current_state is in the list of possible accept states *)
let rec in_accept_state (current_state:state) (list_of_accept_states:state list) =
  (* Begin separating possible scenarios for the list_of_accept_states *)
  match list_of_accept_states with
  (* If we've recursively examined the list_of_accept_states and there were no accept states that matched current_state *)
  | [] ->
    (* Return false, as the current_state is not an accept state. *)
    false
  (* The list is not empty, so evaluate the head of the list (i.e. accept_state) and have the tail be other_accept_states *)
  | accept_state::other_accept_states ->
    (* If the current_state is equal to the accept_state we're evaluating
                                            (i.e. if the current_state is equal to the head of the list_of_accept_states) *)
    if current_state = accept_state
    (* Then return true, as the current_state is an acccept state. *)
    then true
    (* Else, recursively call in_accept_state over the other_accept_states, attempting to match one accept state with current_state. *)
    (* For example, the below line with the "@@" symbols is the same as:
                  else in_accept_state current_state other_accept_states
            since that's the same thing with no "@@" symbols. *)
    else in_accept_state current_state other_accept_states

    (* The following recursive function will check to see if the list of input characters is empty AND the current_state == an accept state,
                                                        which would mean that we're in the accept state and have no characters
                                                        that would potentially move us out of the accept state. *)

    (* If the list of input characters is empty AND the current_state == an accept state,
                return true, since the automaton accepts the string.
    Else if the list of input characters is empty and we are NOT in an accept state,
                return false, since the automaton rejects the input string.
    Else, the list of input characters is not empty.
          If calling "apply_transition_function transition_function current_state head_of_input_string_char_list"
              yields the state option None, return false, since the automaton has encountered a transition_symbol
              that is an invalid transition from the current_state, thus leading us to the kill state, i.e. the
              "input string is not accepted" state.
          Else, calling "apply_transition_function transition_function current_state head_of_input_string_char_list"
              yields another state. Thus, using the new_state from our call of "apply_transition_function",
              we must call "accept_reject_or_keep_checking new_state transition_function rest_of_input_string_char_list list_of_accept_states"
              recursively to check the rest of the input string and transitions. *)
let rec accept_reject_or_keep_checking (current_state:state) (transition_function:tf) (list_of_accept_states:state list) (input_string_as_list_of_chars:input) (*: bool*) =
  (* Begin dividing the possible values for the input_string_as_list_of_chars list*)
  match input_string_as_list_of_chars with
  (* If the list of input characters is empty *)
  | [] ->
    (* If the current_state == some accept state in the list_of_accept_states,
        Note that this if statement is valid because the function in_accept_state evaluates to a boolean *)
    if (in_accept_state current_state list_of_accept_states)

    (* Then return true, as transitioning with the input string has led us to the accept state *)
    then true
    (* Otherwise, the input string has not led us to an accept state, so we return false. *)
    else false
  (* If the list of input characters is not empty and has a head (i.e. current_input_symbol) and a tail (i.e. other_input_symbols) *)
  | current_input_symbol::other_input_symbols ->
    (* Check if the return value of applying the transition function
                  using transition_function, current_state, and current_input_symbol results in None or a new state. *)
    match apply_transition_function transition_function current_state current_input_symbol with

    (* If there is no next state that the current_input_symbol can take us to from the state current_state *)
    | None ->
    (* Return false, as the input string is not accepted by this automaton *)
      false
    (* Else, we have a new_state that we get by transitioning from the current_state with the symbol current_input_symbol *)
    | Some new_state ->
      (* Recursively call accept_reject_or_keep_checking using the new_state, the transition_function, the other_input_symbols (i.e. the tail),
                                                                                    and the list of possible accept states. *)
      accept_reject_or_keep_checking new_state transition_function list_of_accept_states other_input_symbols

(* Input: The [finite automata] that either accepts or rejects the [string that has been transformed into a list of chars by input_of_string] *)
(* Output: Whether or not (i.e. true or false) the provided string is accepted by the finite automaton *)

(* Commands to see if it's working right and their results:
      utop # accept a @@ input_of_string "abbc";;
      - : bool = true
      utop # accept a @@ input_of_string "ac";;
      - : bool = true
      utop # accept a @@ input_of_string "a";;
      - : bool = false
      utop # accept a @@ input_of_string "bb";;
      - : bool = false
 *)
let accept (my_automaton:fa) (input_string_as_list_of_chars:input) : bool =
  (* If the input string is empty and the automaton is in an accept state, then the string is accepted by the automaton.
        Else the input string is not accepted by the automaton.*)

  accept_reject_or_keep_checking my_automaton.start my_automaton.tf my_automaton.final input_string_as_list_of_chars


(* Exercise 3: deterministic *)

(* Helper function to determine if the first two elements of each transition_tuple are unique,
                                          i.e. current_state and transition_symbol respectively.
  This is achieved by removing the head of the remaining_transition_function_for_this_search list on the recursive call
  and when the function is called initially, storing the current_state and transition_symbol for the head of the list in
  the variables "current_state" and "transition_symbol" respectively.
  Note that this function is not called if there are no transitions out of the start state, which would mean that
  no strings are accepted except possibly the empty string.
      We match remaining_transition_function with
          If the remaining_transition_function == [], i.e. the rest of the
              remaining "current_state"s and "transition_symbol"s
              is empty, then we have checked every element in the list of transition_tuples for duplicates and no duplicates exist,
              and the value "true" should be returned, since the finite automaton we're looking at is indeed deterministic.

          Else, we still have more possible transition_tuples to check for duplicates (i.e. remaining_transition_function is not empty),
          so we split remaining_transition_function into (new_current_state, new_transition_symbol, new_arrival_state)::other_transition_tuples.
                If running "apply_transition_function remaining_transition_function current_state transition_symbol" returns "None",
                    then there are no duplicates of current_state and transition_symbol, so we run
                    "unique_starting_state_and_transition_symbol new_current_state new_transition_symbol other_transition_tuples"
                Else, running "apply_transition_function remaining_transition_function current_state transition_symbol" has found a duplicate,
                    meaning that the automaton is non-deterministic. So we return the value "false".
 *)
let rec unique_starting_state_and_transition_symbol (current_state:state) (transition_symbol:symbol) (remaining_transition_function:tf) =
                                            (* (remaining_transition_function_for_this_search) (remaining_transition_function_in_general:tf) = *)
    (* Begin splitting remaining_transition_function into cases *)
    match remaining_transition_function with
    (* If remaining_transition_function == [] *)
    | [] ->
    (* Return true, since the automaton is determinsitic *)
      true
    (* Else, remaining_transition_function has a head and tail of the list, which is the first transition_tuple in the list
                                                                                        and the rest of the transition_tuples respectively *)
    | (new_current_state, new_transition_symbol, new_arrival_state)::other_transition_tuples ->
      (* Check if the current_state and transition_symbol have any duplicates in remaining_transition_function,
            i.e. if (apply_transition_function remaining_transition_function current_state transition_symbol) == None *)
      if ((apply_transition_function remaining_transition_function current_state transition_symbol) = None)
      (* Then recursively call unique_starting_state_and_transition_symbol over
                                    the new_current_state, new_transition_symbol, and other_transition_tuples *)
      then unique_starting_state_and_transition_symbol new_current_state new_transition_symbol other_transition_tuples
      (* Else, a duplicate of current_state and transition_symbol has been found among the remaining_transition_function,
                which means that the automaton is non-deterministic and we should return false. *)
      else false

(* Checks if a given automaton is deterministic or non-deterministic.
    The parameters for an automaton being non-deterministic is that two transitions using the same state and input symbol exist.
                                    So, if the first two elements of each transition_tuple are unique, then the automaton is deterministic.
                                        Else, the automaton is non-deterministic. *)

(* Input: a [finite automaton] that is either deterministic or non-deterministic *)
(* Output: whether the automaton is deterministic (i.e. true) or non-deterministic (i.e. false) *)

(* Note: the (my_automaton:fa) syntax means that you're [defining the variable my_automaton] [of type] [fa, i.e. finite automaton],
                                                      where the three things in []s are "my_automaton", ":", and "fa" respectively.
        This same principle holds true for whatever "variable_name:variable_type" you use. *)

(* Commands to see if it's working right and their results:
      utop # deterministic a;;
      - : bool = true
      utop # deterministic a2;;
      - : bool = true
      utop # deterministic non_deterministic_1;;
      - : bool = false
      utop # deterministic non_deterministic_2;;
      - : bool = false
 *)

let deterministic (my_automaton:fa) =
  (* Begin separating possible scenarios for what the transition function (i.e. the field "tf") of my_automaton can be *)
  match my_automaton.tf with
  (* If the transition_function is initially empty,
  i.e. there are no possible transitions, i.e. no strings are accepted except maybe the empty string *)
  | [] ->
  (* Then return true, as there are no duplicate elements in an empty list *)
    true
  (* Else, the list has a head and a tail, and the head is of the form (origin_state, transition_symbol, new_state)
                                            and the tail (i.e. other_transition_tuples) is called other_transition_tuples *)
  | (origin_state, transition_symbol, new_state)::other_transition_tuples ->
    (* Then check if the origin_state and transition_symbol appear more than the initial time that they appear. *)
    unique_starting_state_and_transition_symbol origin_state transition_symbol other_transition_tuples


(* Exercise 4: valid *)

(* This following function checks if the list my_list contains the element possible_element_to_check *)

let rec list_contains (my_list:'a list) (possible_element_to_check:'a) =
  (* Divide possible values for my_list into cases *)
  match my_list with
  (* If the list is empty *)
  | [] ->
    (* Then the value we were searching for was not found in the list and we should return false. *)
    false
  (* Else, the list contains a first element and the rest of the list (i.e. the head of the list and tail of the list) *)
  | first_element::rest_of_my_list ->
    (* If the first element in the list == the element we're searching for *)
    if (first_element = possible_element_to_check)
    (* Return true, as my_list contains the element possible_element_to_check *)
    then true
    (* Else, recursively call list_contains over the rest of my list. *)
    else list_contains rest_of_my_list possible_element_to_check

(* This following function checks if the list my_list contains every element that is within the list list_of_elements_to_check *)
let rec list_contains_elements_of_second_list (my_list:'a list) (list_of_elements_to_check:'a list) =
  (* Divide possible values for list_of_elements_to_check into cases *)
  match list_of_elements_to_check with
  (* If the list_of_elements_to_check is empty *)
  | [] ->
    (* Then return true. This is not because we necessarily found each item in the list,
                                  but true && false (for example) will return false, so
                                  if finding any element of the list_of_elements_to_check fails,
                                  then the function itself will return false.
                                  Otherwise, the result will be true && true && ... && true, which returns true. *)
    true
  | first_element_to_check::other_elements_to_check ->
    (list_contains my_list first_element_to_check) && (list_contains_elements_of_second_list my_list other_elements_to_check)


(* Input: a [finite automaton], which is either valid or invalid *)
(* Output: whether the automaton is valid (i.e. true) or invalid (i.e. false) *)

(* Note: an automaton is valid if:
          1. The automaton's start state belongs to the set of states,
          2. The automaton's end state belongs to the set of states,
          3. The automaton is deterministic *)

(* Commands to see if it's working right and their results:
      utop # valid a;;
      - : bool = true
      utop # valid a2;;
      - : bool = true
      utop # valid invalid_start_state;;
      - : bool = false
      utop # valid invalid_end_state;;
      - : bool = false
      utop # valid non_deterministic_1;;
      - : bool = false
      utop # valid non_deterministic_2;;
      - : bool = false
 *)

let valid (my_automaton:fa) =
  (* Return the logical "and" of the following three conditions:
        the automaton is deterministic,
        the states in my_automaton contain the start state,
        and the states in my_automaton contain the end state(s)
     If each of those are true, then the logical "and" will return true.
     But if even one of those is false, then the logical "and" will return false.
     So only this line is needed to determine if my_automaton is valid. *)
  ((deterministic my_automaton) && (list_contains my_automaton.states my_automaton.start)
              && (list_contains_elements_of_second_list my_automaton.states my_automaton.final))


(* Exercise 5: reachable *)

(* Returns a list of states without duplicates, with any potential duplicates removed *)
let rec uniquify (my_list:state list) =
  (* Divide possible values for my_list into cases *)
  match my_list with
  (* If there are no more elements in my_list, concatenate the empty list to the result *)
  | [] -> []
  (* For the first element of the list (i.e. the head) and the rest of the list (i.e. the tail of the list) *)
  | head_of_my_list::rest_of_my_list ->
  (* If the first element of the list (i.e. the head of the list) appears anywhere in the rest of the list *)
    if (list_contains rest_of_my_list head_of_my_list)
    (* Then don't concatenate the first element of the list,
              just recursively call uniquify over the rest of the list, as the first element will appear again and be added once it is unique *)
    then uniquify rest_of_my_list
    (* Else, the first element of the list is unique, so concatenate the head of the list to the final list
                                                                          and recursively call uniquify over the rest of the list *)
    else head_of_my_list::(uniquify rest_of_my_list)

(* Like apply_transition_function, but instead of requiring a symbol to transition, this checks if a transition is possible at all.
                                        If a transition is possible, return Some new_state, where the new_state is the state that applying
                                            the transition arrives at.
                                        Else, return None
                  Note that this will only return the first located transition, so it must be run multiple times
                                                      to detect other transitions, if they exist. *)
let rec apply_possible_transition (transition_tuples:tf) (current_state:state) =
  (* Begin dividing the possible values for the transition_tuples *)
  match transition_tuples with
  (* If there are no more possible transition_tuples *)
  | [] -> None
  (* Else, there is at least one transition tuple, i.e. a head and a tail *)
  | (origin_state, transition_symbol, new_state)::other_transition_tuples ->
    (* If the origin_state == current_state, i.e. we've found what we're looking for *)
    if (origin_state = current_state)
    (* Then return the Some new_state *)
    then Some new_state
    (* Else keep looking through the rest of the transition_tuples *)
    else apply_possible_transition other_transition_tuples current_state

(* Function to find all of the possible states that can be reached from a single start state *)
let rec all_possible_states_from_current_state (transition_tuples:tf) (current_state:state) =
  (* Begin dividing the possible values for the transition_tuples *)
  match transition_tuples with
  (* If there are no more possible transition_tuples to check, then concatenate the empty list to the result *)
  | [] -> []
  (* Else, there is at least one transition tuple to check, i.e. a head and a tail *)
  | (origin_state, transition_symbol, new_state)::other_transition_tuples ->

    match apply_possible_transition transition_tuples current_state with
    (* If applying the possible transition function using the current state returns None,
        then it is not possible to transition to a new state from current_state, so just return the empty list*)
    | None -> []
    (* Else, a state, deNoted reached_state, is reachable in the transition tuples. *)
    | Some reached_state ->
      (* So concatenate that new state with any other possible transitions that you can reach using the other_transition_tuples. *)
      reached_state :: all_possible_states_from_current_state other_transition_tuples current_state

(* Removes any looping transitions, where one state would transition to itself and cause future functions which recursively
                                    search a list of states to loop forever. *)
let rec transition_tuples_without_looping_transitions (transition_tuples:tf) =
  (* Begin by dividing the possible values for the transition_tuples *)
  match transition_tuples with
  (* If we've gone through and recursively concatenated all of the non-looping transition_tuples together, concatenate the empty list *)
  | [] -> []
  (* Else, we still have at least one transition_tuple left in the list of transition_tuples, i.e. we have a head and tail *)
  | (origin_state, transition_symbol, new_state)::other_transition_tuples ->
    (* If the origin_state == new_state, then the transition_tuple loops on itself and should not be included in the final list *)
    if origin_state = new_state
    (* Then just call transition_tuples_without_looping_transitions over the rest of the transition_tuples *)
    then transition_tuples_without_looping_transitions other_transition_tuples
    (* Else the transition_tuple we're evaluating does not loop and should be added to the list of non-looping transition_tuples *)
    else (origin_state, transition_symbol, new_state)::transition_tuples_without_looping_transitions other_transition_tuples

(* Function to get all reachable states, where duplicates may exist *)
let rec all_reachable_states (transition_tuples:tf) (current_states:state list) =
  (* Begin dividing the possible values for current_states *)
  match current_states with
  (* If there are no more states to look at, i.e. we've finished compiling our list recursively, then return the empty list *)
  | [] -> []
  (* Else, there is at least one state to check reachable states from *)
  | first_state::rest_of_states ->
    (* Begin dividing the possible values for transition_tuples *)
      all_possible_states_from_current_state transition_tuples first_state
      @ all_reachable_states (transition_tuples_without_looping_transitions transition_tuples) (all_possible_states_from_current_state transition_tuples first_state)
      @ all_reachable_states (transition_tuples_without_looping_transitions transition_tuples) rest_of_states

(* Returns a list of reachable states from the start state *)

(* Input: a [finite automaton] *)
(* Output: a [state list], which contains every state that can be reached through transitions starting at the start state,
                                        and all non-reachable state will be left out.
          Note: The start state, since it is inherently reachable by being the start state,
                  will be included in the set of states IF it is a valid state *)

(* Commands to see if it's working right and their results:
      utop # reachable a;;
      - : state list = ["q0"; "q1"; "q2"]
      utop # reachable a2;;
      - : state list = ["q0"; "q1"; "q2"]
      utop # reachable non_deterministic_1;;
      - : state list = ["q0"; "q1"; "q3"; "q2"]
      utop # reachable non_deterministic_2;;
      - : state list = ["q0"; "q1"; "q3"; "q2"]
      utop # reachable invalid_start_state;;
      - : state list = []
      utop # reachable invalid_end_state;;
      - : state list = ["q0"; "q1"; "q2"]
 *)

let reachable (my_automaton:fa) =
  (* Returns the uniquified list of reachable states using the [transition_function of my_automaton] and the [start state of my_automaton]
      This list is uniquified because you might be able to reach identical states through different paths. *)
  (* If the start state of my_automaton is a valid start state (i.e. exists in the set of states) *)
  if list_contains my_automaton.states my_automaton.start
  (* Then concatenate the start state of the automaton with the unique list of all other reachable states *)
  then my_automaton.start::uniquify (all_reachable_states my_automaton.tf [my_automaton.start])
  (* Else, just uniquify all_reachable_states of the automaton *)
  else uniquify (all_reachable_states my_automaton.tf [my_automaton.start])


(* Exerise 6: remove_dead_states *)

(* Returns the list of elements that are in the first list but not the second list *)
let rec get_elements_not_found_in_second_list (first_list:'a list) (second_list:'a list) =
  (* Divide possible values for first_list into cases *)
  match first_list with
  (* If the list is empty, we've recursively looked at the whole list, so concatenate the empty list with the rest of the list *)
  | [] -> []
  (* Else, there are still elements in the first list, so for the first element of the first list *)
  | first_element::rest_of_first_list ->
    (* If the first_element is also an element of the second_list *)
    if (list_contains second_list first_element)
    (* Then don't include the first_element in the final list,
                                i.e. just call "get_elements_not_found_in_second_list rest_of_first_list second_list" *)
    then get_elements_not_found_in_second_list rest_of_first_list second_list
    (* Else, we've found a unique element and should concatenate it to the rest of the unique elements,
                              i.e. concatenate first_element to "get_elements_not_found_in_second_list rest_of_first_list second_list" *)
    else first_element::(get_elements_not_found_in_second_list rest_of_first_list second_list)

(* Returns a list of elements that both lists do not have in common, where elements that the two lists both have are not included *)
let get_not_common_elements (first_list:'a list) (second_list:'a list) =
  (* Return the concatenation of "get_elements_not_found_in_second_list first_list second_list"
                                  and "get_elements_not_found_in_second_list second_list first_list" *)
  (get_elements_not_found_in_second_list first_list second_list) @ (get_elements_not_found_in_second_list second_list first_list)

(* Returns whether or not (i.e. true or false) the origin_state or new_state is a dead state,
                                              i.e. if the transition being fed is a dead transition *)
let rec is_dead_transition (dead_states:state list) (origin_state:state) (new_state:state) =
  (* Divide possible values for dead_states into cases *)
  match dead_states with
  (* If we've recursively reviewed all dead states with no detections, then return false, since this is not a dead transition *)
  | [] -> false
  (* Else, there is still at least one dead state left in the list of dead_states, i.e. a head and a tail *)
  | dead_state::other_dead_states ->
    (* Check if either the origin_state == dead_state or new_state == dead_state *)
    if (origin_state = dead_state || new_state = dead_state)
    (* Then return true, as either the origin state or new state is a dead_state, making the transition a dead transition *)
    then true
    (* Else we have to recursively search the other_dead_states for potentially mapping a dead state with the origin_state or new_state *)
    else is_dead_transition other_dead_states origin_state new_state

(* Returns transition_tuples without any transitions containing the dead_states, where the dead_states are in the list dead_states *)
let rec remove_dead_transitions (dead_states:state list) (transition_tuples:tf) =
  (* Divide possible values for transition_tuples into cases *)
  match transition_tuples with
  (* If we've recursively reviewed all of the transition_tuples and the list is empty, concatenate an empty list to the final list *)
  | [] -> []
  (* Else, there is at least one transition tuple, i.e. a head and a tail *)
  | (origin_state, transition_symbol, new_state)::other_transition_tuples ->
    match transition_tuples with
    | [] -> []
    | transition_tuple::rest_of_transition_tuples ->
      (* If the current transition_tuple is a dead_transition *)
      if (is_dead_transition dead_states origin_state new_state)
      (* Then just call "remove_dead_transitions dead_states other_transition_tuples" and don't concatenate anything new *)
      then remove_dead_transitions dead_states other_transition_tuples
      (* Else, the transition is not dead, so concatenate it to the list of alive transitions,
                                    i.e. the final list, and recursively call remove_dead_transitions over the other_transition_tuples *)
      (* else [(origin_state, transition_symbol, new_state)] @ remove_dead_transitions dead_states other_transition_tuples *)
      else transition_tuple::remove_dead_transitions dead_states rest_of_transition_tuples

(* Input: [finite automaton] *)
(* Output: the same [finite automaton], except without any dead states or state transitions *)
(* Note: Only valid finite automatons will be fed into this function. *)

(* Commands to see if it's working right and their results:
      utop # remove_dead_states a;;
      - : fa = {states = ["q0";"q1";"q2"];
                 start = "q0";
                 tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
                 final = ["q2"]}
      utop # remove_dead_states a2;;
      - : fa = {states = ["q0";"q1";"q2"];
                start = "q0";
                tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
                final = ["q2"]}
      utop # remove_dead_states a3;;
      - : fa = {states = ["q0";"q1";"q2"];
                 start = "q0";
                 tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
                 final = ["q2"]}
*)

let remove_dead_states (my_automaton:fa) =
  (* Return the finite automaton
          [all reachable states in my_automaton]
          [start state of my_automaton]
          [transition_function without any dead transitions]
          and [final states, excluding dead states]*)
  {states = (reachable my_automaton); start = my_automaton.start;
    tf = remove_dead_transitions (get_not_common_elements my_automaton.states (reachable my_automaton)) my_automaton.tf;
    final = get_elements_not_found_in_second_list my_automaton.final (get_not_common_elements my_automaton.states (reachable my_automaton))}
