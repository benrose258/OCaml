



type 'a result = Ok of 'a | Error of string


type ('a,'e) a_result = 'e -> 'a result


let return (v:'a) : ('a,'e) a_result =
  fun _env ->
  Ok v

let error (s:string) : ('a,'e) a_result =
  fun _env ->
  Error s

let (>>=) (c:('a,'e) a_result) (f: 'a -> ('b,'e) a_result) : ('b,'e) a_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:('e,'e) a_result) (d:('a,'e) a_result): ('a,'e) a_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv



let sequence (cs: (('a,'e) a_result) list) : ('a list,'e) a_result  =
  let mcons p q = p >>= fun x -> q >>= fun y -> return (x::y)
  in List.fold_right mcons cs (return []) 

let mapM (f:'a -> ('b,'e) a_result) (vs:'a list) : ('b list,'e) a_result =
   sequence (List.map f vs)
