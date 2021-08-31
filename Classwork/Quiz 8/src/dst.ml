open ReM

type tenv =
  | EmptyTEnv
  | ExtendTEnv of string*Ast.texpr*tenv


type 'a tea_result = ('a,tenv) a_result



let runt (c:'a tea_result) : 'a result =
  c EmptyTEnv

let rec apply_tenv : string -> Ast.texpr tea_result =
  fun id env ->
  match env with
  | EmptyTEnv -> Error (id^" not found!")
  | ExtendTEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_tenv id tail

let extend_tenv : string -> Ast.texpr -> tenv tea_result =
  fun id v ->
  fun env ->
  Ok (ExtendTEnv(id,v,env))

let extend_list_tenv : string list -> Ast.texpr list -> tenv tea_result =
  fun ids tys ->
  fun env ->
  Ok (List.fold_left2 (fun env id v -> ExtendTEnv(id,v,env)) env ids tys)


  let list_of_tupleType : Ast.texpr -> (Ast.texpr list) tea_result =  function
    |  TupleType(ts) -> return ts
    | _ -> error "Expected a tuple type!"

  let list_of_recordType : Ast.texpr -> ((string*Ast.texpr) list) tea_result =  function
    |  RecordType(ts) -> return ts
    | _ -> error "Expected a record type!"

let rec string_of_tenv'  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,tid,env) -> "("^id^","^Ast.string_of_texpr tid^")\n"^string_of_tenv' env

let string_of_tenv : string tea_result =
  fun env ->
  Ok ("Type Environment:\n"^ string_of_tenv' env)
