open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.iter (fun (var, value) ->
      let vs = match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b in
      Printf.printf "- %s => %s\n" var vs) (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      let vs = match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b in
      acc ^ (Printf.sprintf "- %s => %s\n" var vs)) "" (prune_env env)



(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
    match e with
    |Number x -> Int_Val x
    |True -> Bool_Val true
    |False -> Bool_Val false
 |Var x -> if (List.mem_assoc x env) then (List.assoc x env) else raise UndefinedVar
    |Plus (e1,e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Int_Val n1, Int_Val n2 -> Int_Val (n1+n2)
        |_ -> raise TypeError)
    |Minus (e1, e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Int_Val n1, Int_Val n2 -> Int_Val (n1-n2)
        |_ -> raise TypeError)
    |Times (e1, e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Int_Val n1, Int_Val n2 -> Int_Val (n1*n2)
        |_ -> raise TypeError)
    |Div (e1, e2) ->
 let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Int_Val n1, Int_Val n2 -> if n2=0 then raise DivByZeroError
                                   else Int_Val (n1/n2)
        |_ -> raise TypeError)
    |Mod (e1, e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Int_Val n1, Int_Val n2 -> if n2=0 then raise DivByZeroError
                                   else Int_Val (n1 mod n2)
        |_ -> raise TypeError)
    |And (e1, e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Bool_Val n1, Bool_Val n2 -> Bool_Val (n1 && n2)
        |_ -> raise TypeError)
    |Or (e1, e2) ->
  let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Bool_Val n1, Bool_Val n2 -> Bool_Val (n1 || n2)
        |_ -> raise TypeError)
    |Not (e1) ->
        let n1 = eval_expr e1 env in
        (match n1 with
        |Bool_Val n1 -> Bool_Val (not n1)
        |_ -> raise TypeError)
    |Lt (e1, e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Int_Val n1, Int_Val n2 -> Bool_Val (n1 < n2)
        |_ -> raise TypeError)
    |Leq (e1, e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
 |Int_Val n1, Int_Val n2 -> Bool_Val (n1 <= n2)
        |_ -> raise TypeError)
    |Eq (e1, e2) ->
        let n1 = eval_expr e1 env in
        let n2 = eval_expr e2 env in
        (match n1, n2 with
        |Int_Val n1, Int_Val n2 -> Bool_Val (n1 = n2)
        |_ -> raise TypeError)



(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
    match c with
    |Skip ->
        env

    |Comp (e1, e2) ->
        eval_command e2 (eval_command e1 env)
  |Declare (e1, e2) ->
        (match e1 with
        |Int_Type -> env @ [(e2, Int_Val(0))]
        |Bool_Type -> env @ [(e2, Bool_Val(false))])

    |Assg (e1, e2) ->
        let x = eval_expr e2 env in
        if (List.mem_assoc e1 env) then
         let y = List.assoc e1 env in
         (match x,y with
         |Int_Val a, Int_Val b -> (e1,x)::env
         |Bool_Val a, Bool_Val b -> (e1,x)::env
         |_ -> raise TypeError)
        else raise UndefinedVar

    |Cond (e1, e2, e3) ->
        let x = eval_expr e1 env in
        (match x with
        |Bool_Val true-> eval_command e2 env
        |Bool_Val false -> eval_command e3 env
 |_ -> raise TypeError)


    |While (e1, e2) ->
        let x = eval_expr e1 env in
        (match x with
        |Bool_Val true-> eval_command (While(e1,e2)) (eval_command e2 env)
        |Bool_Val false -> env
        |_ -> raise TypeError)

    |For (e1, e2) ->
        let x = eval_expr e1 env in
        (match x with
        |Int_Val 0 -> env
        |Int_Val n -> eval_command (For(Number(n-1),e2)) (eval_command e2 env)
        |_ -> raise TypeError)

