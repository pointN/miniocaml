open Syntax;;

let emptyenv() = []
let ext env x v = (x,v) :: env
let rec lookup x env = 
    match env with
    |   [] -> failwith ("unbound variable: " ^ x)
    |   (y,v)::tl -> if x=y then v else lookup x tl

let rec listeq a b = 
  match (a,b) with
  | ([],[]) -> true
  | (_,[]) -> false
  | ([],_) -> false
  | (h1::t1,h2::t2) ->
      begin
        match (h1,h2) with
        | (IntVal(n1),IntVal(n2)) ->
            if n1=n2 then listeq t1 t2
            else false
        | (BoolVal(b1),BoolVal(b2)) ->
            if b1=b2 then listeq t1 t2
            else false
        | (ListVal(l1),ListVal(l2)) ->
            if listeq l1 l2 then listeq t1 t2
            else false
        | _ -> failwith "wrong value"
      end

let rec listgt a b = 
  match (a,b) with
  | ([],[]) -> false
  | (_,[]) -> true
  | ([],_) -> false
  | (h1::t1,h2::t2) ->
      begin
        match (h1,h2) with
        | (IntVal(n1),IntVal(n2)) ->
            if      n1>n2 then true
            else if n1<n2 then false
            else    listgt t1 t2
        | (BoolVal(b1),BoolVal(b2)) ->
            if      b1>b2 then true
            else if b1<b2 then false
            else    listgt t1 t2
        | (ListVal(l1),ListVal(l2)) ->
            if      listgt l1 l2 then true
            else if listlt l1 l2 then false
            else    listgt t1 t2
        | _ -> failwith "wrong value"
      end
and 
listlt a b = 
  match (a,b) with
  | ([],[]) -> false
  | (_,[]) -> false
  | ([],_) -> true
  | (h1::t1,h2::t2) ->
      begin
        match (h1,h2) with
        | (IntVal(n1),IntVal(n2)) ->
            if      n1<n2 then true
            else if n1>n2 then false
            else    listlt t1 t2
        | (BoolVal(b1),BoolVal(b2)) ->
            if      b1<b2 then true
            else if b1>b2 then false
            else    listlt t1 t2
        | (ListVal(l1),ListVal(l2)) ->
            if      listlt l1 l2 then true
            else if listgt l1 l2 then false
            else    listlt t1 t2
        | _ -> failwith "wrong value"
      end


let rec eval e env = 
  let binop f e1 e2 env = 
    match (eval e2 env, eval e1 env) with
    | (IntVal(n2),IntVal(n1)) -> 
        (try let res = f n1 n2 in IntVal(res) (* IntVal(f n1 n2) *)
         with Division_by_zero -> failwith "Division by zero")
    | _ -> failwith "integer value expected"
  in
  match e with
  | Var(x) -> lookup x env
  | IntLit(n) -> IntVal(n)
  | Plus(e1,e2) -> binop (+) e1 e2 env
  | Minus(e1,e2) -> binop (-) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Div(e1,e2) -> binop (/) e1 e2 env 
  | BoolLit(b) -> BoolVal(b)
  | Eq(e1,e2) -> 
      begin
        match (eval e2 env, eval e1 env) with
        | (IntVal(n2),IntVal(n1)) -> BoolVal(n1=n2)
        | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1=b2)
        | (ListVal(l2),ListVal(l1))-> 
            (try
               let res = listeq l1 l2 in BoolVal(res)
             with Failure(_) -> failwith "wrong value in Eq")
        | _ -> failwith "wrong value in Eq"
      end
  | Neq(e1,e2) -> 
      begin
        match (eval e2 env, eval e1 env) with
        | (IntVal(n2),IntVal(n1)) -> BoolVal(n1<>n2)
        | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1<>b2)
        | (ListVal(l2),ListVal(l1)) ->
            (try
               let res = listeq l1 l2 in BoolVal(not res)
             with Failure(_) -> failwith "wrong value in Neq")
        | _ -> failwith "wrong value in Neq"
      end
  | Greater(e1,e2) ->
      begin
        match (eval e2 env, eval e1 env) with
        | (IntVal(n2),IntVal(n1)) -> BoolVal(n1>n2)
        | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1>b2)
        | (ListVal(l2),ListVal(l1)) ->
            (try
               let res = listgt l1 l2 in BoolVal(res)
             with Failure(_) -> failwith "wrong value in Greater")
        | _ -> failwith "wrong value in Greater"
      end
  | Less(e1,e2) -> 
      begin
        match (eval e2 env, eval e1 env) with
        | (IntVal(n2),IntVal(n1)) -> BoolVal(n1<n2)
        | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1<b2)
        | (ListVal(l2),ListVal(l1)) ->
            (try
               let res = listlt l1 l2 in BoolVal(res)
             with Failure(_) -> failwith "wrong value in Less")
        | _ -> failwith "wrong value in Less"

      end
  | If(e1,e2,e3) -> 
      begin
        match (eval e1 env) with
        | BoolVal(true) -> eval e2 env
        | BoolVal(false) -> eval e3 env
        | _ -> failwith "wrong value in If"
      end
  | Let(x,e1,e2) ->
      let env1 = ext env x (eval e1 env)
      in eval e2 env1
  | Fun(x,e1) -> FunVal(x,e1,env)
  | App(e1,e2) -> 
      let arg = (eval e2 env) in
      let funpart = (eval e1 env) in
        begin
          match funpart with
          | FunVal(x,body,env1) ->
              let env2 = (ext env1 x arg) in
              eval body env2
          | RecFunVal(f,x,body,env1) ->
              let env2 = (ext (ext env1 x arg) f funpart) in 
              eval body env2
          | _ -> failwith "wrong value in App"
        end
  | LetRec(f,x,e1,e2) ->
      let env1 = ext env f (RecFunVal (f,x,e1,env))
      in eval e2 env1
  | Empty -> ListVal([])
  | Cons(e1,e2) -> 
      begin
        match (eval e1 env, eval e2 env) with
        | (v1,ListVal(v2)) -> ListVal(v1::v2)
        | _ -> failwith "list value expected in 2nd element"
      end
  | Head(e1) ->
      begin
        match (eval e1 env) with
        | ListVal(v1) -> 
            (try List.hd v1 with Failure(_) -> failwith "Hd failed")
        | _ -> failwith "list value expected"
      end
  | Tail(e1) -> 
      begin
        match (eval e1 env) with
        | ListVal(v1) -> 
            (try ListVal(List.tl v1) with Failure(_) -> failwith "Tl failed")
        | _ -> failwith "list value expected"
      end 
  | _ -> failwith "unknown expression"

let rec tostr v = 
  match v with
  | IntVal(n) -> string_of_int n
  | BoolVal(b) -> string_of_bool b
  | ListVal(l) ->
      let rec lstr s = 
        match s with
        | [] -> ""
        | h::t ->
           let hs =
             match h with
             | IntVal(n) -> string_of_int n
             | BoolVal(b) -> string_of_bool b
             | ListVal(_) -> tostr h
             | FunVal(_,_,_) | RecFunVal(_,_,_,_) -> "<fun>"
             | _ -> failwith "unknown value"
           in
           if t=[] then hs
           else hs ^ "; " ^ (lstr t)
      in "[" ^ (lstr l) ^ "]"
  | FunVal(_,_,_) | RecFunVal(_,_,_,_) -> "<fun>"
  | _ -> failwith "unknown value"