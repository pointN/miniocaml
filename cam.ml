open Syntax;;

type cam_instr = 
  | CAM_Ldi of int
  | CAM_Ldb of bool
  | CAM_Access of int
  | CAM_Closure of cam_code
  | CAM_Apply
  | CAM_Return
  | CAM_Let
  | CAM_EndLet
  | CAM_Test of cam_code * cam_code
  | CAM_Add
  | CAM_Sub
  | CAM_Mul
  | CAM_Div
  | CAM_Eq
  | CAM_Gt
  | CAM_Lt
  | CAM_Neq
  | CAM_Empty
  | CAM_Cons
  | CAM_Hd
  | CAM_Tl
and cam_code = cam_instr list

type cam_value = 
  | CAM_IntVal of int
  | CAM_BoolVal of bool
  | CAM_ListVal of cam_value list
  | CAM_ClosVal of cam_code * cam_env
and cam_stack = cam_value list
and cam_env = cam_value list

let rec listeq a b = 
  match (a,b) with
  | ([],[]) -> true
  | (_,[]) -> false
  | ([],_) -> false
  | (h1::t1,h2::t2) ->
      begin
        match (h1,h2) with
        | (CAM_IntVal(n1),CAM_IntVal(n2)) ->
            if n1=n2 then listeq t1 t2
            else false
        | (CAM_BoolVal(b1),CAM_BoolVal(b2)) ->
            if b1=b2 then listeq t1 t2
            else false
        | (CAM_ListVal(l1),CAM_ListVal(l2)) ->
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
        | (CAM_IntVal(n1),CAM_IntVal(n2)) ->
            if      n1>n2 then true
            else if n1<n2 then false
            else    listgt t1 t2
        | (CAM_BoolVal(b1),CAM_BoolVal(b2)) ->
            if      b1>b2 then true
            else if b1<b2 then false
            else    listgt t1 t2
        | (CAM_ListVal(l1),CAM_ListVal(l2)) ->
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
        | (CAM_IntVal(n1),CAM_IntVal(n2)) ->
            if      n1<n2 then true
            else if n1>n2 then false
            else    listlt t1 t2
        | (CAM_BoolVal(b1),CAM_BoolVal(b2)) ->
            if      b1<b2 then true
            else if b1>b2 then false
            else    listlt t1 t2
        | (CAM_ListVal(l1),CAM_ListVal(l2)) ->
            if      listlt l1 l2 then true
            else if listgt l1 l2 then false
            else    listlt t1 t2
        | _ -> failwith "wrong value"
      end

let rec execute code env stk = 
  match code with
  | [] ->
      if List.length env <> 0 then failwith "env is not empty"
      else if List.length stk <> 1 then failwith "stack size is not 1"
      else List.hd stk
  | h::t ->
    begin
      match h with
      | CAM_Ldi (n) -> execute t env (CAM_IntVal(n)::stk)
      | CAM_Ldb (b) -> execute t env (CAM_BoolVal(b)::stk)
      | CAM_Access (i) -> execute t env ((List.nth env i)::stk)
      | CAM_Closure (c) -> execute t env (CAM_ClosVal(c,env)::stk)
      | CAM_Apply -> 
          begin
            match stk with
            | (CAM_ClosVal(c,env2)::v::s) -> execute c (v::CAM_ClosVal(c,env2)::env2) (CAM_ClosVal(t,env)::s)
            | _ -> failwith "Apply failed"
          end
      | CAM_Return -> 
          begin
            match stk with
            | (v::CAM_ClosVal(c,env2)::s) -> execute c env2 (v::s)
            | _ -> failwith "Return failed"
          end
      | CAM_Let -> 
          begin
            match stk with
            | (v::s) -> execute t (v::env) s
            | _ -> failwith "Let failed"
          end
      | CAM_EndLet ->
          begin
            match env with
            | (v::env2) -> execute t env2 stk
            | _ -> failwith "EndLet failed"
          end
      | CAM_Test(c1,c2) ->
          begin
            match stk with
            | ((CAM_BoolVal(true))::s) -> execute (c1@t) env s
            | ((CAM_BoolVal(false))::s) -> execute (c2@t) env s
            | _ -> failwith "Test failed"
          end
      | CAM_Add ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> execute t env (CAM_IntVal(n1+n2)::s)
            | _ -> failwith "Add failed"
          end
      | CAM_Sub ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> execute t env (CAM_IntVal(n1-n2)::s)
            | _ -> failwith "Sub failed"
          end
      | CAM_Mul ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> execute t env (CAM_IntVal(n1*n2)::s)
            | _ -> failwith "Mul failed"
          end
      | CAM_Div ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> 
                if n2=0 then failwith "Division by zero"
                else execute t env (CAM_IntVal(n1/n2)::s)
            | _ -> failwith "Div failed"
          end
      | CAM_Eq ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> execute t env (CAM_BoolVal(n1=n2)::s)
            | (CAM_BoolVal(b1)::CAM_BoolVal(b2)::s) -> execute t env (CAM_BoolVal(b1=b2)::s)
            | (CAM_ListVal(l1)::CAM_ListVal(l2)::s) -> 
                (try
                   let res = listeq l1 l2 in execute t env (CAM_BoolVal(res)::s)
                 with Failure(_) -> failwith "functional value compared")
            | (CAM_ClosVal(_,_)::CAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Eq failed"
          end
      | CAM_Gt ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> execute t env (CAM_BoolVal(n1>n2)::s)
            | (CAM_BoolVal(b1)::CAM_BoolVal(b2)::s) -> execute t env (CAM_BoolVal(b1>b2)::s)
            | (CAM_ListVal(l1)::CAM_ListVal(l2)::s) -> 
                (try 
                   let res = listgt l1 l2 in execute t env (CAM_BoolVal(res)::s)
                 with Failure(_) -> failwith "functional value compared")
            | (CAM_ClosVal(_,_)::CAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Gt failed"
          end
      | CAM_Lt ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> execute t env (CAM_BoolVal(n1<n2)::s)
            | (CAM_BoolVal(b1)::CAM_BoolVal(b2)::s) -> execute t env (CAM_BoolVal(b1<b2)::s)
            | (CAM_ListVal(l1)::CAM_ListVal(l2)::s) -> 
                (try
                   let res = listlt l1 l2 in execute t env (CAM_BoolVal(res)::s)
                 with Failure(_) -> failwith "functional value compared")
            | (CAM_ClosVal(_,_)::CAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Lt failed"
          end
      | CAM_Neq ->
          begin
            match stk with
            | (CAM_IntVal(n1)::CAM_IntVal(n2)::s) -> execute t env (CAM_BoolVal(n1<>n2)::s)
            | (CAM_BoolVal(b1)::CAM_BoolVal(b2)::s) -> execute t env (CAM_BoolVal(b1<>b2)::s)
            | (CAM_ListVal(l1)::CAM_ListVal(l2)::s) -> 
                (try
                   let res = listeq l1 l2 in execute t env (CAM_BoolVal(not res)::s)
                 with Failure(_) -> failwith "functional value compared")
            | (CAM_ClosVal(_,_)::CAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Neq failed"
          end
      | CAM_Empty ->
          execute t env (CAM_ListVal([])::stk)
      | CAM_Cons ->
          begin
            match stk with
            | (hd::CAM_ListVal(l)::s) -> execute t env (CAM_ListVal(hd::l)::s)
            | _ -> failwith "Cons failed"
          end
      | CAM_Hd ->
          begin
            match stk with
            | (CAM_ListVal(l1)::s) -> 
                if l1=[] then failwith "Hd failed: List is empty"
                else execute t env ((List.hd l1)::s)
            | _ -> failwith "Hd failed"
          end
      | CAM_Tl ->
          begin
            match stk with
            | (CAM_ListVal(l1)::s) -> 
                if l1=[] then failwith "Tl failed: List is empty"
                else execute t env (CAM_ListVal(List.tl l1)::s)
            | _ -> failwith "Tl failed"
          end 
    end


let rec position (x: string) (venv: string list) : int = 
  match venv with
  | [] -> failwith "no matching variable in environment"
  | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec compile e venv = 
  match e with
  | Var(x) -> [CAM_Access(position x venv)]
  | Fun(x,e1) -> [CAM_Closure((compile e1 (x::"_"::venv))@[CAM_Return])]
  | App(e1,e2) -> (compile e2 venv)@(compile e1 venv)@[CAM_Apply]
  | Let(x,e1,e2) -> (compile e1 venv)@CAM_Let::(compile e2 (x::venv))@[CAM_EndLet]
  | LetRec(f,x,e1,e2) -> CAM_Closure((compile e1 (x::f::venv))@[CAM_Return])::CAM_Let::(compile e2 (f::venv))@[CAM_EndLet]
  | IntLit(n)  -> [CAM_Ldi(n)]
  | BoolLit(b) -> [CAM_Ldb(b)]
  | Plus(e1,e2)  -> (compile e2 venv)@(compile e1 venv)@[CAM_Add]
  | Minus(e1,e2) -> (compile e2 venv)@(compile e1 venv)@[CAM_Sub]
  | Times(e1,e2) -> (compile e2 venv)@(compile e1 venv)@[CAM_Mul]
  | Div(e1,e2)  -> (compile e2 venv)@(compile e1 venv)@[CAM_Div]
  | Eq(e1,e2)      -> (compile e2 venv)@(compile e1 venv)@[CAM_Eq]
  | Greater(e1,e2) -> (compile e2 venv)@(compile e1 venv)@[CAM_Gt]
  | Less(e1,e2)    -> (compile e2 venv)@(compile e1 venv)@[CAM_Lt]
  | Neq(e1,e2)     -> (compile e2 venv)@(compile e1 venv)@[CAM_Neq]
  | If(e1,e2,e3) -> (compile e1 venv)@[CAM_Test((compile e2 venv),(compile e3 venv))]
  | Empty -> [CAM_Empty]
  | Cons(e1,e2) -> (compile e2 venv)@(compile e1 venv)@[CAM_Cons]
  | Head(e1) -> (compile e1 venv)@[CAM_Hd]
  | Tail(e1) -> (compile e1 venv)@[CAM_Tl]
  | _ -> failwith "unknown expression"

let rec tostr cv = 
  match cv with
  | CAM_IntVal(n) -> string_of_int n
  | CAM_BoolVal(b) -> string_of_bool b
  | CAM_ListVal(l) -> 
      let rec lstr s = 
        match s with
        | [] -> ""
        | h::t ->
           let hs = (
             match h with
             | CAM_IntVal(n) -> string_of_int n
             | CAM_BoolVal(b) -> string_of_bool b
             | CAM_ListVal(l) -> tostr h
             | CAM_ClosVal(_,_) -> "<fun>"
           ) in
           if t=[] then hs
           else hs ^ "; " ^ (lstr t)
      in "[" ^ (lstr l) ^ "]"
  | CAM_ClosVal(_,_) -> "<fun>"
