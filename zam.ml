open Syntax;;

type zam_instr = 
  | ZAM_Ldi of int
  | ZAM_Ldb of bool
  | ZAM_Access of int
  | ZAM_Closure of zam_code
  | ZAM_Let
  | ZAM_EndLet
  | ZAM_Test of zam_code * zam_code
  | ZAM_Add
  | ZAM_Sub
  | ZAM_Mul
  | ZAM_Div
  | ZAM_Eq
  | ZAM_Gt
  | ZAM_Lt
  | ZAM_Neq
  | ZAM_Empty
  | ZAM_Cons
  | ZAM_Hd
  | ZAM_Tl
  | ZAM_Apply
  | ZAM_TailApply
  | ZAM_PushMark
  | ZAM_Grab
  | ZAM_Return
and zam_code = zam_instr list

type zam_value = 
  | ZAM_IntVal of int
  | ZAM_BoolVal of bool
  | ZAM_ListVal of zam_value list
  | ZAM_ClosVal of zam_code * zam_env
  | ZAM_Epsilon
and zam_stack = zam_value list
and zam_env = zam_value list

let rec listeq a b = 
  match (a,b) with
  | ([],[]) -> true
  | (_,[]) -> false
  | ([],_) -> false
  | (h1::t1,h2::t2) ->
      begin
        match (h1,h2) with
        | (ZAM_IntVal(n1),ZAM_IntVal(n2)) ->
            if n1=n2 then listeq t1 t2
            else false
        | (ZAM_BoolVal(b1),ZAM_BoolVal(b2)) ->
            if b1=b2 then listeq t1 t2
            else false
        | (ZAM_ListVal(l1),ZAM_ListVal(l2)) ->
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
        | (ZAM_IntVal(n1),ZAM_IntVal(n2)) ->
            if      n1>n2 then true
            else if n1<n2 then false
            else    listgt t1 t2
        | (ZAM_BoolVal(b1),ZAM_BoolVal(b2)) ->
            if      b1>b2 then true
            else if b1<b2 then false
            else    listgt t1 t2
        | (ZAM_ListVal(l1),ZAM_ListVal(l2)) ->
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
        | (ZAM_IntVal(n1),ZAM_IntVal(n2)) ->
            if      n1<n2 then true
            else if n1>n2 then false
            else    listlt t1 t2
        | (ZAM_BoolVal(b1),ZAM_BoolVal(b2)) ->
            if      b1<b2 then true
            else if b1>b2 then false
            else    listlt t1 t2
        | (ZAM_ListVal(l1),ZAM_ListVal(l2)) ->
            if      listlt l1 l2 then true
            else if listgt l1 l2 then false
            else    listlt t1 t2
        | _ -> failwith "wrong value"
      end

let rec execute code env argstk rtnstk = 
  match code with
  | [] -> 
      if List.length argstk <> 1 then failwith "argstk size is not 1"
      else if List.length rtnstk <> 0 then failwith "rtnstk is not empty"
      else if List.length env <> 0 then failwith "env is not empty"
      else List.hd argstk
  | h::t ->
    begin
      match h with
      | ZAM_Ldi (n) -> execute t env (ZAM_IntVal(n)::argstk) rtnstk
      | ZAM_Ldb (b) -> execute t env (ZAM_BoolVal(b)::argstk) rtnstk
      | ZAM_Access (i) -> execute t env ((List.nth env i)::argstk) rtnstk
      | ZAM_Closure (c) -> execute t env (ZAM_ClosVal(c,env)::argstk) rtnstk
      | ZAM_Let -> 
          begin
            match argstk with
            | (v::s) -> execute t (v::env) s rtnstk
            | _ -> failwith "Let failed"
          end
      | ZAM_EndLet -> 
          begin
            match env with
            | (v::env2) -> execute t env2 argstk rtnstk
            | _ -> failwith "EndLet failed"
          end
      | ZAM_Test(c1,c2) -> 
          begin
            match argstk with
            | ((ZAM_BoolVal(true))::s) -> execute (c1@t) env s rtnstk
            | ((ZAM_BoolVal(false))::s) -> execute (c2@t) env s rtnstk
            | _ -> failwith "Test failed"
          end
      | ZAM_Add ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> execute t env (ZAM_IntVal(n1+n2)::s) rtnstk
            | _ -> failwith "Add failed"
          end
      | ZAM_Sub ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> execute t env (ZAM_IntVal(n1-n2)::s) rtnstk
            | _ -> failwith "Sub failed"
          end
      | ZAM_Mul ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> execute t env (ZAM_IntVal(n1*n2)::s) rtnstk
            | _ -> failwith "Mul failed"
          end
      | ZAM_Div ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> 
                if n2=0 then failwith "Division by zero"
                else execute t env (ZAM_IntVal(n1/n2)::s) rtnstk
            | _ -> failwith "Div failed"
          end
      | ZAM_Eq ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> execute t env (ZAM_BoolVal(n1=n2)::s) rtnstk
            | (ZAM_BoolVal(b1)::ZAM_BoolVal(b2)::s) -> execute t env (ZAM_BoolVal(b1=b2)::s) rtnstk
            | (ZAM_ListVal(l1)::ZAM_ListVal(l2)::s) -> 
                (try
                   let res = listeq l1 l2 in execute t env (ZAM_BoolVal(res)::s) rtnstk
                 with Failure(_) -> failwith "functional value compared")
            | (ZAM_ClosVal(_,_)::ZAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Eq failed"
          end
      | ZAM_Gt ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> execute t env (ZAM_BoolVal(n1>n2)::s) rtnstk
            | (ZAM_BoolVal(b1)::ZAM_BoolVal(b2)::s) -> execute t env (ZAM_BoolVal(b1>b2)::s) rtnstk
            | (ZAM_ListVal(l1)::ZAM_ListVal(l2)::s) -> 
                (try
                   let res = listgt l1 l2 in execute t env (ZAM_BoolVal(res)::s) rtnstk
                 with Failure(_) -> failwith "functional value compared")
            | (ZAM_ClosVal(_,_)::ZAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Gt failed"
          end
      | ZAM_Lt ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> execute t env (ZAM_BoolVal(n1<n2)::s) rtnstk
            | (ZAM_BoolVal(b1)::ZAM_BoolVal(b2)::s) -> execute t env (ZAM_BoolVal(b1<b2)::s) rtnstk
            | (ZAM_ListVal(l1)::ZAM_ListVal(l2)::s) -> 
                (try
                   let res = listlt l1 l2 in execute t env (ZAM_BoolVal(res)::s) rtnstk
                 with Failure(_) -> failwith "functional value compared")
            | (ZAM_ClosVal(_,_)::ZAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Lt failed"
          end
      | ZAM_Neq ->
          begin
            match argstk with
            | (ZAM_IntVal(n1)::ZAM_IntVal(n2)::s) -> execute t env (ZAM_BoolVal(n1<>n2)::s) rtnstk
            | (ZAM_BoolVal(b1)::ZAM_BoolVal(b2)::s) -> execute t env (ZAM_BoolVal(b1<>b2)::s) rtnstk
            | (ZAM_ListVal(l1)::ZAM_ListVal(l2)::s) -> 
                (try
                   let res = listeq l1 l2 in execute t env (ZAM_BoolVal(not res)::s) rtnstk
                 with Failure(_) -> failwith "functional value compared")
            | (ZAM_ClosVal(_,_)::ZAM_ClosVal(_,_)::s) -> failwith "functional value compared"
            | _ -> failwith "Neq failed"
          end
      | ZAM_Empty -> execute t env (ZAM_ListVal([])::argstk) rtnstk
      | ZAM_Cons ->
          begin
            match argstk with
            | (hd::ZAM_ListVal(l)::s) -> execute t env (ZAM_ListVal(hd::l)::s) rtnstk
            | _ -> failwith "Cons failed"
          end
      | ZAM_Hd ->
          begin
            match argstk with
            | (ZAM_ListVal(l1)::s) ->
                 if l1=[] then failwith "Hd failed: List is empty"
                 else execute t env ((List.hd l1)::s) rtnstk
            | _ -> failwith "Hd failed"
          end
      | ZAM_Tl ->
          begin
            match argstk with
            | (ZAM_ListVal(l1)::s) ->
                if l1=[] then failwith "Tl failed: List is empty"
                else execute t env (ZAM_ListVal(List.tl l1)::s) rtnstk
            | _ -> failwith "Tl failed"
          end
      | ZAM_Apply ->
          begin
            match argstk with
            | (ZAM_ClosVal(c2,env2)::v::s) -> execute c2 (v::ZAM_ClosVal(c2,env2)::env2) s (ZAM_ClosVal(t,env)::rtnstk)
            | _ -> failwith "Apply failed"
          end
      | ZAM_TailApply ->
          begin
            match argstk with
            | (ZAM_ClosVal(c2,env2)::v::s) -> execute c2 (v::ZAM_ClosVal(c2,env2)::env2) s rtnstk
            | _ -> failwith "TailApply failed"
          end
      | ZAM_PushMark -> execute t env (ZAM_Epsilon::argstk) rtnstk
      | ZAM_Grab ->
          begin
            match (argstk, rtnstk) with
            | (ZAM_Epsilon::s, ZAM_ClosVal(c2,env2)::r) -> execute c2 env2 (ZAM_ClosVal(t,env)::s) r
            | (v::s, r) -> execute t (v::ZAM_ClosVal(t,env)::env) s r
            | _ -> failwith "Grab failed"
          end
      | ZAM_Return ->
          begin
            match (argstk,rtnstk) with
            | (v::ZAM_Epsilon::s, ZAM_ClosVal(c2,env2)::r) -> execute c2 env2 (v::s) r
            | (ZAM_ClosVal(c2,env2)::v::s, r) -> execute c2 (v::ZAM_ClosVal(c2,env2)::env2) s r
            | _ -> failwith "Return failed"
          end
    end

let rec position (x: string) (venv: string list) : int = 
  match venv with
  | [] -> failwith "no matching variable in environment"
  | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec compile_c e venv = 
  match e with
  | Var(x) -> [ZAM_Access(position x venv)]
  | Fun(x,e1) -> [ZAM_Closure(compile_t e1 (x::"_"::venv))]
  | App(e1,e2) ->
      let rec appchain e lst = 
        match e with
        | App(e11,e12) -> appchain e11 (lst@(compile_c e12 venv))
        | _ -> lst@(compile_c e venv)@[ZAM_Apply]
      in appchain e [ZAM_PushMark]
  | Let(x,e1,e2) -> (compile_c e1 venv)@ZAM_Let::(compile_c e2 (x::venv))@[ZAM_EndLet]
  | LetRec(f,x,e1,e2) -> ZAM_Closure(compile_t e1 (x::f::venv))::ZAM_Let::(compile_c e2 (f::venv))@[ZAM_EndLet]  
  | IntLit(n)  -> [ZAM_Ldi(n)]
  | BoolLit(b) -> [ZAM_Ldb(b)]
  | Plus(e1,e2)  -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Add]
  | Minus(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Sub]
  | Times(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Mul]
  | Div(e1,e2)   -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Div]
  | Eq(e1,e2)      -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Eq]
  | Greater(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Gt]
  | Less(e1,e2)    -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Lt]
  | Neq(e1,e2)     -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Neq]
  | If(e1,e2,e3) -> (compile_c e1 venv)@[ZAM_Test((compile_c e2 venv),(compile_c e3 venv))]
  | Empty -> [ZAM_Empty]
  | Cons(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Cons]
  | Head(e1) -> (compile_c e1 venv)@[ZAM_Hd]
  | Tail(e1) -> (compile_c e1 venv)@[ZAM_Tl]
  | _ -> failwith "unknown expression"
and
compile_t e venv = 
  match e with
  | Var(x) -> [ZAM_Access(position x venv);ZAM_Return]
  | Fun(x,e1) -> ZAM_Grab::(compile_t e1 (x::"_"::venv))
  | App(e1,e2) ->
      let rec appchain e lst = 
        match e with
        | App(e11,e12) -> appchain e11 (lst@(compile_c e12 venv))
        | _ -> lst@(compile_c e venv)@[ZAM_TailApply]
      in appchain e []
  | Let(x,e1,e2) -> (compile_c e1 venv)@ZAM_Let::(compile_t e2 (x::venv))
  | LetRec(f,x,e1,e2) -> ZAM_Closure((compile_t e1 (x::f::venv)))::ZAM_Let::(compile_t e2 (f::venv))
  | IntLit(n)  -> [ZAM_Ldi(n);ZAM_Return]
  | BoolLit(b) -> [ZAM_Ldb(b);ZAM_Return]
  | Plus(e1,e2)  -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Add;ZAM_Return]
  | Minus(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Sub;ZAM_Return]
  | Times(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Mul;ZAM_Return]
  | Div(e1,e2)   -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Div;ZAM_Return]
  | Eq(e1,e2)      -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Eq;ZAM_Return]
  | Greater(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Gt;ZAM_Return]
  | Less(e1,e2)    -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Lt;ZAM_Return]
  | Neq(e1,e2)     -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Neq;ZAM_Return]
  | If(e1,e2,e3) -> (compile_c e1 venv)@[ZAM_Test((compile_t e2 venv),(compile_t e3 venv))]
  | Empty -> [ZAM_Empty;ZAM_Return]
  | Cons(e1,e2) -> (compile_c e2 venv)@(compile_c e1 venv)@[ZAM_Cons;ZAM_Return]
  | Head(e1) -> (compile_c e1 venv)@[ZAM_Hd;ZAM_Return]
  | Tail(e1) -> (compile_c e1 venv)@[ZAM_Tl;ZAM_Return]
  | _ -> failwith "unknown expression"

let compile e venv = compile_c e venv


let rec tostr cv = 
  match cv with
  | ZAM_IntVal(n) -> string_of_int n
  | ZAM_BoolVal(b) -> string_of_bool b
  | ZAM_ListVal(l) -> 
      let rec lstr s = 
        match s with
        | [] -> ""
        | h::t ->
           let hs = (
             match h with
             | ZAM_IntVal(n) -> string_of_int n
             | ZAM_BoolVal(b) -> string_of_bool b
             | ZAM_ListVal(l) -> tostr h
             | ZAM_ClosVal(_,_) -> "<fun>"
             | _ -> "print failed"
           ) in
           if t=[] then hs
           else hs ^ "; " ^ (lstr t)
      in "[" ^ (lstr l) ^ "]"
  | ZAM_ClosVal(_,_) -> "<fun>"
  | _ -> failwith "print failed"
