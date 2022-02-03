open Syntax;;

let ext env x v = (x,v) :: env
let rec lookup x env = 
    match env with
    |   [] -> failwith ("unbound variable: " ^ x)
    |   (y,v)::tl -> if x=y then v else lookup x tl

type tyvar = string
type ty = TInt | TBool | TArrow of ty * ty | TVar of tyvar | TList of ty
type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

let rec occurs tx t = 
  if tx = t then true
  else
    match t with
    | TArrow(t1,t2) -> (occurs tx t1) || (occurs tx t2)
    | _ -> false

(* subst_ty : tysubst -> ty -> ty *)
(* 代入thetaを型tに適用する *)
let rec subst_ty theta t = 
  let rec subst_ty1 theta1 s = 
    match theta1 with
    | [] -> TVar(s)
    | (tx,t1)::theta2 -> if tx=s then t1 else subst_ty1 theta2 s
  in
  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TArrow(t2,t3) -> TArrow(subst_ty theta t2, subst_ty theta t3)
  | TVar(s) -> subst_ty1 theta s
  | TList(l) -> TList(subst_ty theta l)

(* susbt_tyenv : tysubst -> tyenv -> tyenv *)
(* 代入thetaを型環境teに適用 *)
let subst_tyenv theta te = 
  List.map (fun (x,t) -> (x,subst_ty theta t)) te

(* subst_eql : tysubst -> (ty * ty) list -> (ty * ty) list *)
(* 代入thetaを型の等式のリストeqlに適用 *)
let subst_eql theta eql = 
  List.map (fun (t1,t2) -> (subst_ty theta t1, subst_ty theta t2)) eql

(* compose_subst : tysubst -> tysubst -> tysubst *)
(* 2つの代入を合成した代入を返す theta1が先 *)
let rec compose_subst theta2 theta1 = 
  let theta11 = 
    List.map (fun (tx,t) -> (tx, subst_ty theta2 t)) theta1
  in
  List.fold_left (fun tau -> fun (tx,t) -> 
    try let _ = lookup tx theta1 in tau
    with Failure(_) -> (tx,t)::tau) theta11 theta2

(* unify : (ty * ty) list -> tysubst *)
let unify eql = 
  let rec solve eql theta =
    match eql with
    | [] -> theta
    | (t1,t2)::eql2 ->
        if t1=t2 then solve eql2 theta
        else
          begin
            match (t1,t2) with
            | (TArrow(t11,t12),TArrow(t21,t22)) ->
                solve ((t11,t21)::(t12,t22)::eql2) theta
            | (TList(t11),TList(t12)) ->
                solve ((t11,t12)::eql2) theta
            | (TVar(s),_) ->
                if (occurs t1 t2) then failwith "unification failed"
                else solve (subst_eql [(s,t2)] eql2) (compose_subst [(s,t2)] theta)
            | (_,TVar(s)) ->
                if (occurs t2 t1) then failwith "unification failed"
                else solve (subst_eql [(s,t1)] eql2) (compose_subst [(s,t1)] theta)
            | (_,_) -> failwith "unification failed"
          end
  in solve eql []

(* remove : tyenv -> string -> tyenv *)
let rec remove env x = 
  match env with
  | [] -> []
  | (t,s)::env1 -> if t=x then env1 else (t,s)::(remove env1 x)


let theta0 = ([] : tysubst)

(* new_typevar : int -> ty * int *)
let new_typevar n = (TVar("'a" ^ (string_of_int n)), n+1)

(* tinf2 : tyenv -> exp -> int -> tyenv * ty * tysubst * int *)
let rec tinf2 te e n = 
  match e with
  | Var(s) -> (try let t1 = lookup s te in (te, t1, theta0, n)
               with Failure(_) ->
                 let (tx,n1) = new_typevar n in
                 let te1 = ext te s tx in
                 (te1, tx, theta0, n1))
  | IntLit(_) -> (te, TInt, theta0, n)
  | BoolLit(_) -> (te, TBool, theta0, n)
  | Plus(e1,e2) | Minus(e1,e2) | Times(e1,e2) | Div(e1,e2) ->
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TInt); (t2,TInt)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TInt, theta4, n2)
  | Eq(e1,e2) | Greater(e1,e2) | Less(e1,e2) | Neq(e1,e2) -> 
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,t2)] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TBool, theta4, n2)
  | If(e1,e2,e3) -> 
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let (te3, t3, theta3, n3) = tinf2 te2 e3 n2 in
      let t11 = subst_ty theta2 t1 in
      let t12 = subst_ty theta3 t11 in
      let t21 = subst_ty theta3 t2 in
      let theta4 = unify [(t12,TBool); (t21,t3)] in
      let t22 = subst_ty theta4 t21 in
      let te4 = subst_tyenv theta4 te3 in
      let theta5 = compose_subst theta4 (
                     compose_subst theta3 (
                       compose_subst theta2 theta1)) in
      (te4,t22,theta5, n3)
  | Fun(x,e) ->
      let (tx,n1) = new_typevar n in
      let te1 = ext te x tx in
      let (te2, t1, theta1, n2) = tinf2 te1 e n1 in
      let t2 = subst_ty theta1 tx in
      let te3 = remove te2 x in
      (te3, TArrow(t2,t1), theta1, n2)
  | App(e1,e2) ->
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let (tx,n3) = new_typevar n2 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(t11,TArrow(t2,tx))] in
      let t3 = subst_ty theta3 tx in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, t3, theta4, n3)
  | Let(x,e1,e2) -> tinf2 te (App(Fun(x,e2),e1)) n
  | LetRec(f,x,e1,e2) -> tinf2 te (Let(f,Fun(x,e1),e2)) n
  | Empty -> let (tx,n1) = new_typevar n in (te, TList(tx), theta0, n1)
  | Cons(e1,e2) -> 
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (te2, t2, theta2, n2) = tinf2 te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [(TList(t11),t2)] in
      let t3 = subst_ty theta3 t2 in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, t3, theta4, n2)
  | Head(e1) ->
      let (te1, t1, theta1, n1) = tinf2 te e1 n in
      let (tx, n2) = new_typevar n1 in
      let theta2 = unify [(TList(tx),t1)] in
      let t2 = subst_ty theta2 tx in
      let te2 = subst_tyenv theta2 te1 in
      let theta3 = compose_subst theta2 theta1 in
      (te2, t2, theta3, n2)
  | Tail(e1) -> tinf2 te e1 n
  | _ -> failwith "unknown expression"



(* 型推論の結果を文字列に、型変数名の振り直しは行わない 
let rec tostr ty flag = 
  match ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow(t1,t2) -> 
      if flag = 0 then (tostr t1 1) ^ " -> " ^ (tostr t2 0)
      else "(" ^ (tostr t1 1) ^ " -> " ^ (tostr t2 0) ^ ")"
  | TVar(s) -> s
  | TList(t) -> 
      match t with
      | TArrow(_,_) -> (tostr t 1) ^ " list"
      | _ -> (tostr t 0) ^ " list"

let tinf te e n = 
  let (_,t,_,_) = (tinf2 te e n) in
  tostr t 0
*)


(* 型推論の結果を文字列に、型変数名の振り直しを行う *)
let rec upddic s n dic = 
  match dic with
  | [] -> 
      let newname = "'a"^(string_of_int n)
      in (newname, n+1, [(s,"'a"^(string_of_int n))])
  | (nm1,nm2)::t ->
      if nm1 = s then (nm2,n,dic)
      else
        let (s1,n1,dic1) = (upddic s n t) in
        (s1,n1,(nm1,nm2)::dic1)

let rec tostr ty flag n dic = 
  match ty with
  | TInt -> ("int",n,dic)
  | TBool -> ("bool",n,dic)
  | TArrow(t1,t2) -> 
      let (s1,n1,dic1) = (tostr t1 1 n  dic ) in
      let (s2,n2,dic2) = (tostr t2 0 n1 dic1) in
      if flag = 0 then ((s1 ^ " -> " ^ s2),n2,dic2)
      else (("(" ^ s1 ^ " -> " ^ s2 ^ ")"),n2,dic2)
  | TVar(s) -> upddic s n dic
  | TList(t) -> 
      let (s,n1,dic1) =
        match t with
        | TArrow(_,_) -> (tostr t 1 n dic)
        | _ -> (tostr t 0 n dic)
      in ((s ^ " list"),n1,dic1)

let tinf te e n = 
  let (_,t,_,_) = (tinf2 te e n) in
  let (t1,_,_) = (tostr t 0 0 []) in
  t1

let tinf2top e = tinf2 [] e 0
let tinf2top2 e = tinf [] e 0