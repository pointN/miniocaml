open Syntax;;

type ty = TInt | TBool | TArrow of ty * ty;;
type tyenv = (string * ty) list;;

let rec lookup x env = 
    match env with
    |   [] -> failwith ("unbound variable: " ^ x)
    |   (y,v)::tl -> if x=y then v else lookup x tl;;

let rec tcheck3 te e = 
  match e with
  | Var(s) -> lookup s te
  | IntLit(_) -> TInt
  | BoolLit(_) -> TBool
  | Plus(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | Minus(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TInt,TInt) -> TInt
        | _ -> failwith "type error in Minus"
      end
  | Times(e1,e2) ->
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TInt,TInt) -> TInt
        | _ -> failwith "type error in Times"
      end
  | Div(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TInt,TInt) -> TInt
        | _ -> failwith "type error in Div"
      end
  | If(e1,e2,e3) ->
      begin
        match (tcheck3 te e1, tcheck3 te e2, tcheck3 te e3) with
        | (TBool,t1,t2) -> if t1=t2 then t1 else failwith "type error in IF"
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TBool,TBool) -> TBool
        | (TInt,TInt) -> TBool
        | _ -> failwith "type error in Eq"
      end
  | Greater(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TInt,TInt) -> TBool
        | _ -> failwith "type error in Greater"
      end
  | Less(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TInt,TInt) -> TBool
        | _ -> failwith "type error in Less"
      end
  | Neq(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
        | (TBool,TBool) -> TBool
        | (TInt,TInt) -> TBool
        | _ -> failwith "type error in Neq"
      end
  | Fun(x,e1) -> 
      let t1 = lookup x te in
      let t2 = tcheck3 te e1 in
        TArrow(t1,t2)
  | App(e1,e2) -> 
      let t1 = tcheck3 te e1 in
      let t2 = tcheck3 te e2 in
        begin
          match t1 with
          | TArrow(t10,t11) ->
              if t2=t10 then t11
              else failwith "type error in App"
          | _ -> failwith "type error in App"
        end
  | Let(x,e1,e2) ->
      (try tcheck3 te (App(Fun(x,e2),e1))
      with Failure(_) -> failwith "type error in Let")
  | LetRec(f,x,e1,e2) ->
      (try tcheck3 te (Let(f,Fun(x,e1),e2))
      with Failure(_) -> failwith "type error in LetRec")
  | _ -> failwith "unknown expression";;
