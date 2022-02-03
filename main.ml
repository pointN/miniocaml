(* main.ml *)

(* 構文定義ファイル syntax.ml で定義された exp型を使う *)
open Syntax ;;

(* 与えられた文字列の字句解析と構文解析だけを行う関数 *)
(* parse : string -> exp *)

let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)


(* ミニOCamlインタープリタ *)

(* 実行 ex. run "let x = ..." *)
let run str = 
  let e = (parse str) in
  let ts = 
    (try Tinf.tinf2top2 e
     with Failure(_) ->  failwith "Type error") in
  let res = Eval.tostr(Eval.eval e (Eval.emptyenv())) in
  print_string "- : "; print_string ts; print_string " = ";
  print_endline(res)

(* 実行 (value表現) *)
let runraw str =
  let e = (parse str) in
  let _ = Tinf.tinf2top e in
  Eval.eval e (Eval.emptyenv())



(* 型推論 *)

(* 評価した式の型のみ表示 ex. tinf "let x = ..." *)
let tinf str = print_endline (Tinf.tinf2top2 (parse str))

(* 元の実装 ex. tinf_raw "let x = ..." *)
let tinf_raw str = Tinf.tinf2top (parse str)



(* CAM *)

(* コンパイルして実行 ex. camrun "let x = ..." *)
let camrun str =
  let e = (parse str) in
  let ts = 
    (try Tinf.tinf2top2 e 
     with Failure(_) -> failwith "Type error") in
  let res = Cam.tostr(Cam.execute (Cam.compile e []) [] []) in
  print_string "- : "; print_string ts; print_string " = ";
  print_endline (res)

(* コンパイルのみ ex. camc "let x = ..." *)
let camc str = 
  let e = (parse str) in
  let _ =
    (try Tinf.tinf2top e 
     with Failure(_) -> failwith "Type error") in
  Cam.compile e []

(* 命令列実行 ex. camexe [CAM_...] *)
let camexe code = 
  Cam.execute code [] []

(* コンパイルして実行 (cam_value表現) *)
let camrunraw str = 
  let e = (parse str) in
  let _ = Tinf.tinf2top in
  Cam.execute (Cam.compile e []) [] []



(* ZAM *)

(* コンパイルして実行 ex.zumrun "let x = ..." *)
let zamrun str =
  let e = (parse str) in
  let ts = 
    (try Tinf.tinf2top2 e
     with Failure(_) -> failwith "Type error") in
  let res = Zam.tostr(Zam.execute (Zam.compile e []) [] [] []) in
  print_string "- : "; print_string ts; print_string " = ";
  print_endline (res)

(* コンパイルのみ ex.zamc "let x = ..." *)
let zamc str = 
  let e = (parse str) in
  let _ = 
    (try Tinf.tinf2top e
     with Failure(_) -> failwith "Type error") in
  Zam.compile e []

(* 命令列実行 ex.zamexe [ZAM_...] *)
let zamexe code = 
  Zam.execute code [] [] []

(* コンパイルして実行 (zam_value表現) *)
let zamrunraw str = 
  let e = (parse str) in
  let _ = Tinf.tinf2top in
  zamexe (Zam.compile e [])
