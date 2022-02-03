open Main;;

let test str = 
  print_endline ("test: "^str);
  print_endline ("---Interpreter---");
  (try (run str) with Failure(x) -> print_endline(x));
  print_endline ("-------CAM-------");
  (try (camrun str) with Failure(x) -> print_endline(x));
  print_endline ("-------ZAM-------");
  (try (zamrun str) with Failure(x) -> print_endline(x));;


(* 正しく実行できる例 *)
let code = "1+1" in
test code;;

(* 構文エラー *)
let code = "1+" in
test code;;

let code = "Lst.hd [1;2;3]" in
test code;;

(* 存在しない変数 *)
let code = "let x = 1 in y" in
test code;;

(* 型エラー *)
let code = "1+true" in 
test code;;

let code = "(fun x -> x+1) true)" in
test code;;

let code = "fun x -> if x then x else 10" in
test code;;

let code = "1::1" in
test code;;

let code = "1::(List.tl [false])" in
test code;;

(* ゼロ除算 *)
let code = "let x = 1 in let y = 1 in x/(x-y)" in
test code;;

(* 関数比較 *)
let code = "let f = fun x -> x + 1 in f=f" in 
test code;;

let code = "let f = fun x -> x+1 in let g = fun y -> y-1 in [f]=[g]" in
test code;;

(* 空リスト *)
let code = "List.hd (List.hd [[]])" in
test code;;

let code = "List.tl []" in
test code;;


(* 実行関数は単体では型の整合性判定をしない *)
(* 計算時に型が一致しない場合は例外が発生 *)

(* エラーの例 *)
let code = "1+true" in
let e = parse code in
print_endline("!!! run without type inference !!!");
print_endline ("test: "^code);
print_endline ("---Interpreter---");
print_endline (try Eval.tostr (Eval.eval e []) with Failure(x) -> x);
print_endline ("-------CAM-------");
print_endline (try Cam.tostr (Cam.execute (Cam.compile e []) [] []) with Failure(x) -> x);
print_endline ("-------ZAM-------");
print_endline (try Zam.tostr (Zam.execute (Zam.compile e []) [] [] []) with Failure(x) -> x);;

(* エラーにはならない例 *)
let code = "1::[false]" in
let e = parse code in
print_endline("!!! run without type inference !!!");
print_endline ("test: "^code);
print_endline ("---Interpreter---");
print_endline (Eval.tostr (Eval.eval e []));
print_endline ("-------CAM-------");
print_endline (Cam.tostr (Cam.execute (Cam.compile e []) [] []));
print_endline ("-------ZAM-------");
print_endline (Zam.tostr (Zam.execute (Zam.compile e []) [] [] []));;
