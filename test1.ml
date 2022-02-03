open Main;;

let test str = 
  print_endline ("test: "^str);
  print_endline ("---Interpreter---");
  run str;
  print_endline ("-------CAM-------");
  camrun str;
  print_endline ("-------ZAM-------");
  zamrun str;;

(* 四則演算 *)
let code = "1+2*3-15/4" in
test code;;

(* 符号反転 *)
let code = "---1" in
test code;;

(* 比較 *)
let code = "1<2" in
test code;;

let code = "false=true" in
test code;;

let code = "1*1<>2*2" in
test code;;

let code = "4>4" in
test code;;

(* if *)
let code = "if 1+1=2 then (if 1*1=2 then 0 else 1) else -1" in
test code;;

(* let, fun, app *)
let code = "let f = fun x -> x * 2 in let x = 3 in f x" in
test code;;

(* 関数値と型推論 *)
let code = "let f = fun x -> fun y -> if x then y else 1 in f" in
test code;;

let code = "let f = fun x -> fun y -> if x then y else 1 in f true 0" in
test code;;

(* 再帰関数 *)
let code = "let rec f x = if x=0 then 0 else x+f(x-1) in f 3" in
test code;;

(* 末尾再帰 *)
let code = "let rec f x = fun sum -> if x=0 then sum else f (x-1) (sum+x) in f 5 0" in
test code;;

(* リスト *)
let code = "[]" in
test code;;

let code = "(100)::[(1+1)]" in 
test code;;

let code = "List.hd [[1;2];[2]]" in
test code;;

let code = "List.tl [1;2;3]" in
test code;;

let code = "List.tl [1]" in 
test code;;

let code = "let x = [[1;2];[]] in (List.hd x)::(List.tl x)" in
test code;;

let code = "let f = fun x -> List.hd x + 5 in f [1;2;3]" in
test code;;

(* 大小比較の拡張 *)
let code = "if true<false then -1 else if true>false then 1 else 0" in
test code;;

let code = "[]<[1]" in
test code;;

let code = "[1;2;3]<[1;3;2]" in
test code;;

let code = "[[1;2];[2]] > [[1;2];[1;5]]" in
test code;;

(* 関数どうしの比較ではないのでエラーにならない例 *)
let code = "[] > [fun x -> x]" in
test code;;

(* 複雑な式と型推論 *)
(* OCamlの評価: (bool -> int) -> bool -> (bool -> bool) -> int = <fun> *)
let code = "fun f -> fun x -> fun g -> if x then (f x)+1 else f(g x)+1" in
test code;;

(* OCamlの評価: 'a -> ('a -> 'b) -> 'b = <fun> *)
let code = "fun x -> fun g -> g x" in
test code;;

(* OCamlの評価: ('a -> int) -> 'a -> (('a -> int) -> int -> int) -> bool = <fun> *)
let code = "fun a -> fun b -> fun c -> (a b)+1=((c a) 1)" in
test code;;

(* OCamlの評価: (('a -> 'b) -> 'c -> 'b) -> ('a -> 'b) -> ('a -> 'c) -> 'a -> bool = <fun> *)
let code = "fun a -> fun b -> fun c -> fun d -> (a b) (c d) = (b d)" in
test code;;

(* OCamlの評価: ('a -> int -> 'b) list list -> 'a -> 'b = <fun> *)
let code = "fun x -> fun y -> ((List.hd (List.hd x)) y) 1" in
test code;;