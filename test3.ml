open Main;;

(* タイマー *)
(* 参考 http://logic.cs.tsukuba.ac.jp/~sunaga/fp2017.html *)
let timer = 
  fun f -> 
    let start = Sys.time() in
    let _ = f () in
    let finish = Sys.time() in
    print_endline (
      "time: " ^
      (string_of_float ((finish-.start)*.1000.)) ^
      " msec."
    );;

let test str = 
  print_endline ("test: "^str);
  print_endline ("---Interpreter---");
  timer(fun () -> run str);
  print_endline ("-------CAM-------");
  timer(fun () -> camrun str);
  print_endline ("-------ZAM-------");
  timer(fun () -> zamrun str);;


(* 100000までの総和 *)
let code = "let rec sum x = if x=0 then 0 else x+sum(x-1) in sum 100000" in
test code;;

(* 100000までの総和(末尾再帰) *)
let code =
"let rec sum x = fun nowsum -> \
   if x=0 then nowsum else sum (x-1) (nowsum+x) \
 in sum 100000 0" in
test code;;

(* 配列構築 *)
let code = 
"let rec makeary n = \
   if n=0 then [] \
   else let tl = makeary (n-1) in (50001-n)::(tl) \
 in List.hd (makeary 50000)" in
test code;;

let code = 
"let rec makeary ary = fun n -> \
   if n=0 then ary \
   else makeary (n::ary) (n-1) \
 in List.hd (makeary [] 50000)" in
test code;;

(* フィボナッチ数列1,1,2,3...の第30項 *)
(* answer is 832040, https://univ-juken.com/fibonacci-suretsu *)
let code = 
"let rec fib x = \
   if x<3 then 1 \
   else fib (x-1) + fib (x-2) \
 in fib 30" in
test code;;

(* フィボナッチ数列1,1,2,3...の第30項 末尾再帰（計算量が小さいので1000回繰り返し総和をとる） *)
let code = 
"let rec fib n = fun a -> fun b -> \
   if n<3 then a+b \
   else fib (n-1) b (a+b) \
 in \
 let rec many n = fun sum -> \
   if n=0 then sum \
   else many (n-1) (sum + (fib 30 0 1))
 in many 1000 0" in
test code;;

(* コラッツ予想 1まで310ステップ要する最小数 *)
(* answer is 34239, http://www.ericr.nl/wondrous/delrecs.html *)
let code = 
"let rec collatz x = \
   if x=1 then 0 \
   else 1 + collatz (if x/2*2=x then x/2 else (3*x+1)) \
 in
 let rec collatzsearch n = \
   if collatz n = 310 then 1 \
   else 1 + collatzsearch (n+1) \
 in collatzsearch 1" in
test code;;

(* 末尾再帰 *)
let code = 
"let rec collatz x = fun n -> \
   if x=1 then n \
   else collatz (if x/2*2=x then x/2 else (3*x+1)) (n+1) \
 in \
 let rec collatzsearch n = \
   if collatz n 0 = 310 then n \
   else collatzsearch (n+1) \
 in collatzsearch 1"
in
test code;;

(* アッカーマン関数 A(3,8) *)
(* A(3,y) = 2^(y+3)-3, https://mathworld.wolfram.com/AckermannFunction.html *)
(* answer is 2^(8+3)-3  = 2045 *)
let code = 
"let rec ackermann m = fun n -> \
   if m=0 then n+1 \
   else if n=0 then ackermann (m-1) 1 \
   else ackermann (m-1) (ackermann m (n-1)) \
 in ackermann 3 8" in
test code;;