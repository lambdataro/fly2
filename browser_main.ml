(*==================================================*
 * browser_main.ml - Fly 2.1
 * 2017/04/17 @lambdataro
 *==================================================*)

open Error
open Syntax

(* 文字列を構文解析 *)
let parse_string str =
  let open Lexing in
  let lexbuf = Lexing.from_string str in
  try Parser.main Lexer.token lexbuf
  with Parsing.Parse_error ->
    errorf lexbuf.lex_curr_p "構文エラー"

(* 出力用バッファ *)
let buf = Buffer.create 256

(* 初期化 *)
let init () =
  Buffer.clear buf;
  Global.global_print_ref := (fun str -> Buffer.add_string buf str)

(* 実行 *)
let run_fly_code str =
  try
    init ();
    let stdlib_items = parse_string Stdlib.stdlib_string in
    let prog_items = parse_string str in
    let expr = Block (None, stdlib_items @ prog_items) in
    let _ = Eval.start_eval expr in
    Buffer.contents buf
  with
  | FlyError (_, line, message) ->
    if line <> 0 then
      Printf.sprintf "エラー(%d行目): %s\n" line message
    else
      Printf.sprintf "エラー: %s\n" message
  | Stack_overflow ->
    "エラー: スタックオーバーフロー\n"
  | _ ->
    "エラー: 不明なエラー\n"

(* js_of_ocaml エクスポート *)
let _ = Js.export "runFlyCode" @@
  Js.wrap_callback (fun str -> Js.string (run_fly_code (Js.to_string str)))
