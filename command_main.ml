(*==================================================*
 * command_main.ml - Fly 2.1
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

(* ファイルを構文解析 *)
let parse_file fname =
  let open Lexing in
  let ich =
    try open_in fname
    with Sys_error _ ->
      errorf dummy_pos "ファイル \'%s\' を開くのに失敗" fname
  in
  let lexbuf = Lexing.from_channel ich in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = fname };
  try Parser.main Lexer.token lexbuf
  with Parsing.Parse_error ->
    errorf lexbuf.lex_curr_p "構文エラー"

(* 初期化 *)
let init () =
  Global.global_print_ref := (fun str -> print_string str; flush stdout)

(* メイン *)
let main () =
  init ();
  try
    if Array.length Sys.argv < 2 then
      begin
        print_endline "プログラム言語 Fly 2.1.0";
        errorf Lexing.dummy_pos "入力ファイルがない";
      end;    
    let stdlib_items = parse_string Stdlib.stdlib_string in
    let prog_items = parse_file Sys.argv.(1) in
    let expr = Block (None, stdlib_items @ prog_items) in
    let _ = Eval.start_eval expr in
    ()
  with FlyError (fname, line, message) ->
    if line <> 0 then
      Printf.eprintf "エラー %s(%d): %s\n" fname line message
    else
      Printf.eprintf "エラー: %s\n" message

let () = main ()
