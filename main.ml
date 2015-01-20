open Syntax

let version = "プログラム言語 Fly 2.0.1"

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

(* メイン *)
let main () =
  if Array.length Sys.argv < 2 then
    begin
      print_endline version;
      errorf Lexing.dummy_pos "入力ファイルがない";
    end;
  let lib_dir = Filename.dirname Sys.executable_name in
  let stdlib = parse_file (Filename.concat lib_dir "stdlib.fly") in
  let items = parse_file Sys.argv.(1) in
  let expr = Block (None, stdlib @ items) in
  let _ = Eval.start_eval expr in
  ()

let () = main ()

