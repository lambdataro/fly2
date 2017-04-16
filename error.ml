(*==================================================*
 * error.ml - Fly 2.1
 * 2017/04/17 @lambdataro
 *==================================================*)

(* 実行時エラー *)
exception FlyError of string * int * string

(* 実行時エラーを発生させる *)
let errorf pos fmt =
  let open Printf in
  ksprintf (fun str ->
    if pos = Lexing.dummy_pos then
      raise (FlyError ("", 0, str))
    else
      raise (FlyError (pos.Lexing.pos_fname, pos.Lexing.pos_lnum, str))
  ) fmt
