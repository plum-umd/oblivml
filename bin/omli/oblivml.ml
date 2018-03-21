open Cmd
open Error
open Lexing

let main () =
  setup ();

  match !input with
  | None   ->
     handle ErrUnimpl "This should launch a REPL, but I haven't gotten around to it yet."
  | Some s ->
     let input_chan =
       match s with
       | "^" -> stdin
       | _   -> open_in s
     in

     let lexbuf = Lexing.from_channel input_chan in
     lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = s };

     let expr =
       (try
          Parser.start Lexer.token lexbuf
        with
        | SyntaxError (p, msg) ->
           handle ErrSyntax (Printf.sprintf "%s\n%s" (Error.string_of_pos p) msg))
     in

     handle ErrUnimpl "Need to fix lexer, parser, and then call type checker + eval"

(*
  if_debug (fun () -> print_endline (expr_to_string expr));
  let (t, env) = typecheck Scope.empty Constraints.empty Var.Map.empty Var.Map.empty expr in
  print_endline (Type.to_string t);
  print_endline (TEnv.to_string env)
 *)
;;

main ()
