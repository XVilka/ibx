open Core.Std
open Printf

type t = out_channel

let create () =
  let t = Unix.open_process_out "gnuplot" in
  output_string t "set term wxt enhanced persist\n";
  t

let close t = ignore (Unix.close_process_out t)
let send t cmd = output_string t (sprintf "%s\n" cmd)

let send_cols t cols =
  let max_rows = List.fold_left (List.map cols ~f:Array.length) ~init:0 ~f:max in
  for i = 0 to max_rows-1 do
    List.iter cols ~f:(fun col ->
      if i < Array.length col then fprintf t "%f " col.(i) else fprintf t "- ");
    fprintf t "\n%!";
  done;
  fprintf t "e\n%!"

let plot t data_opts =
  let data, opts = List.unzip data_opts in
  let cmd =
    List.map opts ~f:(sprintf "'-' %s")
    |> String.concat ~sep:","
    |> sprintf "plot %s"
  in
  send t cmd;
  List.iter data ~f:(fun cols -> send_cols t cols)
