(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 OCamlPro                                                *)
(*                                                                            *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* This file is distributed under the terms of the GNU Lesser General Public  *)
(* License version 2.1, with the special exception on linking described in    *)
(* the LICENSE.md file in the root directory.                                 *)
(*                                                                            *)
(******************************************************************************)

let header_file = ref "HEADER"

let source_files = ref []

(* Safe open in *)
let (let<) x f =
  let in_chan = open_in x in
  try let res = f in_chan in close_in in_chan; res with
  | exn -> close_in in_chan; raise exn

(* Safe open out *)
let (let>) x f =
  let out_chan = open_out x in
  try let res = f out_chan in close_out out_chan; res with
  | exn -> close_out out_chan; raise exn

(* Checks if a line is a "(*********)" line *)
let is_border_line (line : Bytes.t) =
  let s = Bytes.to_seq line in
  match s () with
  | Seq.Nil -> false
  | Cons ('(', seq) ->
    let last = Bytes.length line - 1 in 
    Seq.for_all (
      let cpt = ref 1 in
      function
          | '*' -> incr cpt; true
          | ')' when !cpt = last -> true
          | _ -> false
      ) seq
  | _ -> false

(* Checks if a line is a "(*       *)" line. *)
let is_comment_line (line : Bytes.t) =
  let last = Bytes.length line - 1 in
  Bytes.get line 0 = '(' &&
  Bytes.get line 1 = '*' &&
  Bytes.get line (last - 1) = '*' &&
  Bytes.get line last = ')'

(* Takes an in_channel that is a ml file and eats its header, composed of:
   - a border line "(***********)"
   - comments "(* some text *)"
   - another border line "(**********)"
*)
let ignore_header (ic : in_channel) =
  let stop = ref false in                        (* [true] => stops the loop *)
  let in_header = ref false in          (* [true] => we are reading a header *)
  let useful_line_consumed = ref None in     (* Line to add back if consumed *)
  while not !stop do
    match input_line ic with
    | "" ->
       if !in_header
       then stop := true                              (* Badly formed header *)
    | l ->
       let b = Bytes.unsafe_of_string l in
       if is_border_line b then                   (* Line is "(*****...***)" *)
         if !in_header
         then stop := true                       (* Bottom line of the header*)
         else in_header := true                    (* Top line of the header *)
       else if is_comment_line b then              (* Line is "(*        *)" *)
         if not !in_header then                         (* Line is a comment *)
           stop := true
         else ()
       else begin                              (* Line is not a comment line *)
         stop := true;
         useful_line_consumed := Some l
       end
  done;
  !useful_line_consumed

(* Copies verbatim an in_channel into an out_channel *)
let copy_into (ic : in_channel) (oc : out_channel) =
  try
    input_line ic |> output_string oc;
    while true do
      output_char oc '\n';
      input_line ic |> output_string oc;       
    done
  with End_of_file -> ()

(* Duplicated from string.ml, splits on a list of chars *)
let split_on_chars seps s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if List.mem (String.unsafe_get s i) seps then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

(* Writes a border line "(***********)" *)
let write_border_line oc =
  output_char oc '(';
  for _ = 0 to 77 do output_char oc '*' done;
  output_char oc ')';
  output_char oc '\n'

(* Writes an empty comment line "(*          *)"*)
let write_empty_line oc =
  output_string oc "(*";
  for _ = 0 to 75 do output_char oc ' ' done;
  output_string oc "*)\n"

(* Writes a sequence of words as a header. *)
let write_header oc (header_words : string Seq.t) =
  let limit_per_line = 74 in (* 80 - the characters for commenting + margin *)
  (* The current position on the header line being written. *)
  let pos = ref 0 in
  (* Closes properly a comment line and reinitializes the position. *)
  let close_comment () =
    while !pos < limit_per_line do
      incr pos;
      output_char oc ' '
    done;
    output_string oc " *)\n";
    pos := 0
  in
  (* Starts a new line comment line *)
  let start_line () = output_string oc "(* " in
  (* Closes a comment line and starts a new one. *)
  let flush () =
    close_comment ();
    start_line ()
  in
  (* Write a word and updates the cursor position. *)
  let write_word w =
    output_string oc w;
    pos := !pos + String.length w;
    assert (!pos <= limit_per_line);
  in
  (* Writes a space if it is not the start of a line.
     If it is the end of a line, flushes. *)
  let write_space () =
    if !pos = limit_per_line then begin
        flush ()
      end
    else if !pos = 0 then ()
    else
      begin
        output_char oc ' ';
        incr pos
      end
  in
  let rec loop seq = match seq () with
    | Seq.Nil -> close_comment ()
    | Cons (word, rest) -> treat_word word rest
  and treat_word word rest =
    let wlen = String.length word in
    if wlen = 0 then begin (* This should be a new line *)
      if !pos <> 0 then flush (); flush (); loop rest
    end else if wlen >= limit_per_line then begin (* Word too long for the line *)
        if !pos <> 0 then flush ();
        let pre = String.sub word 0 limit_per_line
        and post = String.sub word limit_per_line (String.length word - limit_per_line) in
        write_word pre;
        flush ();
        treat_word post rest
      end else
      if wlen + !pos >= limit_per_line then begin
          (* Word too long for the rest of the line. *)
          flush ();
          write_word word;
          write_space ();
          loop rest
        end
      else begin
        write_word word;
        write_space ();
        loop rest
      end
  in
  write_border_line oc;
  write_empty_line oc;
  start_line ();
  loop header_words;
  write_empty_line oc;
  write_border_line oc


let update_header header ic oc =
  write_header oc header;
  let useful_line_opt = ignore_header ic in
  let () =
    match useful_line_opt with
    | None -> ()
    | Some l -> output_string oc l; output_char oc '\n'
  in
  copy_into ic oc

let space_chars =
  [' '; '\x0C'; '\n'; '\r'; '\t']

(* TODO: not a list *)
let header_words_seq header_file =
  let ic = open_in header_file in
  let res = ref "" in
  let close () = close_in ic in
  let () =
    try
      res := input_line ic;
      while true do
        let l = input_line ic in
        res := !res ^ "\n" ^ l
      done
    with
    | End_of_file -> close ();
    | exn -> close (); raise exn
  in
  let l = !res |> split_on_chars space_chars in
  List.to_seq l

let recopy_into_old ~old ~new_ =
  let< new_ml_file = new_ in
  let> old_ml_file = old  in
  copy_into new_ml_file old_ml_file

let run_for_file ~header_words f =
  let ml_res_file_name =
    Format.sprintf "/tmp/%s" (String.map (function '/' -> '_' | c -> c) f)
  in
  let () =
    let< ml_file = f in
    let> ml_res_file = ml_res_file_name in
    update_header header_words ml_file ml_res_file
  in
  recopy_into_old ~old:f ~new_:ml_res_file_name

let run () =
  let header_words = header_words_seq !header_file in
  List.iter (run_for_file ~header_words) !source_files

let usage_msg = "ez-header <file1> [<file2>] -H <header_file>"

let anon_fun filename =
  source_files := filename :: !source_files

let speclist =
  [("-H", Arg.String (fun s -> header_file := s), "Use this header file (default: HEADER)")]

let () =
  Arg.parse speclist anon_fun usage_msg;
  run ()
