(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 OCamlPro                                                *)
(*                                                                            *)
(* Contributeurs:                                                             *)
(* - Steven de Oliveira <steven.de-oliveira@ocamlpro.com>                     *)
(*                                                                            *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* This file is distributed under the terms of the GNU Lesser General Public  *)
(* License version 2.1, with the special exception on linking described in    *)
(* the LICENSE.md file in the root directory.                                 *)
(*                                                                            *)
(******************************************************************************)

type header = | Unknown | Text of string | File of string

let header : header ref = ref Unknown

let source_files = ref []

(* Returns the length of an utf8 string *)
let utf8_length s =
  let len = ref 0 in
  for i = 0 to String.length s - 1 do
    let c = Char.code s.[i] in
    (* Count bytes that are not UTF-8 continuation bytes (10xxxxxx) *)
    if (c land 0xC0) <> 0x80 then
      incr len
  done;
  !len

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
let write_header oc (header_words : [`Newline | `Word of string] Seq.t) =
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
  let start_line () = 
    output_string oc "(* " in
  (* Closes a comment line and starts a new one. *)
  let flush () =
    close_comment ();
    start_line ();
  in
  (* Write a word and updates the cursor position. *)
  let write_word w =
    output_string oc w;
    pos := !pos + utf8_length w;
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
    | Seq.Nil -> ()
    | Cons (word, rest) -> treat_word word rest
  and treat_word word rest =
    match word with
    | `Newline -> flush (); loop rest
    | `Word word -> 
       let wlen = utf8_length word in
       if wlen >= limit_per_line then begin (* Word too long for the line *)
           if !pos <> 0 then flush ();
           let pre = String.sub word 0 limit_per_line
           and post = String.sub word limit_per_line (wlen - limit_per_line) in
           write_word pre;
           flush ();
           treat_word (`Word post) rest
         end else
         if wlen + !pos > limit_per_line then begin
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
  if !pos = 0
  then close_comment ()
  else begin close_comment (); write_empty_line oc end;
  write_border_line oc

let update_header header ic oc =
  write_header oc header;
  let useful_line_opt = ignore_header ic in
  let () =
    match useful_line_opt with
    | None -> ()
    | Some l ->
       if String.trim l <> "" then
         begin output_string oc l; output_char oc '\n' end
  in
  copy_into ic oc

let header_words_seq_of_text text =
  let i = ref 0 in
  let j = ref (-1) in
  let len = String.length text in
  let rec loop () =
    incr j;
    if !j = len then
      let word = String.sub text !i (!j - !i) in
      Seq.Cons (`Word word, fun () -> Seq.Nil)
    else
      match String.get text !j with
      | '\n' ->
         let word = String.sub text !i (!j - !i) in
         i := !j + 1;
         Seq.Cons (
           `Word word,
           fun () -> Seq.Cons (`Newline, loop))
      | ' ' | '\x0C' | '\r' | '\t' -> 
         let word = String.sub text !i (!j - !i) in
         i := !j + 1;
         Seq.Cons (`Word word, loop)
      | _ -> loop ()
  in loop

let header_words_seq_of_file file =
  let ic = open_in file in
  let word = ref "" in
  let rec loop () =
    try match input_char ic with
      | '\n' ->
         let w = !word in
         word := "";
         Seq.Cons (
           `Word w,
           fun () -> Seq.Cons (`Newline, loop))
      | ' ' | '\x0C' | '\r' | '\t' -> 
         let w = !word in
         word := "";
         Seq.Cons (`Word w, loop)
      | c ->
         word := Format.sprintf "%s%c" !word c; 
         loop ()
    with
    | End_of_file ->
       Seq.Cons (`Word !word, fun () -> close_in ic; Seq.Nil)
    | exn -> close_in ic; raise exn
  in
  loop

let header_words_seq () =
  match !header with
  | Unknown -> assert false (* Should be checked beforehand *)
  | File file -> header_words_seq_of_file file
  | Text t -> header_words_seq_of_text t

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
  let header_words = header_words_seq () in
  List.iter (run_for_file ~header_words) !source_files

let usage_msg = "ez-header <file1> [<file2>] (-H <header_file>) (--header <text>)"

let anon_fun filename =
  source_files := filename :: !source_files

let speclist = [
    "-H",
    Arg.String (fun s -> header := File s),
    "Use this header file (default: HEADER)";

    "--header",
    Arg.String (fun s -> header := Text s),
    "Use this header text"
  ]

let check_config () =
  if !source_files = []
  then begin
      Format.eprintf "Error: no files to add a header";
      exit 1
    end;
  if !header = Unknown then begin
      Format.eprintf "Error: no header provided (use -H <file>).";
      exit 2
    end   

let () =
  Format.printf "Run ez-header...@.";
  Arg.parse speclist anon_fun usage_msg;
  check_config ();
  run ()
