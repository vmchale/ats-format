#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

staload "prelude/basics_sta.sats"
staload "libats/libc/SATS/stdio.sats"
staload "prelude/SATS/string.sats"

vtypedef pos = @{ filepath = string, line = int, col = int }

datavtype parser_state =
  | success of (string, string)
  | failure

datavtype plain_toml =
  | toml_str of string
  | toml_int of int
  | toml_floar of float
  | toml_bool of bool

datavtype toml =
  | toml_array of [ n : nat ] list_vt(plain_toml, n)
  | toml_table of [ n : nat ] list_vt((string, toml), n)

datavtype token =
  | string_tok of string
  | int_tok of int
  | eq_tok
  | pound_tok
  | float_tok of float
  | bool_tok of bool

vtypedef cstream = stream_vt(char)

vtypedef tstream = stream_vt(token)

fun consume(chars : cstream) : void =
  case+ !chars of
    | ~stream_vt_nil() => ()
    | ~stream_vt_cons (x, xs) => (print(x) ; consume(xs))

fun next {m : nat} (x : string(m)) : Option_vt(char) =
  if length(x) > 0 then
    Some_vt(string_head(x))
  else
    None_vt

fun snoc(s : string, c : char) : string =
  let
    val sc = char2string(c)
    val x = string0_append(s, sc)
  in
    strptr2string(x)
  end

// amb_str_st for bool/string, num_st for ints/floats, quoted_st for quoted strings
datavtype lexer_state =
  | amb_st of string
  | plain_st
  | quoted_st
  | num_st of string
  | float_st
  | lex_err
  | any_st

fun free_state(l : lexer_state) : void =
  case+ l of
    | ~amb_st (_) => ()
    | ~quoted_st() => ()
    | ~num_st (_) => ()
    | ~float_st() => ()
    | ~lex_err() => ()
    | ~any_st() => ()
    | ~plain_st() => ()

fun stop_plain_string(c : char) : bool =
  case+ c of
    | '\n' => true
    | '#' => true
    | _ => false

// TODO we need something like monadic parsers, possibly via dependent types?
fun consume_quoted(input : cstream) : token =
  let
    fun loop(input : cstream, is_escaped : bool, data : string) : string =
      case+ !input of
        | ~stream_vt_cons ('\\', xs) => loop(xs, true, data)
        | ~stream_vt_cons (x, xs) => 
          begin
            if not(is_escaped) then
              loop(xs, false, snoc(data, x))
            else
              loop(xs, false, data)
          end
        | ~stream_vt_nil() => ""
    
    val data = loop(input, false, "")
  in
    string_tok(data)
  end

/* streamize_string_char */
fun tokenize(input : cstream, st : lexer_state, toks : tstream) : tstream =
  case+ !input of
    | ~stream_vt_cons (x, xs) => 
      begin
        case- st of
          | ~amb_st (s) => (stream_vt_free(xs) ; toks)
          | ~any_st() => (stream_vt_free(xs) ; toks)
      end
    | ~stream_vt_nil() => (free_state(st) ; toks)

implement main0 () =
  let
    var fr = fileref_open_exn(".atsfmt.toml", file_mode_r)
    var stream = streamize_fileref_char(fr)
    val toks: tstream = tokenize(stream, any_st, $ldelay(stream_vt_nil))
    val _ = stream_vt_free(toks)
  in end