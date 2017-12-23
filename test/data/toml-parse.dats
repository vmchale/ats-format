#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

staload UN = "prelude/SATS/unsafe.sats"
staload "src/types.sats"
staload "prelude/basics_sta.sats"
staload "libats/libc/SATS/stdio.sats"
staload "prelude/SATS/string.sats"

fun snoc(s : string, c : char) : string =
  let
    val sc = char2string(c)
    val x = string0_append(s, sc)
  in
    strptr2string(x)
  end

fun next {m : nat} (x : string(m)) : Option_vt(char) =
  if length(x) > 0 then
    Some_vt(string_head(x))
  else
    None_vt

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

fun compose {a : type}{b : type}{c : type} (f : a -<lincloptr1> b, g : b -<lincloptr1> c) : a -<lincloptr1> c =
  llam x =<lincloptr1> 
    begin
      let
        val y = g(f(x))
      in
        (cloptr_free($UN.castvwtp0(f)) ; (cloptr_free($UN.castvwtp0(g)) ; y))
      end
    end

fun on_snd {a : type}{b : type}{c : viewtype} (f : a -<lincloptr1> b) : (c, a) -<lincloptr1> (c, b) =
  llam (const, x) => let
    val z = f(x)
    val _ = cloptr_free($UN.castvwtp0(f))
  in
    (const, z)
  end

fun map {a : type}{b : type} (f : a -<lincloptr1> b, x : parser(a)) : parser(b) =
  let
    val g = x.modify
    val h = on_snd(f)
  in
    compose(g, h)
  end

fun consume_quoted() : parser(string) =
  let
    fun loop(input : cstream, is_escaped : bool, data : string) : (cstream, string) =
      case+ !input of
        | ~stream_vt_cons ('\\', xs) => loop(xs, true, data)
        | ~stream_vt_cons (x, xs) => 
          begin
            if not(is_escaped) then
              if x = '"' then
                (xs, data)
              else
                loop(xs, false, snoc(data, x))
            else
              loop(xs, false, snoc(data, x))
          end
        | ~stream_vt_nil() => ($ldelay(stream_vt_nil), "")
    
    fun data(input : cstream) : (cstream, string) =
      loop(input, false, "")
  in
    @{ modify = llam (input) =<lincloptr1> data(input) }
  end

// streamize_string_char
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