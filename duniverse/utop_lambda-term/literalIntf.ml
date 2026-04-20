module type Type =
  sig
    type char_intf
    type string_intf
    val empty_string : unit -> string_intf
    val of_char : Utop_zed_char.t -> char_intf
    val of_string : Utop_zed_string.t -> string_intf
    val to_char : char_intf -> Utop_zed_char.t option * Uchar.t list
    val to_string : string_intf -> Utop_zed_string.t * Uchar.t list
    val to_char_exn : char_intf -> Utop_zed_char.t
    val to_string_exn : string_intf -> Utop_zed_string.t
  end

module Utop_zed : Type
  with type char_intf= Utop_zed_char.t
  and type string_intf= Utop_zed_string.t =
struct
  let id x = x
  type char_intf= Utop_zed_char.t
  type string_intf= Utop_zed_string.t

  let empty_string ()= Utop_zed_string.empty ()
  let of_char= id
  let of_string= id
  let to_char ch= Some ch, []
  let to_string str= str, []
  let to_char_exn= id
  let to_string_exn= id
end

module UTF8 : Type
  with type char_intf= Utop_zed_utf8.t
  and type string_intf= Utop_zed_utf8.t =
struct
  type char_intf= Utop_zed_utf8.t
  type string_intf= Utop_zed_utf8.t

  let empty_string ()= ""
  let of_char= Utop_zed_char.to_utf8
  let of_string= Utop_zed_string.to_utf8
  let to_char x = Utop_zed_char.of_uChars (Utop_zed_utf8.explode x)
  let to_string x = Utop_zed_string.of_uChars (Utop_zed_utf8.explode x)
  let to_char_exn x = match to_char x with Some t, _ -> t | _ -> failwith "to_t_exn"
  let to_string_exn = Utop_zed_string.unsafe_of_utf8
end

