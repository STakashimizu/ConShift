type __ = Obj.t

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

val app : 'a1 list -> 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

type positive =
| XI of positive
| XO of positive
| XH

type z =
| Z0
| Zpos of positive
| Zneg of positive

type t =
| New

type command = __

type answer = __

type 'x t0 =
| Ret of 'x
| Call of command
| Let of __ t0 * (__ -> 'x t0)
| Choose of 'x t0 * 'x t0
| Join of __ t0 * __ t0

val ret : t -> 'a1 -> 'a1 t0

val call : t -> command -> answer t0

type t1 = char list

module Option :
 sig
  val bind : 'a1 option -> ('a1 -> 'a2 option) -> 'a2 option
 end

module LString :
 sig
  val of_string : char list -> t1

  val s : char list -> t1

  type t = char list

  module Char :
   sig
    val n : char
   end
 end

type t2 =
| ListFiles of LString.t
| ReadFile of LString.t
| WriteFile of LString.t * LString.t
| DeleteFile of LString.t
| System of LString.t
| Eval of LString.t list
| Print of LString.t
| ReadLine

val effect : t

val printl : LString.t -> bool t0

val log : LString.t -> unit t0

val apply : ('a1 -> 'a2) -> 'a1 -> 'a2

module BigInt :
 sig
  type t = Big_int.big_int

  val to_Z_aux :
    t -> 'a1 -> ('a2 -> 'a1) -> ('a2 -> 'a1) -> 'a2 -> ('a2 -> 'a2) -> ('a2
    -> 'a2) -> 'a1

  val to_Z : t -> z
 end

module String :
 sig
  type t = string

  val of_lstring : LString.t -> t

  val to_lstring : t -> LString.t
 end

module Sys :
 sig
  val argv : String.t list
 end

module Lwt :
 sig
  type 'a t = 'a Lwt.t

  val ret : 'a1 -> 'a1 t

  val bind : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t

  val join : 'a1 t -> 'a2 t -> ('a1 * 'a2) t

  val choose : 'a1 t -> 'a1 t -> 'a1 t

  val launch : 'a1 t -> 'a1

  val list_files : String.t -> String.t list option t

  val read_file : String.t -> String.t option t

  val write_file : String.t -> String.t -> bool t

  val delete_file : String.t -> bool t

  val system : String.t -> bool option t

  val eval : String.t list -> ((BigInt.t * String.t) * String.t) option t

  val print : String.t -> bool t

  val read_line : unit -> String.t option t
 end

val eval_command : command -> answer Lwt.t

val eval0 : 'a1 t0 -> 'a1 Lwt.t

val launch0 : (LString.t list -> unit t0) -> unit

val hello_world : LString.t list -> unit t0

val main : unit
