type __ = Obj.t

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some a -> Some (f a)
| None -> None

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t3 -> (f a) :: (map f t3)

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

(** val ret : t -> 'a1 -> 'a1 t0 **)

let ret _ x =
  Ret x

(** val call : t -> command -> answer t0 **)

let call _ command0 =
  Call command0

type t1 = char list

module Option =
 struct
  (** val bind : 'a1 option -> ('a1 -> 'a2 option) -> 'a2 option **)

  let bind x f =
    match x with
    | Some x0 -> f x0
    | None -> None
 end

module LString =
 struct
  (** val of_string : char list -> t1 **)

  let rec of_string = function
  | [] -> []
  | c::s1 -> c :: (of_string s1)

  (** val s : char list -> t1 **)

  let s =
    of_string

  type t = char list

  module Char =
   struct
    (** val n : char **)

    let n =
      '\n'
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

(** val effect : t **)

let effect =
  New

(** val printl : LString.t -> bool t0 **)

let printl message =
  Obj.magic call effect (Print (app message (LString.Char.n :: [])))

(** val log : LString.t -> unit t0 **)

let log message =
  Let ((Obj.magic printl message), (fun _ -> ret effect ()))

(** val apply : ('a1 -> 'a2) -> 'a1 -> 'a2 **)

let apply f x =
  f x

module BigInt =
 struct
  type t = Big_int.big_int

  (** val to_Z_aux :
      t -> 'a1 -> ('a2 -> 'a1) -> ('a2 -> 'a1) -> 'a2 -> ('a2 -> 'a2) -> ('a2
      -> 'a2) -> 'a1 **)

  let to_Z_aux = IoSystem.Big.to_Z_aux

  (** val to_Z : t -> z **)

  let to_Z big =
    to_Z_aux big Z0 (fun x -> Zpos x) (fun x -> Zneg x) XH (fun x -> XO x)
      (fun x -> XI x)
 end

module String =
 struct
  type t = string

  (** val of_lstring : LString.t -> t **)

  let of_lstring = IoSystem.String.of_lstring

  (** val to_lstring : t -> LString.t **)

  let to_lstring = IoSystem.String.to_lstring
 end

module Sys =
 struct
  (** val argv : String.t list **)

  let argv = IoSystem.argv
 end

module Lwt =
 struct
  type 'a t = 'a Lwt.t

  (** val ret : 'a1 -> 'a1 t **)

  let ret = Lwt.return

  (** val bind : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t **)

  let bind = Lwt.bind

  (** val join : 'a1 t -> 'a2 t -> ('a1 * 'a2) t **)

  let join = IoSystem.join

  (** val choose : 'a1 t -> 'a1 t -> 'a1 t **)

  let choose = IoSystem.choose

  (** val launch : 'a1 t -> 'a1 **)

  let launch = Lwt_main.run

  (** val list_files : String.t -> String.t list option t **)

  let list_files = IoSystem.list_files

  (** val read_file : String.t -> String.t option t **)

  let read_file = IoSystem.read_file

  (** val write_file : String.t -> String.t -> bool t **)

  let write_file = IoSystem.write_file

  (** val delete_file : String.t -> bool t **)

  let delete_file = IoSystem.delete_file

  (** val system : String.t -> bool option t **)

  let system = IoSystem.system

  (** val eval :
      String.t list -> ((BigInt.t * String.t) * String.t) option t **)

  let eval = IoSystem.eval

  (** val print : String.t -> bool t **)

  let print = IoSystem.print

  (** val read_line : unit -> String.t option t **)

  let read_line = IoSystem.read_line
 end

(** val eval_command : command -> answer Lwt.t **)

let eval_command c =
  match Obj.magic c with
  | ListFiles folder ->
    Lwt.bind (apply Lwt.list_files (String.of_lstring folder)) (fun files ->
      apply (Obj.magic Lwt.ret)
        (Option.bind files (fun files0 -> Some
          (map String.to_lstring files0))))
  | ReadFile file_name ->
    Lwt.bind (apply Lwt.read_file (String.of_lstring file_name))
      (fun content ->
      apply (Obj.magic Lwt.ret) (option_map String.to_lstring content))
  | WriteFile (file_name, content) ->
    let file_name0 = String.of_lstring file_name in
    let content0 = String.of_lstring content in
    Obj.magic Lwt.write_file file_name0 content0
  | DeleteFile file_name ->
    apply (Obj.magic Lwt.delete_file) (String.of_lstring file_name)
  | System command0 -> Obj.magic Lwt.system (String.of_lstring command0)
  | Eval command0 ->
    let command1 = map String.of_lstring command0 in
    Lwt.bind (Lwt.eval command1) (fun result ->
      Lwt.ret
        (apply
          (Obj.magic option_map (fun result0 ->
            let (y, err) = result0 in
            let (status, output) = y in
            (((BigInt.to_Z status), (String.to_lstring output)),
            (String.to_lstring err)))) result))
  | Print message ->
    let message0 = String.of_lstring message in Obj.magic Lwt.print message0
  | ReadLine ->
    Lwt.bind (Lwt.read_line ()) (fun line ->
      apply (Obj.magic Lwt.ret) (option_map String.to_lstring line))

(** val eval0 : 'a1 t0 -> 'a1 Lwt.t **)

let rec eval0 = function
| Ret x0 -> Lwt.ret x0
| Call command0 -> Obj.magic eval_command command0
| Let (x0, f) -> Lwt.bind (Obj.magic eval0 x0) (fun x1 -> eval0 (f x1))
| Choose (x1, x2) -> Lwt.choose (eval0 x1) (eval0 x2)
| Join (x0, y) ->
  Obj.magic Lwt.join (eval0 (Obj.magic x0)) (eval0 (Obj.magic y))

(** val launch0 : (LString.t list -> unit t0) -> unit **)

let launch0 main0 =
  let argv0 = map String.to_lstring Sys.argv in
  Lwt.launch (eval0 (main0 argv0))

(** val hello_world : LString.t list -> unit t0 **)

let hello_world _ =
  log
    (LString.s
      ('H'::('e'::('l'::('l'::('o'::(' '::('w'::('o'::('r'::('l'::('d'::('!'::[])))))))))))))

(** val main : unit **)

let main =
  launch0 hello_world
