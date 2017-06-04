type __ = Obj.t

type nat =
| O
| S of nat

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some a -> Some (f a)
| None -> None

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

(** val rev_append : 'a1 list -> 'a1 list -> 'a1 list **)

let rec rev_append l l' =
  match l with
  | [] -> l'
  | a :: l0 -> rev_append l0 (a :: l')

(** val rev' : 'a1 list -> 'a1 list **)

let rev' l =
  rev_append l []

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t3 -> (f a) :: (map f t3)

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q -> XI (add p q)
       | XO q -> XO (add p q)
       | XH -> XI p)
    | XH ->
      (match y with
       | XI q -> XO (succ q)
       | XO q -> XI q
       | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XI (add_carry p q)
       | XO q -> XO (add_carry p q)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q -> XI (succ q)
       | XO q -> XO (succ q)
       | XH -> XI XH)

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont r p q
       | XO q -> compare_cont Gt p q
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont Lt p q
       | XO q -> compare_cont r p q
       | XH -> Gt)
    | XH ->
      (match y with
       | XH -> r
       | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq
 end

module N =
 struct
  (** val add : n -> n -> n **)

  let add n0 m =
    match n0 with
    | N0 -> m
    | Npos p ->
      (match m with
       | N0 -> n0
       | Npos q -> Npos (Pos.add p q))

  (** val mul : n -> n -> n **)

  let mul n0 m =
    match n0 with
    | N0 -> N0
    | Npos p ->
      (match m with
       | N0 -> N0
       | Npos q -> Npos (Pos.mul p q))

  (** val compare : n -> n -> comparison **)

  let compare n0 m =
    match n0 with
    | N0 ->
      (match m with
       | N0 -> Eq
       | Npos _ -> Lt)
    | Npos n' ->
      (match m with
       | N0 -> Gt
       | Npos m' -> Pos.compare n' m')
 end

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

(** val n_of_digits : bool list -> n **)

let rec n_of_digits = function
| [] -> N0
| b :: l' ->
  N.add (if b then Npos XH else N0) (N.mul (Npos (XO XH)) (n_of_digits l'))

(** val n_of_ascii : char -> n **)

let n_of_ascii a =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun a0 a1 a2 a3 a4 a5 a6 a7 ->
    n_of_digits
      (a0 :: (a1 :: (a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: [])))))))))
    a

(** val compare0 : char -> char -> comparison **)

let compare0 x y =
  N.compare (n_of_ascii x) (n_of_ascii y)

(** val eqb : char -> char -> bool **)

let eqb x y =
  match compare0 x y with
  | Eq -> true
  | _ -> false

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
  (** val to_string : t1 -> char list **)

  let rec to_string = function
  | [] -> []
  | c :: s1 -> c::(to_string s1)

  (** val of_string : char list -> t1 **)

  let rec of_string = function
  | [] -> []
  | c::s1 -> c :: (of_string s1)

  (** val s : char list -> t1 **)

  let s =
    of_string

  (** val join : t1 -> t1 list -> t1 **)

  let rec join separator = function
  | [] -> []
  | s0 :: l0 ->
    (match l0 with
     | [] -> s0
     | _ :: _ -> app s0 (app separator (join separator l0)))

  (** val split_aux : t1 -> char -> t1 -> t1 list **)

  let rec split_aux s0 c beginning =
    match s0 with
    | [] -> (rev' beginning) :: []
    | c' :: s1 ->
      if eqb c c'
      then (rev' beginning) :: (split_aux s1 c [])
      else split_aux s1 c (c' :: beginning)

  (** val split : t1 -> char -> t1 list **)

  let split s0 c =
    split_aux s0 c []

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

(** val read_file : LString.t -> LString.t option t0 **)

let read_file file_name =
  Obj.magic call effect (ReadFile file_name)

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

(** val inc : nat -> LString.t -> LString.t **)

let rec inc n0 line =
  let str = LString.to_string line in
  (match n0 with
   | O -> LString.s str
   | S n' ->
     inc n'
       (LString.s
         (let rec inc' st = match st with
          | [] -> st
          | a::s0 ->
            (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
              (fun b b0 b1 b2 b3 b4 b5 b6 ->
              if b
              then if b0
                   then if b1
                        then st
                        else if b2
                             then st
                             else if b3
                                  then st
                                  else if b4
                                       then if b5
                                            then st
                                            else if b6
                                                 then st
                                                 else '#'::('#'::s0)
                                       else st
                   else st
              else if b0
                   then st
                   else if b1
                        then st
                        else if b2
                             then st
                             else if b3
                                  then st
                                  else if b4
                                       then if b5
                                            then st
                                            else if b6
                                                 then st
                                                 else ' '::(inc' s0)
                                       else st)
              a
          in inc' str)))

(** val nr : char **)

let nr =
  '\n'

(** val map_line : (t1 -> t1) -> LString.t -> LString.t **)

let map_line f code =
  LString.join (LString.s (nr::[])) (map f (LString.split code nr))

(** val cat'' : LString.t list -> unit t0 **)

let cat'' = function
| [] ->
  log
    (LString.s
      ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('o'::('n'::('e'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('.'::[]))))))))))))))))))))))))
| _ :: l ->
  (match l with
   | [] ->
     log
       (LString.s
         ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('o'::('n'::('e'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('.'::[]))))))))))))))))))))))))
   | file_name :: l0 ->
     (match l0 with
      | [] ->
        Let ((Obj.magic read_file file_name), (fun content ->
          match Obj.magic content with
          | Some content0 -> log (map_line (inc (S O)) content0)
          | None ->
            log
              (LString.s
                ('C'::('a'::('n'::('n'::('o'::('t'::(' '::('r'::('e'::('a'::('d'::(' '::('t'::('h'::('e'::(' '::('f'::('i'::('l'::('e'::('.'::[]))))))))))))))))))))))))
      | _ :: _ ->
        log
          (LString.s
            ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('o'::('n'::('e'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('.'::[]))))))))))))))))))))))))))

(** val main : unit **)

let main =
  launch0 cat''
