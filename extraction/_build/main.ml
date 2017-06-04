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

module Coq__1 = struct
 (** val add : nat -> nat -> nat **)
 let rec add n0 m =
   match n0 with
   | O -> m
   | S p -> S (add p m)
end
let add = Coq__1.add


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
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end

module Coq_Pos =
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

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double_mask (sub_mask p q)
       | XO q -> succ_double_mask (sub_mask p q)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XH ->
      (match y with
       | XH -> IsNul
       | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q -> double_mask (sub_mask_carry p q)
       | XO q -> succ_double_mask (sub_mask_carry p q)
       | XH -> double_pred_mask p)
    | XH -> IsNeg

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

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a

  (** val to_nat : positive -> nat **)

  let to_nat x =
    iter_op Coq__1.add x (S O)
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
       | Npos q -> Npos (Coq_Pos.add p q))

  (** val sub : n -> n -> n **)

  let sub n0 m =
    match n0 with
    | N0 -> N0
    | Npos n' ->
      (match m with
       | N0 -> n0
       | Npos m' ->
         (match Coq_Pos.sub_mask n' m' with
          | Coq_Pos.IsPos p -> Npos p
          | _ -> N0))

  (** val mul : n -> n -> n **)

  let mul n0 m =
    match n0 with
    | N0 -> N0
    | Npos p ->
      (match m with
       | N0 -> N0
       | Npos q -> Npos (Coq_Pos.mul p q))

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
       | Npos m' -> Coq_Pos.compare n' m')

  (** val leb : n -> n -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : n -> n -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val to_nat : n -> nat **)

  let to_nat = function
  | N0 -> O
  | Npos p -> Coq_Pos.to_nat p
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

(** val to_N : char -> n option **)

let to_N c =
  let n0 = n_of_ascii c in
  if (&&) (N.leb (n_of_ascii '0') n0) (N.leb n0 (n_of_ascii '9'))
  then Some (N.sub n0 (n_of_ascii '0'))
  else if N.leb (n_of_ascii 'A') n0
       then Some (N.add (N.sub n0 (n_of_ascii 'A')) (Npos (XO (XI (XO XH)))))
       else None

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

  (** val to_N_aux : n -> t1 -> n option **)

  let rec to_N_aux base = function
  | [] -> Some N0
  | c :: s1 ->
    Option.bind (to_N c) (fun d ->
      if (&&) (N.leb N0 d) (N.ltb d base)
      then Option.bind (to_N_aux base s1) (fun n0 -> Some
             (N.add d (N.mul base n0)))
      else None)

  (** val to_N : n -> t1 -> n option **)

  let to_N base s0 =
    to_N_aux base (rev' s0)

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

(** val dec : nat -> LString.t -> LString.t **)

let rec dec n0 line =
  let str = LString.to_string line in
  (match n0 with
   | O -> LString.s str
   | S n' ->
     dec n'
       (LString.s
         (let rec dec' line0 = match line0 with
          | [] -> line0
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
                        then line0
                        else if b2
                             then line0
                             else if b3
                                  then line0
                                  else if b4
                                       then if b5
                                            then line0
                                            else if b6
                                                 then line0
                                                 else (match s0 with
                                                       | [] -> line0
                                                       | a0::s1 ->
                                                         (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
                                                           (fun b7 b8 b9 b10 b11 b12 b13 b14 ->
                                                           if b7
                                                           then if b8
                                                                then 
                                                                  if b9
                                                                  then line0
                                                                  else 
                                                                    if b10
                                                                    then 
                                                                    line0
                                                                    else 
                                                                    if b11
                                                                    then 
                                                                    line0
                                                                    else 
                                                                    if b12
                                                                    then 
                                                                    if b13
                                                                    then 
                                                                    line0
                                                                    else 
                                                                    if b14
                                                                    then 
                                                                    line0
                                                                    else 
                                                                    '#'::s1
                                                                    else 
                                                                    line0
                                                                else line0
                                                           else line0)
                                                           a0)
                                       else line0
                   else line0
              else if b0
                   then line0
                   else if b1
                        then line0
                        else if b2
                             then line0
                             else if b3
                                  then line0
                                  else if b4
                                       then if b5
                                            then line0
                                            else if b6
                                                 then line0
                                                 else ' '::(dec' s0)
                                       else line0)
              a
          in dec' str)))

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

(** val new_line : char **)

let new_line =
  '\n'

(** val map_line : (t1 -> t1) -> LString.t -> LString.t **)

let map_line f code =
  LString.join (LString.s (new_line::[]))
    (map f (LString.split code new_line))

(** val is_minus : LString.t -> bool **)

let is_minus num =
  match LString.to_string num with
  | [] -> false
  | a::_ ->
    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
      (fun b b0 b1 b2 b3 b4 b5 b6 ->
      if b
      then if b0
           then false
           else if b1
                then if b2
                     then if b3
                          then false
                          else if b4
                               then if b5
                                    then false
                                    else if b6 then false else true
                               else false
                     else false
                else false
      else false)
      a

(** val lstr2nat : LString.t -> nat option **)

let lstr2nat num =
  let abs_num =
    match LString.to_string num with
    | [] -> LString.s []
    | a::n0 ->
      (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
        (fun b b0 b1 b2 b3 b4 b5 b6 ->
        if b
        then if b0
             then LString.s
                    (((* If this appears, you're using Ascii internals. Please don't *)
 (fun (b0,b1,b2,b3,b4,b5,b6,b7) ->
  let f b i = if b then 1 lsl i else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))
                    (true, true, b1, b2, b3, b4, b5, b6))::n0)
             else if b1
                  then if b2
                       then if b3
                            then LString.s
                                   (((* If this appears, you're using Ascii internals. Please don't *)
 (fun (b0,b1,b2,b3,b4,b5,b6,b7) ->
  let f b i = if b then 1 lsl i else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))
                                   (true, false, true, true, true, b4, b5,
                                   b6))::n0)
                            else if b4
                                 then if b5
                                      then LString.s
                                             (((* If this appears, you're using Ascii internals. Please don't *)
 (fun (b0,b1,b2,b3,b4,b5,b6,b7) ->
  let f b i = if b then 1 lsl i else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))
                                             (true, false, true, true, false,
                                             true, true, b6))::n0)
                                      else if b6
                                           then LString.s ('\173'::n0)
                                           else LString.s n0
                                 else LString.s
                                        (((* If this appears, you're using Ascii internals. Please don't *)
 (fun (b0,b1,b2,b3,b4,b5,b6,b7) ->
  let f b i = if b then 1 lsl i else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))
                                        (true, false, true, true, false,
                                        false, b5, b6))::n0)
                       else LString.s
                              (((* If this appears, you're using Ascii internals. Please don't *)
 (fun (b0,b1,b2,b3,b4,b5,b6,b7) ->
  let f b i = if b then 1 lsl i else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))
                              (true, false, true, false, b3, b4, b5,
                              b6))::n0)
                  else LString.s
                         (((* If this appears, you're using Ascii internals. Please don't *)
 (fun (b0,b1,b2,b3,b4,b5,b6,b7) ->
  let f b i = if b then 1 lsl i else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))
                         (true, false, false, b2, b3, b4, b5, b6))::n0)
        else LString.s
               (((* If this appears, you're using Ascii internals. Please don't *)
 (fun (b0,b1,b2,b3,b4,b5,b6,b7) ->
  let f b i = if b then 1 lsl i else 0 in
  Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))
               (false, b0, b1, b2, b3, b4, b5, b6))::n0))
        a
  in
  (match LString.to_N (Npos (XO (XI (XO XH)))) abs_num with
   | Some n0 -> Some (N.to_nat n0)
   | None -> None)

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
   | num :: l0 ->
     (match l0 with
      | [] ->
        log
          (LString.s
            ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('o'::('n'::('e'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('.'::[]))))))))))))))))))))))))
      | file_name :: l1 ->
        (match l1 with
         | [] ->
           Let ((Obj.magic read_file file_name), (fun content ->
             match Obj.magic content with
             | Some content0 ->
               let f = if is_minus num then dec else inc in
               (match lstr2nat num with
                | Some n0 -> log (map_line (f n0) content0)
                | None ->
                  log
                    (LString.s
                      ('W'::('r'::('o'::('n'::('g'::(' '::('n'::('u'::('m'::('b'::('e'::('r'::(' '::('s'::('p'::('e'::('c'::('.'::[]))))))))))))))))))))
             | None ->
               log
                 (LString.s
                   ('C'::('a'::('n'::('n'::('o'::('t'::(' '::('r'::('e'::('a'::('d'::(' '::('t'::('h'::('e'::(' '::('f'::('i'::('l'::('e'::('.'::[]))))))))))))))))))))))))
         | _ :: _ ->
           log
             (LString.s
               ('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('o'::('n'::('e'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('.'::[])))))))))))))))))))))))))))

(** val main : unit **)

let main =
  launch0 cat''
