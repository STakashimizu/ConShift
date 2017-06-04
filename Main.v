Require Import Coq.Lists.List.
Require Import Io.All.
Require Import Io.System.All.
Require Import ListString.All.

Import ListNotations.
Import C.Notations.
Require Import String.

(* Definition level (line:string) : nat := (fix count (str:string) : nat := *)
(*                                         match str with *)
(*                                           | EmptyString   => O *)
(*                                           | String "#" ss => S (count ss) *)
(*                                           | String _   _  => O *)
(*                                         end) ((fix drop (line':string) : string := *)
(*                                                  match line' with *)
(*                                                    | String " " s => drop s   *)
(*                                                    | String c   s => String c s *)
(*                                                    | EmptyString  => EmptyString *)
(*                                                  end) line). *)

Fixpoint dec (n : nat) (line : LString.t) : LString.t :=
  match (n, LString.to_string line) with
    | (O, str)    => LString.s str
    | (S n', str) =>
      dec n' (LString.s ((fix dec' line :=
                             match line with
                               | String " " s              => String " " (dec' s)
                               | String "#" (String "#" s) => String "#" s
                               | s                         => s
                             end) str))
  end.

Fixpoint inc (n:nat) (line : LString.t) : LString.t :=
  match (n, LString.to_string line) with
    | (O, str)    => LString.s str
    | (S n', str) =>
      inc n' (LString.s ((fix inc' st :=
                            match st with
                              | String " " s => String " " (inc' s)
                              | String "#" s => String "#" (String "#" s)
                              | s            => s
                            end) str))
  end.

Import Ascii.
Definition new_line : Ascii.ascii := "010".

Definition map_line f (code : LString.t) : LString.t :=
  (LString.join (LString.s (String new_line ""))) (map f (LString.split code new_line)).

Definition is_minus (num : LString.t) : bool :=
  match LString.to_string num with
    | String "-" _ => true
    | _            => false
  end.

Definition lstr2nat (num : LString.t) : option nat :=
  let abs_num := match LString.to_string num with
                  | String "-" n => LString.s n
                  | n            => LString.s n
                end in
  match (LString.to_N 10 abs_num) with
    | Some n => Some (BinNat.N.to_nat n)
    | None   => None
  end.           

Definition shift (argv : list LString.t) : C.t System.effect unit :=
  match argv with
  | [_; num; file_name] =>
    let! content := System.read_file file_name in
    match content with
    | None => System.log (LString.s "Cannot read the file.")
    | Some content =>
      let shifter := if is_minus num then dec else inc in
      match lstr2nat num with
        | Some n => System.log (map_line (shifter n) content)
        | None   => System.log (LString.s "Wrong number spec.")
      end
    end
  | _ => System.log (LString.s "Expected one parameter.")
  end.


Definition main := Extraction.launch shift.
Extraction "./extraction/main" main.
