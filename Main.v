Require Import Coq.Lists.List.
Require Import Io.All.
Require Import Io.System.All.
Require Import ListString.All.

Import ListNotations.
Import C.Notations.

Print LString.s.
(* LString.s = LString.of_string *)
(*      : String.string -> ListString.LString.t *)

(* Argument scope is [string_scope] *)

Locate LString.
(* Module ListString.All.LString *)
(* Module ListString.LString *)
(*   (shorter name to refer to it in current context is ListString.LString) *)


Require Import String.



(** Shift makedown's section level *)
Definition level (line:string) : nat := (fix count (str:string) : nat :=
                                        match str with
                                          | EmptyString   => O
                                          | String "#" ss => S (count ss)
                                          | String _   _  => O
                                        end) ((fix drop (line':string) : string :=
                                                 match line' with
                                                   | String " " s => drop s  
                                                   | String c   s => String c s
                                                   | EmptyString  => EmptyString
                                                 end) line).


Fixpoint dec (n:nat) (line:string) : string :=
  match (n, line) with
    | (O, str)    => str
    | (S n', str) => dec n' ((fix dec' line :=
                             match line with
                               | String " " s              => String " " (dec' s)
                               | String "#" (String "#" s) => String "#" s
                               | s                         => s
                             end) str)
  end.

(* Fixpoint inc (n:nat) (line:string) : string := *)
(*   match (n, line) with *)
(*     | (O, str)    => str *)
(*     | (S n', str) => inc n' ((fix inc' line := *)
(*                              match line with *)
(*                                | String " " s => String " " (inc' s) *)
(*                                | String "#" s => String "#" (String "#" s) *)
(*                                | s            => s *)
(*                              end) str) *)
(*   end. *)
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
Let nr : Ascii.ascii := "010".
Compute nr%string.

Definition map_line f (code : LString.t) : LString.t :=
  (LString.join (LString.s (String nr ""))) (map f (LString.split code nr)).
About map_line.




Compute inc 2 (LString.s " # #fdka").

Print String.

Let content0 :=  append (append "#aa" (String nr "")) " ##bb".
Compute content0.
 (*     = "#aa *)
 (* ##bb"%string *)
 (*     : string *)

Compute map_line (inc 1) (LString.s content0).

Let lis := LString.split (LString.s content0) nr.
About lis.  
(* lis : list ListString.LString.t *)

Let lis2 := map (inc 1) lis.
About lis2.
(* lis2 : list string *)

Let tmp := lis2.
About tmp.
(* tmp : list ListString.LString.t *)

Compute (LString.join (LString.s (String nr ""))) tmp. 

(* Check (LString.join (LString.s (String nr ""))) (map LString.s (map (inc 1) (map LString.to_string (LString.split (LString.s content0) nr)))). *)




Definition cat'' (argv : list LString.t) : C.t System.effect unit :=
  match argv with
  | [_; file_name] =>
    let! content := System.read_file file_name in
    match content with
    | None => System.log (LString.s "Cannot read the file.")
    | Some content => System.log (map_line (inc 1) content)
    end
  | _ => System.log (LString.s "Expected one parameter.")
  end.


Definition main := Extraction.launch cat''.
Extraction "./extraction/main" main.
