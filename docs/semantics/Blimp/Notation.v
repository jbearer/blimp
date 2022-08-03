Require Import Coq.Init.Nat.
Require Import Coq.Strings.String.

(** We borrow a notation from Haskell for low-precedence function application. *)
Notation "A $ B" := (A B) (right associativity, at level 99).

(** Throughout this library, it will be useful to discuss boolean equality
generically, defining notions that hold for equality on both strings and natural
numbers. *)
Class EqDec (A : Type) := { eqb : A -> A -> bool }.
Notation "A == B" := (eqb A B) (at level 42).
#[export] Instance nat_EqDec : EqDec nat := {| eqb := Nat.eqb |}.
#[export] Instance string_EqDec : EqDec string := {| eqb := String.eqb |}.
