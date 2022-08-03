(** * the formal semantics of bl:mp

This Coq library formally defines the semantics of the bl:mp programming language and lays out some
basic properties of these semantics.

The library is divided into three parts:

Definition of the semantics: *)
Require Import Blimp.Semantics.
(** Statement and proof of basic properties: *)
Require Import Blimp.Properties.
(** Verified evaluations of some example programs: *)
Require Import Blimp.Examples.
