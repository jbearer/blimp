(** Some facts about [nat] not proven in the standard library, but widely used
in other proofs about bl:mp semantics. *)
Require Import Coq.Arith.Arith.

(** **** [strong_induction]

[strong_induction] is an induction principle for [nat] where the inductive
hypothesis is strengthened for free #&mdash;# instead of [P n] (where the goal
in the inductive step is [P (S n)]), we get to assume [forall k, k <= n -> P k].
*)
Lemma strong_induction (P : nat -> Prop) :
  P 0 -> (forall n, (forall k, k <= n -> P k) -> P (S n)) -> forall n, P n.
(* begin details *)
intros H_0 H_S n.
assert (H_le : n <= n) by constructor.
generalize H_le.
clear H_le.
generalize n at 1 3.
induction n; intros k H_le.
- apply le_n_0_eq in H_le as H_eq0.
  now rewrite H_eq0 in *.
- destruct k; [exact H_0|].
  apply H_S.
  intros k' H_le'.
  apply IHn.
  apply le_S_n in H_le.
  now transitivity k.
(* end details *)
Qed.

(** **** [S_n_le_m_pred]: get a predcessor when lower bounded by a successor

If [S n <= m], then [m] has a predecessor [p] and [n <= p].
*)
Lemma S_n_le_m_pred (n : nat) (m : nat) :
  S n <= m -> exists p, m = S p /\ n <= p.
Proof.
(* begin details *)
intros H_S_le.
inversion H_S_le as [|p].
+ now exists n.
+ exists p.
  split; [reflexivity|].
  now apply le_Sn_le.
(* end details *)
Qed.
