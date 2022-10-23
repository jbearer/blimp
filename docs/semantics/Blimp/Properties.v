Require Import Coq.Arith.Arith.
Require Import Coq.micromega.Lia.
Require Import RecordUpdate.RecordUpdate.
Require Import String.

Require Import Blimp.Nat.
Require Import Blimp.Notation.
Require Import Blimp.Semantics.
Import Semantics.Machine.

(** * properties of bl:mp semantics

In this module, we state and prove some basic properties of the semantics given
in [Blimp.Semantics]. This is not mean to be a complete exposition of the
behavior of bl:mp. We merely lay out some properties which are critical to the
well-formedness of the semantics, and some which lay a groundwork for more
interesting proofs elsewhere. *)

Section Finality.
(** ** finality

Many of the properties in this module deal in some way with _finality_ #&mdash;#
whether a bl:mp program has run to completion. In this section, we define
finality and prove some import properties of it. *)

(** **** [final]: a [Result] is [final] if it represents a complete execution *)
Inductive final {A} : Result A -> Prop :=
| final_ok : forall s a, final $ Ok s a
| final_err : forall s, final $ Err s
.

(** **** [final_lem]: finality is decidable *)
Lemma final_lem {A} (r : Result A) : final r \/ ~final r.
Proof.
(* begin details *)
destruct r.
+ left; constructor.
+ left; constructor.
+ right. intros H_final. inversion H_final.
(* end details *)
Qed.

(** **** [bind_final]: derive finality of the first statement in a sequence

This lemma is useful for proofs that step through a bl:mp program statement by
statement, depending on finality at each step. If the proof requires finality of
the whole program, [bind_final] can be used to derive finality of the first
statement and then operate on that statement. *)
Lemma bind_final {A B} (m : Machine A) (f : A -> Machine B)
                       (d : nat) (i : Input) :
  final (bind m f d i) -> final (m d i).
Proof.
(* begin details *)
unfold bind.
destruct (m d i); try constructor.
intros H_false.
exfalso.
now inversion H_false.
(* end details *)
Qed.

(** **** [bind_final_ok]: derive finality of the second statement in a sequence

This lemma complements [bind_final]. It states that the second statement in a
sequence must be final if the overall sequence is final and the first statement
succeeds. When stepping through a sequence of statements in a proof that
requires finality, this can be used to step into the second statement in a
sequence in the case where the sequence does not short-circuit after the first
statement. *)
Lemma bind_final_ok {A B} (m : Machine A) (f : A -> Machine B)
                          (d : nat) (i : Input) :
  final (bind m f d i) ->
    forall s a, m d i = Ok s a -> final (f a d $ i<|state := s|>).
Proof.
(* begin details *)
intros H_final s a H_ok.
unfold bind in H_final.
now rewrite H_ok in H_final.
(* end details *)
Qed.

(** **** [choice_final]: derive finality of the first statement in a choice

This lemma is useful for proofs that step through a bl:mp program statement by
statement, depending on finality at each step. It allows the proof to consider
both branches of a choice by cases. If the proof requires finality of the whole
program, [choice_final] can be used to derive finality of the first
statement in a choice and then operate on that statement. *)
Lemma choice_final {A} (l : Machine A) (r : Machine A) (d : nat) (i : Input) :
  final (choice l r d i) -> final (l d i).
Proof.
(* begin details *)
unfold choice.
destruct (l d i); try congruence.
constructor.
(* end details *)
Qed.

(** **** [choice_final_err]: derive finality of the second statement in a choice

This lemma complements [choice_final]. It states that the second statement in a
choice must be final if the overall choice is final and the first statement
fails. When analyzing a choice of statements by caess in a proof that requires
finality, this can be used to focus on the second statement in the case where
the choice does not short-circuit after the first statement. *)
Lemma choice_final_err {A} (l : Machine A) (r : Machine A)
                           (d : nat) (i : Input) :
  final (choice l r d i) ->
    forall s, l d i = Err s -> final (r d $ i<|state := s|>).
Proof.
(* begin details *)
intros H_final s H_err.
unfold choice in H_final.
now rewrite H_err in H_final.
(* end details *)
Qed.

End Finality.

(** **** [set_id] : replace [x<|f := f x|>] with [x] in assumptions and goals *)
Ltac set_id x f := 
  replace (x<|f := f x|>) with x by (vm_compute; now destruct x).

(** **** [fixM_succ] : unfold one application of [fixM]

If [fixM] is applied to a non-zero depth limit (a [nat] of the form [S d]) then
we can replace it with the body of the fixpoint, where recursive calls invoke
the fixpoint applied to the predecessor of the original depth limit ([d] if the
depth limit was originally [S d]).
*)
Lemma fixM_succ {A R} (f : (A -> Machine R) -> A -> Machine R)
                      (a : A) (d : nat) (i : Input) :
  fixM f a (S d) i = f (fun a' _ => fixM f a' d) a d i.
Proof.
(* begin details *)
now unfold fixM.
(* end details *)
Qed.

(** **** [result_state] : extract the state from a [Result] *)
Definition result_state {A} (r : Result A) : State := match r with
| Ok s _ => s
| Err s => s
| Diverge s => s
end.

(** **** [lookup_pure]: [lookup] does not change the machine state *)
Lemma lookup_pure (x : Symbol) (d : nat) (i : Input) :
  result_state (lookup x d i) = state i.
Proof.
(* begin details *)
revert i.
unfold lookup, result_state.
induction d; intros i.
{ now unfold fixM. }
rewrite fixM_succ.
unfold reads, gets, bind, with_state.
repeat set_id i state.
destruct contents.
{ now unfold ret. }
destruct parent.
* unfold with_ctx.
  replace (state i) with (state $ i<|ctx ::=put scope a|>) by now vm_compute.
  apply IHd.
* now unfold ret.
(* end details *)
Qed.

(** **** [branch]: automatic case analysis

A useful tactic for the proofs which follow, many of which feature heavy use of
case analysis.

[branch] searches the goal for a [match] expression, remembers the match head
into a fresh variable (in case it is a constructed term, which [destruct] would
throw away) and then [destruct]s the fresh variable, creating a new goal for
each case of the [match]. *)
Ltac branch := lazymatch goal with
| [ |- context g [match ?x with _ => _ end] ] =>
    let branch := fresh "branch" in
    remember x as branch; destruct branch
end.

Module DepthIrrelevance.
(** ** depth irrelevance

When defining semantics with an extra bookkeeping-only state (as we have done
with the depth limit for [Machine]) which is there only to ensure validity in
the meta-semantics (in this case termination) and which is not intended to deine
the behavior of bl:mp itself, it is critical to prove that the extra state is
limited to its bookkeeping, and does not in fact impact the observable behavior
of semantics.

In this case, what we need to prove is that, as long as a depth limit is
sufficient to drive a computation to completion, the result of the computation
does not depend on the depth limit. In other words, for any depth-limited
computation [f] and any sufficiently large depth limit [d] such that
[final $ f d], we have [forall d', d' >= d -> final f d' = final f d]. We call
this property _depth irrelevance_, and in this module we prove it for all of the
various operations in the bl:mp semantics.

*** tactics

All of the proofs follow a similar pattern, of iteratively destructing both
sides of the desired equality, proving that the first statements in the
resulting sequences are equivalent (possibly relying on a previously proved
depth irrelevance theorem or an inductive hypothesis), performing case analysis
on the results of the first statements and then focusing on the remaining
statements. To capture this pattern, it will be useful to develop a library of
tactics for symbolically executing bl:mp programs, before we give proofs of the
various depth irrelevance lemmas using these tactics. *)

(** **** [trivial_blimp]: solve "easy" bl:mp-related goals

Like the [trivial] tactic, [trivial_blimp] searches for a proof of the current
goal in an intelligent way, but only applies tactics that do not result in the
creation of new sub-goals. Unlike [trivial], [trivial_blimp] uses tactics that
are specifically designed to solve bl:mp-related goals. *)
Local Ltac trivial_blimp := easy || lazymatch goal with
(** This tactic solves the following goals: *)
(** - Finality of a statement that follows from finality of a compound
statement, for example finality of [m] from finality of [bind m f] or finality
of [b] from finality of [choice a b] and the fact that [a] fails. *)
| [ H_final : final (bind ?m ?fn ?d ?i) |- final (?m' ?d ?i) ] =>
    apply bind_final with (f := fn)
| [ H_final : final (bind ?m ?f ?d ?i)
  |- final (?f ?a ?d $ ?i<|state := ?s|>) ] =>
    apply bind_final_ok with (m := m)
| [ H_final : final (choice ?l ?r ?d ?i) |- final (?l' ?d ?i) ] =>
    apply choice_final with (r := r)
| [ H_final : final (choice ?l ?r ?d ?i)
  |- final (?r ?d $ ?i<|state := ?s|>) ] =>
    apply choice_final_err with (l := l)
(** - Depth irrelevance of programs that do not append on depth. For example,
[ret a d' i = ret a d i] can be proven by a simple [unfold]. *) 
| [ |- new ?x ?e ?d' ?i = new ?x ?e ?d ?i ] => unfold new; reflexivity
| [ |- ret ?a ?d' ?i = ret ?a ?d ?i ] => unfold ret; reflexivity
| [ |- fail ?d' ?i = fail ?d ?i ] => unfold fail; reflexivity
| [ |- reads ?x ?d' ?i = reads ?x ?d' ?i ] => unfold reads; reflexivity
| [ |- gets ?x ?d' ?i = gets ?x ?d' ?i ] => unfold gets; reflexivity
(** - Inequalities on natural numbers. These are typical preconditions of depth
irrelevance lemmas or inductive hypotheses. The proofs are usually very simple
(for instance, symmetry or unfolding ">=". However, since it is fairly low-cost,
we use the full power of [lia] in general. *)
| [ |- ?m >= ?n ] => lia
end.

(** **** [split_bind]: split depth irrelevance of a [bind] into sub-goals 

This lemma splits depth irrelevance of a [bind] into depth irrelevance of each
statement. It is used by the [split_blimp] tactic when the goal is a [bind]. *)
Local Lemma split_bind {A B} (m : Machine A) (m' : Machine A)
                             (f : A -> Machine B) (f' : A -> Machine B)
                             (d : nat) (d' : nat) (i : Input) :
  final (bind m f d i) ->
  (final (m d i) -> m d i = m' d' i) ->
  (forall s a, m d i = Ok s a -> final (f a d $ i<|state := s|>) ->
    f a d (i<|state := s|>) = f' a d' (i<|state := s|>)) ->
  bind m f d i = bind m' f' d' i.
Proof.
(* begin details *)
intros H_final H_eq_head H_eq_tail.
unfold bind.
rewrite H_eq_head; [|now trivial_blimp].
destruct (m' d' i) as [s a| |]; try easy.
apply H_eq_tail; assert (m d i = Ok s a) by (apply H_eq_head; now trivial_blimp).
+ easy.
+ now trivial_blimp.
(* end details *)
Qed.

(** **** [split_choice]: split depth irrelevance of a [choice] into sub-goals

This lemma splits depth irrelevance of a [choice] into depth irrelevance of each
statement. It is used by the [split_blimp] tactic when the goal is a [choice]. *)
Local Lemma split_choice {A} (l : Machine A) (l' : Machine A)
                             (r : Machine A) (r' : Machine A)
                             (d : nat) (d' : nat) (i : Input) :
  final (choice l r d i) ->
  (final (l d i) -> l d i = l' d' i) ->
  (forall s, l d i = Err s -> final (r d $ i<|state := s|>) ->
    r d (i<|state := s|>) = r' d' (i<|state := s|>)) ->
  choice l r d i = choice l' r' d' i.
Proof.
(* begin details *)
intros H_final H_eq_l H_eq_r.
unfold choice.
rewrite H_eq_l; [|now trivial_blimp].
destruct (l' d' i) as [|s|]; try easy.
assert (l d i = Err s) by (apply H_eq_l; now trivial_blimp).
apply H_eq_r.
+ assumption.
+ now trivial_blimp.
(* end details *)
Qed.

(** a database of depth irrelevance lemmas for each bl:mp operation *)
Create HintDb depth_irrelevance_hints.
(** transparency information for bl:mp functions *)
Create HintDb blimp_unfold.

(** **** [split_blimp]: split a depth irrelevance goal into sub-goals *)
Local Ltac split_blimp := match goal with
| [ H_final : final (bind ?m' ?f' ?d' ?i)
  |- bind ?m ?f ?d ?i = bind ?m' ?f' ?d' ?i ] =>
    symmetry;
    apply split_bind;
    [|intros ?H_final; symmetry| intros ?s ?a ?H_ok ?H_final; symmetry]
| [ H_final : final (choice ?l' ?r' ?d' ?i)
  |- choice ?l ?r ?d ?i = choice ?l' ?r' ?d' ?i ] =>
    symmetry;
    apply split_choice;
    [|intros ?H_final; symmetry| intros ?s ?H_err ?H_final; symmetry]
| [ |- with_ctx ?f ?m' ?d' ?i = with_ctx ?f ?m ?d ?i ] => unfold with_ctx
| [ |- ?f' ?d' ?i = ?f ?d ?i ] =>
    (** For goals that look like depth irrelevance goals but don't match a
    specific pattern, see if we can invoke an already-proven depth irrelevance
    lemma. First we invoke [lia] to prove the "sufficient depth" precondition of
    all depth irrelevance lemmas. *)
    assert (d' >= d) by lia;
    (** Then we use [auto] to search our hintbase of lemmas. *)
    solve [auto with depth_irrelevance_hints blimp_unfold]
end.

(** **** [auto_blimp]: discharge [trivial_blimp] goals as long as we can, then
split into sub-goals and repeat. *)
Local Ltac auto_blimp := repeat first [trivial_blimp|split_blimp].
(** **** [crush_blimp]: like [auto_blimp], but with automatic case analysis

[crush_blimp] performs the same automated split-and-prove search procedure as
[auto_blimp], except that when it gets stuck on a goal which requires case
analysis (that is, a [match] goal where each branch is a depth irrelevance goal)
it automatically destructs the head of the [match] and recurses into each new
sub-goal. *)
Local Ltac crush_blimp := repeat first [trivial_blimp|split_blimp|branch].

(** *** depth irrelevance lemmas

With those tactics defined, we can begin proving depth irrelevance for each
bl:mp operation. Each lemma we prove will be added to the
[depth_irrelevance_hints] hintbase, and will thus be available to [auto_blimp]
and [crush_blimp] in the proofs of subsequent lemmas. *)

(** **** [lookup_depth_irrelevance] *)
Lemma lookup_depth_irrelevance (x : Symbol) (d : nat) (i : Input) :
  final (lookup x d i) ->
  forall d', d' >= d -> lookup x d' i = lookup x d i.
Proof.
(* begin details *)
revert i.
unfold lookup.
induction d; intros i H_final d' H_ge.
{ exfalso. inversion H_final. }
inversion H_ge as [| p H_le H_eq]; [congruence|];
rewrite (fixM_succ _ _ d _);
rewrite (fixM_succ _ _ p _);
rewrite (fixM_succ _ _ _ _) in H_final.
crush_blimp.
(* end details *)
Qed.
#[export] Hint Resolve lookup_depth_irrelevance : depth_irrelevance_hints.

(** **** [eval_ex_eq]: extensional depth irrelevance for two versions of [eval]

Recall that operations which are mutually recursive with [eval], such as [send]
and [parse], are defined in terms of helpers parameterized on the function they
should use for [eval], such as [send'] and [parse']. These are instantiated in
the definition of [eval] using the recursive fixpoint argument from [fixM].

[fixM] is defined in a slightly odd way in order to ensure termination. Namely,
the depth argument to [fixM] is passed directly to the recursive fixpoint
argument, which ignores the depth limit provided to it by the monadic constant.
In short, this means that when [eval] recurses, it invokes
[(fun (_ : nat) e => eval e d) e] and not [eval e]. This means that when a
function like [eval] or [send'] is applied to different depth limits, when it
recurses, it not only propagates the different depth limits to the recursive
calls, it also recurses into _different functions_, since the different depth
limits are baked into the recursive calls. These different functions have depth
irrelevance as long as [eval] does, of course. It is useful to have an
abbreviation for this fact, since it will be a requirement of the [eval]
parameter for [send'], [parse'], etc. when we prove depth irrelevance for those
functions. *)
Definition eval_ex_eq (eval : Expr -> Machine Value)
                      (eval' : Expr -> Machine Value) :=
  forall e d d' i, final (eval e d i) -> d' >= d -> eval' e d' i = eval e d i.
#[export] Hint Unfold eval_ex_eq : blimp_unfold.

(** **** [eval_depth_irrelevance_ex_eq]: well-formedness of [eval_ex_eq]

To match intuition, [eval_ex_eq] should hold for any function [eval] applied to
two different depth limits as long as that function has depth irrelevance
itself. *)
Local Lemma eval_depth_irrelevance_ex_eq (eval : Expr -> Machine Value) :
  (forall e d i, final (eval e d i) ->
  forall d', d' >= d -> eval e d' i = eval e d i) ->
  eval_ex_eq eval eval.
Proof.
(* begin details *)
intros H_depth_irrelevance.
unfold eval_ex_eq.
intros e d d' i H_final H_ge.
now apply H_depth_irrelevance.
(* end details *)
Qed.

(** **** [send'_depth_irrelevance] *)
Lemma send'_depth_irrelevance (eval : Expr -> Machine Value) (rcv : Value)
                              (msg : Value) (d : nat) (i : Input) :
  (final $ send' eval rcv msg d i) ->
  forall eval' d', eval_ex_eq eval eval' -> d' >= d ->
    send' eval' rcv msg d' i = send' eval rcv msg d i.
Proof.
(* begin details *)
revert rcv msg i.
unfold send'.
induction d; intros rcv msg i H_final eval' d' H_eval_ex_eq H_ge.
- exfalso. inversion H_final.
- apply S_n_le_m_pred in H_ge as [p [H_eq_d'_S_p H_le]].
  rewrite H_eq_d'_S_p in *.
  rewrite (fixM_succ _ (rcv, msg) d i).
  rewrite (fixM_succ _ (rcv, msg) p i).
  rewrite (fixM_succ _ _ _ _) in H_final.
  crush_blimp.
(* end details *)
Qed.
#[export] Hint Resolve send'_depth_irrelevance : depth_irrelevance_hints.

(** **** [parse'_depth_irrelevance] *)
Lemma parse'_depth_irrelevance (eval : Expr -> Machine Value) (stream : Value)
                               (d : nat) (i : Input):
  final (parse' eval stream d i) ->
  forall eval' d', eval_ex_eq eval eval' -> d' >= d ->
    parse' eval' stream d' i = parse' eval stream d i.
Proof.
(* begin details *)
intros H_final eval' d' H_eval_ex_eq H_ge.
unfold parse' in *.
auto_blimp.
(* end details *)
Qed.
#[export] Hint Resolve parse'_depth_irrelevance : depth_irrelevance_hints.

(** **** [parse_symbol'_depth_irrelevance] *)
Lemma parse_symbol'_depth_irrelevance (eval : Expr -> Machine Value)
                                      (delim : Symbol) (stream : Value)
                                      (d : nat) (i : Input):
  final (parse_symbol' eval delim stream d i) ->
  forall eval' d', eval_ex_eq eval eval' -> d' >= d ->
    parse_symbol' eval' delim stream d' i = parse_symbol' eval delim stream d i.
Proof.
(* begin details *)
revert i stream.
unfold parse_symbol'.
induction d; intros i stream H_final eval' d' H_eval_ex_eq H_ge.
{ exfalso; inversion H_final. }
apply S_n_le_m_pred in H_ge as [p [H_eq_d'_S_p H_le]].
rewrite H_eq_d'_S_p in *.
rewrite (fixM_succ _ _ d i).
rewrite (fixM_succ _ _ p i).
rewrite (fixM_succ _ _ _ _) in H_final.
crush_blimp.
(* end details *)
Qed.
#[export] Hint Resolve parse_symbol'_depth_irrelevance : depth_irrelevance_hints.

(** **** [parse_sub_expr'_depth_irrelevance] *)
Lemma parse_sub_expr'_depth_irrelevance (eval : Expr -> Machine Value)
                                        (stream : Value) (recurse : Value)
                                        (d : nat) (i : Input) :
  final (parse_sub_expr' eval stream recurse d i) ->
  forall eval' d', eval_ex_eq eval eval' -> d' >= d ->
    parse_sub_expr' eval' stream recurse d' i =
    parse_sub_expr' eval stream recurse d i.
Proof.
(* begin details *)
revert i.
unfold parse_sub_expr'.
induction d; intros i H_final eval' d' H_eval_ex_eq H_ge.
{ exfalso; inversion H_final. }
apply S_n_le_m_pred in H_ge as [p [H_eq_d'_S_p H_le]].
rewrite H_eq_d'_S_p in *.
crush_blimp.
(* end details *)
Qed.
#[export] Hint Resolve parse_sub_expr'_depth_irrelevance : depth_irrelevance_hints.

Lemma parse_ret'_depth_irrelevance (eval : Expr -> Machine Value)
                                   (e : Expr) (stream : Value)
                                   (d : nat) (i : Input) :
  final (parse_ret' eval e stream d i) ->
  forall eval' d', eval_ex_eq eval eval' -> d' >= d ->
    parse_ret' eval' e stream d' i = parse_ret' eval e stream d i.
Proof.
(* begin details *)
unfold parse_ret'.
intros H_final eval' d' H_eval_ex_eq H_ge.
auto_blimp.
Qed.
#[export] Hint Resolve parse_ret'_depth_irrelevance : depth_irrelevance_hints.

(** **** [eval_depth_irrelevance] *)
Lemma eval_depth_irrelevance (e : Expr) (d : nat) (i : Input) :
  final (eval e d i) -> forall d', d' >= d -> eval e d' i = eval e d i.
Proof.
(* begin details *)
revert e i.
unfold eval.
induction d using strong_induction; intros e i H_final d' H_ge.
{ exfalso. inversion H_final. }
inversion H_ge as [| p H_le H_eq]; try congruence.
(** Collapse the huge inlined recursive call into an opaque variable to keep
proof terms smaller *)
remember (fun eval arg => _) as eval_fix eqn:H_eq_eval_fix.
rewrite (fixM_succ _ _ d _).
rewrite (fixM_succ _ _ p _).
rewrite (fixM_succ _ _ _ _) in H_final.
(** Prove [eval_ex_eq] for our fixpoint argument. This will help us with all of
the recursive calls. *)
assert (H_eval_ex_eq : eval_ex_eq
  (fun a' (_ : nat) => fixM eval_fix a' d)
  (fun a' (_ : nat) => fixM eval_fix a' p)
). {
  unfold eval_ex_eq.
  intros e0 d0 d'0 i0 H_final0 H_ge0.
  apply H; [reflexivity|exact H_final0|now apply le_Sn_le].
}
(** Rewrite the top-level occurrences of [eval] with their definitions.

Rewriting only the top-level occurrences keeps the relevant terms small and
helps the subsequent case analysis (on over 256 cases) proceed much faster.

To work around the fact that [rewrite] at occurrences uses setoid rewriting
instead of Leibniz rewriting, and thus imposes a number of additional
obligations, we first [remember] only those occurrences which we want to
rewrite, giving them a different name. We then rewrite _all_ occurrences of the
new name using Leibniz equality.
*)
remember eval_fix as rewrite_eval_fix in H_eq_eval_fix, H_final at 1 |- * at 1 3.
rewrite H_eq_eval_fix in H_final |- *.
(** Solve the remaining goal by evaluation.

The proof which is generated by the [crush_blimp] tactic here is conceptually
very simple. All it does is evaluate the two bl:mp programs on either side of
the equality statement by statement until it has proven them equal, performing
case analysis where necessary. However, it is also quite a large proof, since
there are many cases to analyze in the definition of [eval] -- over 200, since
the pattern matching on symbols in the parsing logic gets exploded to pattern
matching on sequences of characters. This tactic takes about 30 seconds to
produce a proof on a reasonably powerful desktop. *)
crush_blimp.
(* end details *)
Qed.
#[export] Hint Resolve eval_depth_irrelevance : depth_irrelevance_hints.

(** **** [eval_ex_eq_fixM_rec]: The version of [eval] used for recursive calls
in ['] [fixM] functions (e.g. [send']) satisfies [eval_ex_eq]. *)
Corollary eval_ex_eq_fixM_rec (d : nat) (d' : nat) :
  d' >= d -> (eval_ex_eq (fun e _ => eval e d) (fun e _ => eval e d')).
unfold eval_ex_eq.
intros H_ge e ? ? i H_final ?.
now apply eval_depth_irrelevance.
Qed.

(** *** depth irrelevance corollaries

As a simple consequence of the lemmas above, we have depth irrelevance for all
of the parameterized bl:mp operations instantiated with [eval], (e.g. [send] is
[send'] instantiated with [eval], both of which have depth irrelevance, so
[send] also has depth irrelevance). The proofs of all of these corollaries have
the same shape, summarized by this tactic: *)
Local Ltac wrapper_depth_irrelevance wrapper di_lemma :=
  unfold wrapper;
  intros;
  apply di_lemma; try easy;
  apply eval_depth_irrelevance_ex_eq;
  exact eval_depth_irrelevance
.

Corollary send_depth_irrelevance (rcv : Value) (msg : Value)
                                 (d : nat) (i : Input) :
  final (send rcv msg d i) -> forall d', d' >= d ->
    send rcv msg d' i = send rcv msg d i.
Proof.
wrapper_depth_irrelevance send send'_depth_irrelevance.
Qed.
#[export] Hint Resolve send_depth_irrelevance : depth_irrelevance_hints.

Corollary parse_depth_irrelevance (stream : Value) (d : nat) (i : Input) :
  final (parse stream d i) -> forall d', d' >= d ->
    parse stream d' i = parse stream d i.
Proof.
wrapper_depth_irrelevance parse parse'_depth_irrelevance.
Qed.
#[export] Hint Resolve parse_depth_irrelevance : depth_irrelevance_hints.

Corollary parse_symbol_depth_irrelevance (delim : Symbol) (stream : Value)
                                         (d : nat) (i : Input):
  final (parse_symbol delim stream d i) ->
  forall d', d' >= d ->
    parse_symbol delim stream d' i = parse_symbol delim stream d i.
Proof.
wrapper_depth_irrelevance parse_symbol parse_symbol'_depth_irrelevance.
Qed.
#[export] Hint Resolve parse_symbol_depth_irrelevance : depth_irrelevance_hints.

(** **** [depth_irrelevance] for top-level bl:mp execution *)
Theorem depth_irrelevance (p : string) (d : nat) :
  final (run_blimp p d) -> forall d', d' >= d -> run_blimp p d' = run_blimp p d.
Proof.
(* begin details *)
unfold run_blimp.
intros H_final d' H_ge.
auto_blimp.
(* end details *)
Qed.
#[export] Hint Resolve depth_irrelevance : depth_irrelevance_hints.

(** [depth_irrelevance_inv']: inversion on depth irrelevance

It follows from depth irrelevance, as stated above, that two computations with
different depth limits are either equivalent, or at least one diverges. *)
Corollary depth_irrelevance_inv' {A} (f : (Expr -> Machine Value) -> Machine A)
                                     (eval : Expr -> Machine Value) (d : nat)
                                     (eval' : Expr -> Machine Value) (d' : nat)
                                     (i : Input)
  (di_lemma : forall eval d i, final (f eval d i) ->
    forall eval' d', eval_ex_eq eval eval' -> d' >= d ->
      f eval' d' i = f eval d i) :
  (d' >= d -> eval_ex_eq eval eval') ->
  (d >= d' -> eval_ex_eq eval' eval) ->
  final (f eval d i) -> final (f eval' d' i) -> f eval d i = f eval' d' i.
Proof.
(* begin details *)
intros H_eval_ex_eq H_eval_ex_eq' H_final H_final'.
destruct (le_ge_dec d d'); [symmetry|]; apply di_lemma; try easy.
- now apply H_eval_ex_eq.
- now apply H_eval_ex_eq'.
(* end details *)
Qed.

Corollary depth_irrelevance_inv {A} (m : Machine A) (d : nat)
                                    (d' : nat) (i : Input)
  (di_lemma : forall d i, final (m d i) -> forall d', d' >= d ->
    m d' i = m d i) :
  final (m d i) -> final (m d' i) -> m d i = m d' i.
Proof.
(* begin details *)
apply depth_irrelevance_inv' with
  (f := fun _ => m) (eval := eval) (eval' := eval).
- intros. now apply di_lemma.
- intros. apply eval_depth_irrelevance_ex_eq.
  exact eval_depth_irrelevance.
- intros. apply eval_depth_irrelevance_ex_eq.
  exact eval_depth_irrelevance.
(* end details *)
Qed.

End DepthIrrelevance.
Import DepthIrrelevance.

Module Termination.
(** ** definitions for discussion terminating bl:mp programs *)

(** **** [returns]: [returns p v] means [p] terminates and produces the value
[v] *)
Inductive returns (p : string) (v : Value) : Prop :=
| returns_ok : forall d s, run_blimp p d = Ok s v -> returns p v
.
Notation "p ~!> v" := (returns p v) (at level 70).

(** **** [returns_precise]: for any program [p], [v] such that [p ~!> v] is
unique *)
Lemma returns_precise (p : string) : forall v v', p ~!> v -> p ~!> v' -> v = v'.
Proof.
(* begin details *)
intros v v' H H'.
destruct H.
destruct H'.
assert (run_blimp p d = run_blimp p d0). {
  elim Compare_dec.le_ge_dec with (n := d) (m := d0); intros H_cmp.
  + symmetry.
    apply depth_irrelevance; [|easy].
    rewrite H in *.
    constructor.
  + apply depth_irrelevance; [|easy].
    rewrite H0 in *.
    constructor.
}
congruence.
(* end details *)
Qed.

(** **** [parses]: [parses p e] means [p] terminates and produces the parse tree
[e] *)
Inductive parses (p : string) (e : Expr) : Prop :=
| parses_block : forall s v d d' s' a x,
    run_blimp p d = Ok s v ->
    eval (A $ EValue v) d' (initial_input<|state := s|>) = Ok s' (VBlock a x e) ->
    parses p e
.
Notation "p '~>' e" := (parses p e) (at level 70).

(** **** [succeeds]: [succeeds p] means [p] terminates and produces a value *)
Inductive succeeds (p : string) : Prop :=
| succeeds_at_depth : forall v, returns p v -> succeeds p
.

(** **** [fails]: [fails p] means [p] terminates but fails to produce a value *)
Inductive fails (p : string) : Prop :=
| fails_at_depth : forall d s, run_blimp p d = Err s -> fails p
.

(** **** [terminates]: [terminates p] if and only if [p] terminates *)
Inductive terminates (p : string) : Prop :=
| terminates_ok : succeeds p -> terminates p
| terminates_err : fails p -> terminates p
.

End Termination.
Import Termination.

Module Divergence.
(** ** useful properties for proving non-termination of bl:mp programs *)

(** **** [diverges]: [diverges p] in and only if [p] does not terminate *)
Definition diverges (p : string) := ~terminates p.

(** **** [not_final_eq_diverge]: if [r] is not [final], it has the form
[Diverge s]. *)
Lemma not_final_eq_diverge {A} (r : Result A) :
  ~final r -> exists s, r = Diverge s.
Proof.
(* begin details *)
intro H_not_final.
destruct r as [| |s].
+ exfalso. apply H_not_final. constructor.
+ exfalso. apply H_not_final. constructor.
+ now exists s.
(* end details *)
Qed.

(** **** [diverges_at_any_depth]: [diverges p] means [p] returns [Diverge _] no
matter the depth limit. *)
Lemma diverges_at_any_depth (p : string) :
  diverges p <-> forall d, exists s, run_blimp p d = Diverge s.
Proof.
(* begin details *)
split.
* intros H_div d.
  unfold diverges in H_div.
  unfold not in H_div.
  remember (run_blimp p d) as run.
  destruct run as [s a|s|s].
  - exfalso.
    apply H_div.
    constructor.
    apply succeeds_at_depth with (v := a).
    now apply returns_ok with (d := d) (s := s).
  - exfalso.
    apply H_div.
    constructor 2.
    now apply (fails_at_depth p d s).
  - now exists s.
* intros H_eq.
  unfold diverges.
  intros H_term.
  destruct H_term as [H_succeeds|H_fails].
  - destruct H_succeeds as [v H].
    destruct H as [d s H_ok].
    destruct (H_eq d) as [s' H_div].
    congruence.
  - destruct H_fails as [d s H_err].
    destruct (H_eq d) as [s' H_div].
    congruence.
(* end details *)
Qed.

(** **** [direct_loop_diverges']: if [x] points to itself in the current scope,
sending to [x] diverges. *)
Lemma direct_loop_diverges' eval (x : Symbol) (d : nat) (i : Input) :
  (exists d s, lookup x d i = Ok s (Some $ VSymbol x)) ->
  forall msg, send' eval (VSymbol x) msg d i = Diverge (state i).
Proof.
(* begin details *)
unfold send'.
intros H_ok_witness msg.
induction d; [now unfold fixM|].
rewrite fixM_succ.
unfold bind at 1.
destruct (final_lem $ lookup x d i).
- inversion H_ok_witness as [d_lookup H_ok_witness'].
  inversion H_ok_witness' as [s_lookup H_ok].
  rewrite depth_irrelevance_inv with (d' := d) in H_ok; [
  | apply lookup_depth_irrelevance
  | rewrite H_ok; constructor
  | assumption
  ].
  rewrite H_ok.
  replace s_lookup with (state i) by (
    rewrite <- (lookup_pure x d i); rewrite H_ok; now unfold result_state
  ).
  set_id i state.
  apply IHd.
- remember (lookup x d i) as r_lookup.
  destruct r_lookup as [| |s].
  + exfalso. apply H. constructor.
  + exfalso. apply H. constructor.
  + now replace s with (state i) by (
      rewrite <- (lookup_pure x d i);
      rewrite <- Heqr_lookup;
      now unfold result_state
    ).
(* end details *)
Qed.

(** **** [direct_loop_diverges] *)
Corollary direct_loop_diverges (x : Symbol) (d : nat) (i : Input) :
  (exists d s, lookup x d i = Ok s (Some $ VSymbol x)) ->
  forall msg, send (VSymbol x) msg d i = Diverge (state i).
Proof.
(* begin details *)
unfold send.
apply direct_loop_diverges'.
(* end details *)
Qed.

End Divergence.   
