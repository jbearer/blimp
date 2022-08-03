Require Import Coq.micromega.Lia.
Require Import Coq.Strings.Ascii.
Require Import RecordUpdate.RecordUpdate.
Require Import String.

Require Import Blimp.Semantics.
Require Import Blimp.Notation.
Require Import Blimp.Properties.
Import Semantics.Machine.
Import Properties.DepthIrrelevance.
Import Properties.Divergence.
Import Properties.Termination.

(** * example bl:mp programs, with verified evaluations *)

(** ** tactics for verifying bl:mp programs *)

(** **** [if_final]: yields [x] if [r] is [final]; otherwise fails *)
Ltac if_final r x := match r with
| Ok _ _ => x
| Err _ => x
| _ => fail
end.

(** **** [instantiate_depth]: non-deterministically choose a depth limit

This tactic applies to goals about the evaluation of a bl:mp program where the
depth limit is an existential. It first unifies the depth limit with [hint] and
succeeds. If there is a subsequent failure (presumably due to an insufficient
depth limit) and evaluation backtracks into this tactic, it will retry with a
larger [hint].

This tactic succeeds infinitely many times with successively larger depth
limits, so the user must take care only to invoke it when they are certain there
is some finite depth limit which will make the subsequent proof succeed. *)
Ltac instantiate_depth hint :=
  let d := match goal with
  | [ |- run_blimp _ ?d = ?r ] => if_final r d
  | [ |- eval _ ?d _ = ?r ] => if_final r d
  | [ |- parse _ ?d _ = ?r ] => if_final r d
  end in
  tryif is_evar d then unify d hint + instantiate_depth (hint * 2) else idtac
.

(** **** [eval_blimp]: prove fully instantiated goals about bl:mp programs

If the goal has the form [eval_func d = final_result] (possibly under
existential quantifiers) where
- [eval_func] is one of [run_blimp p], [eval e], or [parse v]
- [final_result] is [Ok s a] or [Err s]
this tactic will attempt to evaluate [eval_func d] to completion, replacing it
with its result and thus hopefully proving the goal by reflexivity. If [d] is
existentially quantified, the tactic will iterate with successively greater
values of [d] until it finds one for which the evaluation terminates. _This
means the tactic may not terminate if [eval_func] does not terminate_. It may be
useful to use this tactic with [timeout] during debugging. *)
Ltac eval_blimp := repeat eexists; repeat match goal with
| [ |- parses ?p ?e ] => constructor
| _ => instantiate_depth 128; vm_compute; try congruence; try reflexivity
end.

(** **** [remove_whitespace] : a simple bl:mp preprocessor

[remove_whitespace] is a simple preprocessor that transforms a program by
removing all whitespace and comments (defined as any substrings starting with
[#] and ending with a newline).

This is intended to make it easier to write example programs in this document.
It is not part of the semantics proper #&mdash;# whitespace and comments have no
meaning in pure bl:mp unless explicitly defined using macros.
*)
Close Scope string_scope.
Open Scope char_scope.
Fixpoint remove_whitespace (s : string) : string := match s with
| String " " cs => remove_whitespace cs
| String "
" cs => remove_whitespace cs
| String "#" cs => next_line cs
| String c cs => String c $ remove_whitespace cs
| EmptyString => EmptyString
end
with next_line (s : string) : string := match s with
| String "
" cs => remove_whitespace cs
| String _ cs => next_line cs
| EmptyString => EmptyString
end.
Close Scope char_scope.
Open Scope string_scope.

(** ** example evaluations *)

(** *** parsing examples *)

Lemma parse_symbol : "`x`" ~> ESymbol "x".
eval_blimp.
Qed.

Lemma parse_empty_symbol : "``" ~> ESymbol "".
eval_blimp.
Qed.

Lemma parse_block : "\x.`x`" ~> EBlock "x" (ESymbol "x").
eval_blimp.
Qed.

Lemma parse_send : "<\x.`x``y`" ~>
  ESend (EBlock "x" $ ESymbol "x") (ESymbol "y").
eval_blimp.
Qed.

Lemma parse_trivial_macro : ">\g.\s.\p.\x.\x.`z`" ~> ESymbol "z".
eval_blimp.
Qed.

Lemma parse_id_macro : remove_whitespace "
> \x.`x` <\x.`x``y`
" ~> ESend (EBlock "x" $ ESymbol "x") (ESymbol "y").
eval_blimp.
Qed.

Lemma parse_lex_macro : remove_whitespace "
> \g.\s.
    <`g` \_.
     <<\A.\B.`B` <`s` `s`
                 <`s` `s`
  .<.`.x.`.`.y.`
" ~> ESend (ESymbol "x") (ESymbol "y").
eval_blimp.
Qed.

Lemma parse_grammar_macro : remove_whitespace "
> \g.\s.\p.
  <<\A.\B.`B` <`s` `s`
              <<`g` `s` `p`
  .\x..`x`
" ~> EBlock "x" (ESymbol "x").
eval_blimp.
Qed.

(** *** execution examples *)

(** **** [bang]: a prelude for executing bl:mp programs

The semantics of executing a bl:mp program merely dictate that the program be
parsed, not executed (all bl:mp expressions are _tethered_ until explicitly
_untethered_). [bang] is a prelude (a snippet of bl:mp code which can be
prepended to a complete bl:mp program) which has the effect of parsing _and_
executing the program appended to it.
*)
Definition bang (p : string) := remove_whitespace $ "
# Define an overlay which evaluates the top-level parse tree after parsing.
> \g.\s.\p.\x.
  <
    # Parse using the fixpoint of `g`, the original grammar. This ensures that
    # sub-expressions will be parsed using `g` as well, not this overlay, and
    # therefore sub-expressions will not be eagerly evaluated.
    < < \g. <
          \x. < `g` \v. < < `x` `x` `v`
          \x. < `g` \v. < < `x` `x` `v`
        < `g` `s`
      `x`
    # Send to the resulting parse tree, which evaluates the top-level parse tree.
    `_`
" ++ p.
Notation "! p" := (bang p) (at level 60).

Lemma eval_symbol : !"`x`" ~!> VSymbol "x".
eval_blimp.
Qed.

Lemma eval_block : exists a, !"\x.`x`" ~!> VBlock a "x" (ESymbol "x").
eval_blimp.
Qed.

Lemma eval_send : !"< \x.`x` `y`" ~!> VSymbol "y".
eval_blimp.
Qed.

Lemma define_and_use_symbol : !"
<<\A.\B.`B` <`foo` \ref.<`ref` \a.`a`
            <`foo` `bar`
" ~!> VSymbol "bar".
eval_blimp.
Qed.

Lemma get_symbol_nested : !"
<<\A.\B.`B` <`foo` \ref.<`ref` \a.`a`
            < \_. <`foo` `bar` `.`
" ~!> VSymbol "bar".
eval_blimp.
Qed.

Lemma set_symbol : !"
<<\A.\B.`B` <`foo` \ref.<`ref` `ref`
<<\A.\B.`B` <`foo` \_.`bar`
            <`foo` `.`
" ~!> VSymbol "bar".
eval_blimp.
Qed.

Lemma set_symbol_nested : !"
<<\A.\B.`B` <`foo` \ref.<`ref` `ref`
<<\A.\B.`B` < \_. <`foo` \_.`bar` `.`
            <`foo` `.`
" ~!> VSymbol "bar".
eval_blimp.
Qed.

Lemma set_symbol_from_unrelated_scope : !"
<<\A.\B.`B` <`scope` \scoperef.
              <`scoperef` \msg.<`foo` `msg`
<<\A.\B.`B` <`scope` \fooref. <`fooref` `fooref`
<<\A.\B.`B` <`scope` \x.`x`
            <`scope` `bar`
" ~!> VSymbol "bar".
eval_blimp.
Qed.

Lemma shadowing : !"
<<\A.\B.`B` <`parent-ref` \^.<`^` `^`
<<\A.\B.`B` <`child-ref` \^.<`^` `^`
<<\A.\B.`B` <`foo` \foo-ref. <`parent-ref` `foo-ref`
            <\_.
<<\A.\B.`B`   <`foo` \foo-ref. <`child-ref` `foo-ref`
<<\A.\B.`B`   <`parent-ref` \_. `foo`
<<\A.\B.`B`   <`child-ref` \_. `bar`
              <`foo` `_`
            `.`
" ~!> VSymbol "bar".
eval_blimp.
Qed.

Lemma shadowed : !"
<<\A.\B.`B` <`parent-ref` \^.<`^` `^`
<<\A.\B.`B` <`child-ref` \^.<`^` `^`
<<\A.\B.`B` <`foo` \foo-ref. <`parent-ref` `foo-ref`
<<\A.\B.`B` <\_.
<<\A.\B.`B`   <`foo` \foo-ref. <`child-ref` `foo-ref`
<<\A.\B.`B`   <`parent-ref` \_. `foo`
              <`child-ref` \_. `bar`
            `.`
            <`foo` `_`
(* " ~!> VSymbol "foo".
eval_blimp.
Qed.

Lemma local_reference : !"
            # Create a reference to `value` in the global scope.
<<\A.\B.`B` <<`ref1` \ref.`ref` <`value` \ref.`ref`
            # Create a reference to `value` in a temporary scope.
<<\A.\B.`B` <<`ref2` \ref.`ref` <\_.<`value` \ref.`ref` `.`
            # Initialize `value` in the global scope.
<<\A.\B.`B` <`ref1` \_.`foo`
            # Modify `value` using the reference from the child scope. This
            # should have no effect on the value in the global scope.
<<\A.\B.`B` <`ref2` \_.`bar`
<`value` `.`
" ~!> VSymbol "foo".
eval_blimp.
Qed.

Lemma first_choice : !"; `foo` `bar`" ~!> VSymbol "foo".
eval_blimp.
Qed.

Lemma second_choice : remove_whitespace ">\g.\s.\p.\x.
;             # Do a side-effect in the first branch
  <<\A.\B.`B` <`foo` \ref. <`ref` \_.`bar`
              # To force a failure, invoke the built-in grammar with a stream
              # that yields `<` and a sub-expression parser that returns
              # symbols. This will force the built-in grammar to parse a
              # sub-expression, and fail since a symbol is not a parse tree.
              <<<`g`
                \_.`<`
                \_.`.`
                \_.`<`
  <`foo` `.`
" ~!> VSymbol "bar".
eval_blimp.
Qed.

(** ** non-termination

An example of proving non-termination of a bl:mp program. Here we will prove
that the simple program [<<<`x``x``x``x`], which sends to [`x`] when [`x`]
points to itself, does not terminate. First, we will develop some useful tactics
and lemmas. *)

(** **** [assert_eval]: add a hypothesis [H] that [e] equals its evaluation

[e] should be a bl:mp computation, a [Machine A] applied to some depth limit and
input to yield an [A]. [e] must not depend on free variables, so that it can be
evaluated to completion yield a term of the form [Ok s r] or [Err s]. The
exception is that the depth limit in [e] may be a free variable. In this case,
it will be replaced by a natural number literal in the proven hypothesis. This
tactic will evaluate [e] using [vm_compute] and prove an equality, named [H],
that [e] is equal to the result of evaluation. *)
Ltac assert_eval H e hint := match e with
| ?f ?d ?i =>
  (** If [d] is free in [f], replace it with [hint] *)
  let f' := match eval pattern d in f with
  | ?f' _ => eval simpl in (f' hint)
  end in
  match eval vm_compute in (f' hint i) with
  | Ok ?s ?r => assert (H : f' hint i = Ok s r) by now vm_compute
  | Err ?s => assert (H : f' hint i = Err s) by now vm_compute
  | Diverge _ =>
      let hint' := eval simpl in (hint * 2) in
      assert_eval H e hint'
  end
end.

(** **** [post_eval_di']: apply depth irrelevance after [assert_eval]

The [assert_eval] tactic replaces the depth limit in the term it evaluates with
a literal natural number, so that it can fully evaluate the term. This tactic
can be used afterwards as long as [f d i] is final. It will prove that [f d i]
is equal to [f d' i] (the hypothesis added by [assert_eval]) using depth
irrelevance and replace [f d' i] with [f d i] in the goal. 

This tactic specifically applies to ['] functions (like [send'] or [parse'])
using the lemma [depth_irrelevance_inv']. For a general version which subsumes
this, use [post_eval_di]. *)
Ltac post_eval_di' f d d' H_eq :=
match f with
| send' ?ev ?rcv ?msg =>
  match ev with
  | context ev' [d] =>
    let eval_d := context ev' [d] in
    let eval_d' := context ev' [d'] in
    rewrite (depth_irrelevance_inv'
      (fun eval => send' eval rcv msg) eval_d d eval_d' d')
  end
| parse_sub_expr' ?ev ?stream ?rec =>
  match ev with
  | context ev' [d] =>
    let eval_d := context ev' [d] in
    let eval_d' := context ev' [d'] in
    rewrite (depth_irrelevance_inv'
      (fun eval => parse_sub_expr' eval stream rec) eval_d d eval_d' d')
  end
end;
[
|auto with depth_irrelevance_hints
|apply eval_ex_eq_fixM_rec
|apply eval_ex_eq_fixM_rec
|assumption
|rewrite H_eq; constructor 1]
.

(** **** [post_eval_di]: apply depth irrelevance after [assert_eval]

The [assert_eval] tactic replaces the depth limit in the term it evaluates with
a literal natural number, so that it can fully evaluate the term. This tactic
can be used afterwards as long as [f d i] is final. It will prove that [f d i]
is equal to [f d' i] (the hypothesis added by [assert_eval]) using depth
irrelevance and replace [f d' i] with [f d i] in the goal. *)
Ltac post_eval_di f d d0 H_eq := post_eval_di' f d d0 H_eq || (
  rewrite depth_irrelevance_inv with (d' := d0);
  [|auto with depth_irrelevance_hints|assumption|rewrite H_eq; constructor 1]
).

(** **** [rewrite_eval_blimp]: replace bl:mp terms with their evaluation *)
Ltac rewrite_eval_blimp := match goal with
| [ |- context g [?f ?d ?i] ] =>
    lazymatch f with
    | eval ?e => idtac
    | parse ?v => idtac
    | send' ?eval ?rcv ?msg => idtac
    | parse_sub_expr' ?eval ?stream ?recurse => idtac
    | fixM ?f ?a => idtac
    | lookup ?x => idtac
    | reads _ => idtac
    | _ => fail
    end;
    let H_eq := fresh "H_eq" in
    assert_eval H_eq (f d i) 16;
    match type of H_eq with
    | ?f' ?d_witness i = ?r =>
      (** Branch on the decidability of the finality of [eval d i]. *)
      let H_final := fresh "H_final" in
      let H_not_final := fresh "H_not_final" in
      destruct (final_lem $ f d i) as [H_final|H_not_final]; [
        (** In the branch where [eval d i] is final, we can use depth
        irrelevance to prove it is equal to [eval d_witness i]. *)
        post_eval_di f d d_witness H_eq;
        rewrite H_eq;
        clear H_eq H_final
      |
        (** In the branch where [eval d i] diverges, replace it with
        [Diverge s]. *)
        let s := fresh "s" in
        let H_eq_diverge := fresh "H_eq_diverge" in
        apply not_final_eq_diverge in H_not_final;
        destruct H_not_final as [s H_eq_diverge];
        rewrite H_eq_diverge
      ]
    end
| [ |- context g [?e [?x := ?v]] ] =>
    let H_eq := fresh "H_eq" in
    let e' := eval vm_compute in e[x := v] in
    replace (e[x := v]) with e' by now vm_compute
end.

(** **** [unfold_head]: unfold the first occurrence of an application in [e] *)
Ltac unfold_head e := match e with
| exists x, ?P = _ => unfold_head P
| ?l = _ => unfold_head l
| ?f _ => tryif is_const f then unfold f at 1 else unfold_head f
end.
Tactic Notation (at level 3) "unfold" "head" :=
  match goal with [ |- ?g ] => unfold_head g end.

(** **** [simpl_input]: simplify the first occurrence of an [Input] term *)
Ltac simpl_input := match goal with
| [ |- context g [?i] ] =>
    match type of i with
    | Input =>
        let i' := eval vm_compute in i in
        replace i with i' by now vm_compute
    | _ => fail
    end
end.
Tactic Notation (at level 3) "simpl" "input" := simpl_input.

(** **** [simpl_input_in]: simplify the first occurence of an [Input] term in [H] *)
Ltac simpl_input_in H := match type of H with
| context e [?i] =>
    match type of i with
    | Input =>
        let i' := eval vm_compute in i in
        replace i with i' in H by now vm_compute
    | _ => fail
    end
end.
Tactic Notation (at level 3) "simpl" "input" "in" constr(e) := simpl_input_in e.

(** **** [filter_heap]: truncate a heap constructed by bl:mp

If [h] is a [Heap] constructed by successive applications of [new],
[filter_heap h max] evaluates to a term which is identical to the results of
reconstructing [h] in the same way up to (and not including) the object at
address [max].

Note that any _pure_ bl:mp program (one that does not send to a symbol) modifies
the heap only through [new], and so [filter_heap] can be applied to the heap
resulting from such a program.

This function is used to compute the heap just before the [!] prelude executes
its input, in [bang_initial_input]. *)
Fixpoint filter_heap (h : Heap) (max : nat) : Heap := match max with
| O => fun _ => empty_scope None
| 1 => fun _ => empty_scope None
| S n => 
  let h' := filter_heap h n in
  let p := parent $ h n in
  h'[n := empty_scope p]
end.

(** **** [bang_initial_input]: the input to [p] after the [!] prelude has
executed. *)
Definition bang_initial_input (p : string) :=
  initial_input (remove_whitespace p)
    <|state;next_addr := 112|>
    <|state;heap := filter_heap (heap $ result_state $ run_blimp (!"") 35) 112|>
    <|ctx := Build_Context 111 $
      VBlock 105 "s" $ EBlock "p" $ EBlock "x" $ ESend
        (ESend
          (Z $ ESend
            (EValue $ VBlock 0 "s" $ EBlock "p" $ EBlock "_" $
              EPrimitive $ PrimParse (ESymbol "s") (ESymbol "p"))
            (ESymbol "s"))
          (ESymbol "x")) 
        (ESymbol "_")
    |>.

(** **** [bang_parse]: the function by which the [!] prelude parses the program
appended to it *)
Definition bang_parse (p : string) (d : nat) :=
  eval (ESend
    (Z $ ESend
      (EValue $ VBlock 0 "s" $ EBlock "p" $ EBlock "_" $
        EPrimitive $ PrimParse (ESymbol "s") (ESymbol "p"))
      (EValue $ VBlock 1 "_" $ EPrimitive PrimInput))
    (EValue $ VBlock 1 "_" $ EPrimitive PrimInput)
  ) d (bang_initial_input p)
.

(** **** [bang_diverge]: prove that a program using the [!] prelude diverges.

Specifically, [!p] diverges if [p] parses to [e] and evaluating [e] diverges. *)
Lemma bang_diverge (p : string) : forall s a x e,
  (** [p] parses to [e] *)
  (exists d, bang_parse p d = Ok s (VBlock a x e)) ->
  (** evaluating [e] (by sending to the parse tree for [e]) in the state after
  parsing diverges *)
  (forall d, exists s', eval e[x := VSymbol "_"] d
    (bang_initial_input p <|state := s|><|ctx;scope := a|>) = Diverge s') ->
  diverges (!p).
Proof.
(* begin details *)
intros s a x e H_parse H_eval.
(** Expand [!p] to the full [!] prelude, with whitespace removed, followed by
[p] with whitespace removed. *)
unfold "!".
match goal with
| [ |- context g [remove_whitespace ?s] ] =>
    let s' := eval vm_compute in (remove_whitespace s) in
    replace (remove_whitespace s) with s' by now vm_compute
end.
(** Prove divergence by showing the evaluation is equal to [Diverge s] for some
[s] given an arbitrary [d]. *)
apply diverges_at_any_depth.
intros d.
(** We now proceed to simplify the evaluation for which we want to show
divergence. The proof is formulaic. We start by unfolding until we get to a
sequence of bl:mp statements. We then evaluate the head statement and proceed
into the rest of the statements. *)
unfold run_blimp.
unfold head.
rewrite_eval_blimp; [simpl input|now eexists].
do 2 unfold head.
do 3 (unfold head; fold eval; rewrite_eval_blimp; [simpl input|now eexists]).
(** [rewrite_eval_blimp] fails when we get to a [fixM] application. To proceed,
we perform case analysis on the depth limit of the [fixM]. The program diverges
trivially when it is 0 (by the definition of [fixM]). In the [S] case, we can
use [fixM_succ] to rewrite the [fixM] in terms of its body. *)
unfold head; fold eval.
destruct d; [unfold fixM; now eexists|rewrite fixM_succ].
do 2 unfold head; simpl input.
destruct d; [unfold fixM; now eexists|rewrite fixM_succ].
rewrite_eval_blimp.
fold eval.
do 7 (unfold head; rewrite_eval_blimp; [simpl input|now eexists]).
unfold head; simpl input.
(** We proceed in a similar fashion, using [rewrite_eval_blimp] and [fixM_succ]
as appropriate, for as long as we can. *)
unfold parse'.
do 3 (unfold head; rewrite_eval_blimp; [simpl input|now eexists]).
unfold send'.
destruct d; [unfold fixM; now eexists|rewrite fixM_succ].
unfold with_ctx.
unfold head.
destruct d; [unfold fixM; now eexists|rewrite fixM_succ].
fold eval.
simpl.
(** We have now parsed the entirety of the [!] prelude. What remains is to parse
the input program [p] and to show that the effect of the [!] prelude is to
evaluate that parse tree (which diverges, by assumption). First we will simplify
a bit by folding [Z] in the goal. *)
match goal with |- context e [ESend (EBlock "g" _) ?g] =>
  replace (ESend (EBlock "g" _) g) with (Z g) by now unfold Z
end.
(** We currently have a send in our goal. Show that the receiver is the fixpoint
of the built-in parser, evaluated in [bang_initial_input]. *)
unfold head.
match goal with |- context g [eval _ _ ?i] =>
  replace i with (bang_initial_input p) by now vm_compute
end.
(** The [eval] statement has the effect of parsing the input program [p]. By
[H_parse], this returns [Ok s $ VBlock a x e] for some depth limit. We can thus
replace the head of the [match] in the goal with this [Ok] term, as long as we
have finality in order to apply depth irrelevance. So we branch on finality. *)
let branch := match goal with
| [ |- context g [eval ?e ?d ?i] ] => constr:(eval e d i)
end in
destruct (final_lem branch) as [H_final|H_not_final]; [ |
  (* In the branch where [eval] diverges, replace it with [Diverge s]. *)
  apply not_final_eq_diverge in H_not_final;
  destruct H_not_final as [? H_eq_diverge];
  rewrite H_eq_diverge;
  now eexists
].
(** We can now manipulate [H_parse] until it has a form matching the goal, and
use it to rewrite the goal. *)
destruct H_parse as [d' H_parse].
unfold bang_parse in H_parse.
rewrite depth_irrelevance_inv with (d' := d');
[|auto with depth_irrelevance_hints|assumption|rewrite H_parse; constructor].
rewrite H_parse.
(** We have proven that [!] has parsed [p] in accordance with [H_parse]. All
that remains is to show it then evaluates the parse tree. It takes a bit of
manipulation to put our goal in a form where we can apply [H_eval]. Once we can,
[H_eval] completes the proof. *)
unfold head.
replace (eval (ESymbol "_") _ _) with
  (Ok (state $ bang_initial_input p <|state := s|>) (VSymbol "_")) by eval_blimp.
unfold send'.
rewrite fixM_succ.
unfold with_ctx.
specialize (H_eval $ S d).
destruct H_eval as [s' H_eval].
simpl input.
simpl input in H_eval.
rewrite H_eval.
now exists s'.
(* end details *)
Qed.

(** *** the non-termination lemma

We are now ready to prove that the program [<<<`x``x``x``x`] diverges. *)
Lemma simple_loop_non_termination : diverges (!"<<<`x``x``x``x`").
Proof.
(* begin details *)
(** Apply [bang_diverge] to break the proof into a proof of parsing followed by
a proof of divergence on the AST. Since parsing terminates, we can handle the
parsing proof trivially using [eval_blimp]. *)
eapply bang_diverge; [unfold bang_parse; eval_blimp|].
(** We now need to prove that evaluating the parse tree for this program
diverges at any depth. *)
simpl input.
intros d.
(** The reason this program ultimately diverges is that it sets [`x`] to point
to itself and then sends to it, which diverges by [direct_loop_diverges]. We now
evaluate the program using a formulaic combination of [rewrite_eval_blimp] and
[fixM_succ] until we reach the point where we are sending to [`x`] while [`x`]
points to itself. *)
unfold head.
destruct d; [unfold fixM; now eexists|].
rewrite fixM_succ.
simpl.
unfold head.
rewrite_eval_blimp; [simpl_input|now eexists].
unfold head.
rewrite_eval_blimp; [simpl_input|now eexists].
unfold head.
destruct d; [unfold fixM; now eexists|].
rewrite fixM_succ.
unfold head; rewrite_eval_blimp; [simpl_input|now eexists].
destruct d; [unfold fixM; now eexists|].
rewrite fixM_succ.
unfold head.
rewrite_eval_blimp; [simpl_input|now eexists].
(** We have now evaluated the program to the point where we are about to send to
[`x`] in a state where [`x`] points to [`x`]. We are ready to apply
[direct_loop_diverges'] and finish the proof. *)
eexists.
fold eval.
(** We need to work on the type of [direct_loop_diverges'] a little bit before
it is unifiable with the goal, so add it as a local hypothesis. *)
pose (H_div := direct_loop_diverges').
unfold send' in H_div.
pose (H_div' := H_div $ fun e => fun _ : nat => eval e (S $ S d)).
apply H_div'.
clear H_div'. clear H_div.
(** Proving that [`x`] points to [`x`] is easy. The lookup operation itself
terminates, so we can just ask Coq to crunch. *)
exists 32. eexists. now vm_compute.
(* end details *)
Qed.
