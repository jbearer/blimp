Require Import Blimp.Notation.
Require Import Coq.Init.Nat.
Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import RecordUpdate.RecordUpdate.
Open Scope string_scope.

(** * the semantics of bl:mp

This document defines a formal semantics for the bl:mp language. It defines
formally the bl:mp language (including the concrete syntax and parsing) together
with the abstract machine that bl:mp programs run in, which includes a few
pieces of state not represented syntactically in the language. We define
semantics for low-level operations in the machine, such as heap loads and
stores, and then we use those low-level operations to define [eval] #&mdash;#
the semantics for how the machine evaluates expressions to produce values
#&mdash;# and [run_blimp] #&mdash;# the semantics of running a complete bl:mp
program from input source code.

** why bother with formality?

Mostly because I want to, and because writing Coq semantics is fun. But it is
generally useful when developing a language to have a clear idea of what the
language means. For one thing, it serves a useful documentary purpose, since it
is more concise and more precise than English prose ever can be. (It is also
less easy to understand, which is why we should have documentation in both
forms.) For another thing, having the semantics defined formally allows us to
prove properties of bl:mp in an incontrovertible, automatically-checkable way.
See [Blimp.Properties] for some examples.

The formal semantics serves another, less obvious but equally essential,
purpose. By defining the language in total detail in a way that matches
intuition for the parts of the language which I have thought about and developed
intuition for, we get a starting point for resolving questions about how parts
of the language which have not been so carefully designed should behave. That
is, if we ever want to know how a language construct should work, or how it
should interact with another construct, we can ask the formal semantics, and it
will give us an answer which is guaranteed to be consistent with the rest of the
language. If we don't like that answer, we can refine the semantics, but it is
a good starting point for design.

You might wonder how this is different from just writing an interpreter and
defining the language based on the behavior of that interpreter. In fact, that's
almost what we're doing #&mdash;# the [eval_blimp] tactic defined in
[Blimp.Examples], which can be used to automatically prove that well-defined
bl:mp programs have a certain result #&mdash;# is just a bl:mp interpreter. It
even outputs a proof of correct interpretation. But, it is not a very efficient
interpreter, and so we maintain a separate, much more complicated implementation
of bl:mp written in C. There are advantages to defining the semantics separately
from the reference implementation of the language, and from defining it in this
way (with Coq, using the conventions of formal language theory):
- We can carry out machine-checked, formal proofs about properties of the
  language.
- In the formal semantics, we can prioritize clarity and stability over
  performance.
- We can provide visibility into the internals of the abstract machine without
  being tied to the internals of a particular implementation.
- We can use the semantics as a reference interpreter to test our practical
  implementation of an interpreter. For example, we can easily check that a
  clever new optimization does not result in behavior that visibly deviates from
  the specification by A/B testing a few examples.
- We can use the semantics as a model for proving the correctness of
  optimizations in the practical implementation. For example, the reference
  implementation includes a bytecode compiler, which translates bl:mp
  expressions to sequences of bytecode instructions which are more efficent to
  execute. We could model the semantics of the bytecode language in Coq, define
  the algorithm which is used to compile bl:mp to bytecode, and then use the
  formal semantics of bl:mp to prove the correctness of this algorithm (that is,
  that the resulting bytecode has the same behavior as the input bl:mp program).
- We can add optional features and extensions to the interpreter that are not
  part of the core language, whithout compromising the simplicity of the spec.

** why coq?

A previous version of these semantics was written in Redex, a Racket DSL
designed for specifying formal languages. While Redex was specifically designed
for this problem space, and has a few nice features (for instance, it can be
used to automatically generate an interpreter from semantics), it does not have
a proof system, which is a significant drawback. After all, half the fun of
having your language defined formally is to be able to write formal proofs about
it.

Meanwhile, Coq _is_ a proof system. When it comes to making abstract definitions
and proving properties about them in an automated, machine-checkable way, there
is no tool with the power, usability, and community adoption to compete with
Coq.

** the abstract machine

In order to define the semantics of bl:mp expressions, we define an abstract
machine in which expressions execute. The machine is defined as a collection of
state variables together with generic logic for chaining together and otherwise
combining fallible operations on that state. These combinators form a monad that
is somewhat akin to Haskell's [StateT] monad transformer applied to the [Either]
monad for handling failure. However, as we shall see, the bl:mp machine monad
adds an additional behavior to the stack: a maximal depth requirement that
ensures termination of the machine on any input.

*** state

The machine state consists of a heap, a grammar, an input program being
interpreted, the address of the object in whose scope the machine is currently
executing, and the smallest heap address not yet allocated, for the purposes of
allocating new objects. The [Heap] is a mapping from addresses to scopes, and a
[Scope] is a mapping from symbols to optional values, together with the address
of an optional parent scope.

*** depth limit

Each machine operation has a _depth limit_, which is a natural number [d]
indicating that the machine should abort execution if the operation requires
recursion with depth greater than [d]. This makes it easy to express our machine
operations as Coq functions (which are required to terminate) without doing a
lot of manual termination proofs which can be tedious at best and, indeed,
impossible at worst, because bl:mp doesn't terminate on all input programs.

The exact meaning of _depth_ is somewhat arbitrary and ad hoc, and depends
heavily on the precise way in which machine operations are specified. It is an
arbitrary construct with the property that more complex evaluations are
"deeper". This arbitrariness is not a problem, though #&mdash;# as long as our
semantics do not use the value of their depth limit other than as a structurally
decreasing fixpoint argument, it is straightforward to prove that for any
sufficient depth limit, the result of evaluating a machine operation does not
vary with the depth limit. We call this crucial property _depth irrelevance_,
and it is proven for all of the machine operations in [Blimp.Properties].

*** execution

The specific execution of the machine is defined by the semantics of evaluating
expressions and programs (e.g. [eval], [parse], and [run_blimp]). However, the
machine has a number of interesting, lower-level behaviors which are independent
of the specific semantics being executed. These include the standard monad
operations [ret] and [bind], as well as operations and combinators for reading
and modifying the state, failure and backtracking, and executing recursive
operations while ensuring termination. We briefly discuss these now:
- [ret] and [bind] implement the typical state/failure monad semantics: [ret]
  succeeds without updating the state, while [bind] sequences two operations: if
  the first succeeds, the second proceeds in the new state, while if the first
  fails, then the sequence fails without executing the second operation. Note
  that the state operations are stacked "on top" of failure operations #&mdash#
  if an operation fails, it still produces a new state. This is essential for
  implementing the language efficiently, because it means we do not have to save
  and revert to a previous state upon failure.
- [with_state] and [gets] can be used to read and modify the state. [with_ctx]
  and [reads] can be used to update local state within the context of a
  self-contained operation, and to get the local state, respectively. These
  pairs of operations correspond to Haskell's [State] and [Reader] monads,
  respectively, and the bl:mp state is partitioned based on whether changes to
  state variables propagate up the call stack ([State] monad) or effect only a
  locally self-contained operation ([Reader] monad). Specifically, [heap],
  [next_addr], and [input] are state variables while [scope] and [grammar] are
  local variables.
- [choice] implements a limited form of backtracking: it executes the first
  alternative, and then executes the second alternative (in the updated state)
  only if the first failed. Note that backtracking is limited to the execution
  of a [choice] expression: if [choice] succeeds with the first alternative and
  a later operation fails, the [choice] that succeeded will not be re-evaluated
  using the second alternative.
- [fixM] is a nifty fixpoint combinator which executes a recusive operation
  while automatically ensuring termination by decreasing the depth limit in each
  recursive call.

** interpreting the semantics

There are three main functions defined by this module that will be of interest
in interpreting semantic questions about bl:mp. They are [eval], [parse], and
[run_blimp].
- [eval] defines the evaluation of an arbitrary bl:mp expression in a given
  state of the abstract machine.
- [parse] defines how the language is parsed. Parsing in bl:mp is intermingled
  with evaluation, and so this function as well takes a machine state as input.
- [run_blimp] is a specialization of [parse] which parses an input source
  program using the built-in parser in the default machine state.

All of these functions are meant to return the result of the bl:mp operation
they represent; however, as described above, they each require a depth limit
[d] to ensure termination, and they may return [Diverge] instead of an actual
result if they are not given a sufficient depth limit. We now describe the
interpretation of semantic operations that sometimes (or always) return
[Diverge].

*** Interpreting divergence

Any bl:mp program can have one of three results:
- [Ok s v] means the program runs successfully, leaving the machine in state [s]
  and producing the [Value] [v].
- [Err s] means the program runs to completion but fails to produce a result,
  leaving the machine in state [s].
- [Diverge s] means the program diverges, or fails to run to completion, and it
  captures the state [s] of the machine when it stopped executing due to
  reaching its depth limit. There is subtlety here, as [run_blimp] returning
  [Diverge] really only means that the program fails to complete within a given
  depth limit. The same program may complete successfully given a greater depth
  limit. If we want our semantics do be unambiguous (we do!) we need to give an
  interpretation that resolves this apparent ambiguity.

To that end, the bl:mp machine returning a result of [Diverge] for any
particular depth limit is not to be interpreted as failure of the bl:mp program.
Instead, the behavior of a bl:mp program [p] is determined by
  <<lim>> #<sub>d&rarr;&infin;</sub># [run_blimp p d].
That is, if there is _any_ depth [d] such that the program completes with a
certain result, then that is the result of the program [p] (and that result is
unique by [depth_irrelevance], proven in [Blimp.Properties]).

This makes it easy to produce a proof that verifies the execution of a bl:mp
program, as long as that bl:mp program does terminate. Simply execute the
program with larger and larger depth limits until execution terminates. This is
exactly what the [eval_blimp] tactic does. Proving divergence is much harder: a
bl:mp program [p] diverges if [forall d, exists s, run_blimp p d = Diverge s].

This leads to a simple completeness result: for any well-formed bl:mp program
[p], there is a Coq proof term of type [exists d, run_blimp p d = r] for a
unique, non-divergent result [r]. On the other hand, while there exist some
divergent bl:mp programs [p_bad] for which the type
[forall d, exists s, run_blimp p_bad d = Diverge s] is inhabited (see
[simple_loop_non_termination] in [Blimp.Examples] for example), in general
proving divergence of a bl:mp program is equivalent to the halting problem,
since bl:mp is Turing complete (proving this is left as an exercise!).
Therefore, there exist some bl:mp programs [p] for which neither
[exists d r, final r /\ run_blimp p d = r] nor
[forall d, exists s, run_blimp p d = Diverge s] are inhabited.

Also, note that the interpretation of divergence is left up to the
implementation, in a constrained way. Nominally, divergence means that the
execution of a bl:mp program will never terminate. However, to ease debugging
and practical programming, implementations may choose instead to terminate with
an error if they detect definitively that a bl:mp program is divergent (this is,
of course, not always possible, but it is possible for many divergent programs).
The reference implementation has an option [-floop-errors] which enables this
early-exit behavior for certain classes of divergent programs.

** without further ado, the bl:mp lanugage...
*)

Declare Scope blimp_scope.
Open Scope blimp_scope.

Definition Symbol := string.
Definition Address := nat.

Inductive Expr : Set :=
  | ESymbol (x : Symbol)
  | EBlock (msg : Symbol) (body : Expr)
  | ESend (rcv : Expr) (msg : Expr)
  | EChoice (left : Expr) (right : Expr)
  | EValue (v : Value)
  | EPrimitive (p : Primitive)
with Value : Set :=
  | VSymbol (x : Symbol)
  | VBlock (addr : Address) (msg : Symbol) (body : Expr)
with Primitive : Set :=
  (** Set the value of [sym] in the scope located at [addr] *)
  | PrimSet (addr : Address) (sym : Symbol) (val : Expr)
  (** Parse the contents of [stream] using the built-in grammar, with
  sub-expressions parsed by [recurse] *)
  | PrimParse (stream : Expr) (recurse : Expr)
  (** Fail and trigger backtracking *)
  | PrimFail
.

(** *** Generic substitution in key-value-like containers. 

The class [Substitute] defines overloaded functions [update] and [substitute],
so that we can use some convenient notations for updating and substituting in a
variety of types, with similar semantics. *)
Class Substitute T K V :=
  { update : K -> (V -> V) -> T -> T
  ; substitute (k : K) (v : V) : T -> T := update k (fun _ => v)
  }.
Notation "t [ k := v ]" := (substitute k v t) (at level 0) : blimp_scope.
Notation "t [ k ::= f ]" := (update k f t) (at level 0) : blimp_scope.

(** *** Replace a value, named by a free variable, in [e].

This implements the beta reduction semantics when a block [\x.e] receives a
message [f x]. This is why we are substituting in a [Value] and not an
[Expr] #&mdash;# during beta reduction, the message being received is always a
[Value]. We will incorporate it into the [Expr] using the [EValue] constructor.

In [e], the symbol [x] is free and does not have an existing value associated
with it. Arbitrarily, the update function [f] will receive the value
[EValue $ VSymbol x]. This is irrelevant when [f] is of the form [fun _ => v]
for some constant [v], as is the case with [substitute], which will be derived
from this implementation of [update] according to the [Substitute] type class.
For beta reduction, it is [substitute] that we are primarily interested in. *)
Fixpoint update_expr (x : Symbol) (f : Value -> Value) (e : Expr) : Expr :=
match e with
| ESymbol x' => if x == x' then EValue $ f (VSymbol x) else e
| EBlock x' body => if x == x' then e else EBlock x' $ update_expr x f body
| ESend rcv msg => ESend (update_expr x f rcv) (update_expr x f msg)
| EChoice l r => EChoice (update_expr x f l) (update_expr x f r)
| EValue v => EValue $ update_value x f v
| EPrimitive p => EPrimitive $ update_prim x f p
end
with update_value (x : Symbol) (f : Value -> Value) (v : Value) : Value :=
match v with
| VSymbol _ => v
| VBlock a x' e => if x == x' then v else VBlock a x' $ update_expr x f e
end
with update_prim
  (x : Symbol) (f : Value -> Value) (p : Primitive) : Primitive :=
match p with
| PrimSet addr sym val => PrimSet addr sym (update_expr x f val)
| PrimParse stream recurse =>
    PrimParse (update_expr x f stream) (update_expr x f recurse)
| PrimFail => PrimFail
end.

#[export] Instance expr_substitute : Substitute Expr Symbol Value :=
  { update := update_expr }.

(** ** the bl:mp abstract machine *)
Module Machine.

(** **** [Scope]: map from symbols to values *)
Record Scope : Type :=
  { contents : Symbol -> option Value
  ; parent : option Address
  }.
#[export] Instance etaScope : Settable _ :=
  settable! Build_Scope <contents; parent>.

Definition empty_scope (parent : option Address) : Scope :=
  {| contents := fun s => None; parent := parent |}.

#[export] Instance scope_substitute : Substitute Scope Symbol (option Value) :=
  { update := fun x f s =>
      s<|contents ::= fun m x' => if x == x' then f $ m x else m x'|> }.

(** **** [Heap]: map from addresses to scopes *)
Definition Heap : Type := Address -> Scope.

#[export] Instance heap_substitute : Substitute Heap Address Scope :=
  { update := fun a f h => fun a' => if a == a' then f $ h a else h a' }.

(** **** [State]: global, mutable state *)
Record State : Type :=
  { heap : Heap
  ; next_addr : Address }.
#[export] Instance etaState : Settable _ :=
  settable! Build_State <heap; next_addr>.

(** **** [Context]: local state *)
Record Context : Type := { scope : Address; grammar : Value }.
#[export] Instance etaContext : Settable _ :=
  settable! Build_Context <scope; grammar>.

(** **** [Input]: combination of [Heap] and [Context] 

This is the input required to evaluate any [Machine A] to produce an [A]. *)
Record Input : Type := { state : State; ctx : Context }.
#[export] Instance etaInput : Settable _ := settable! Build_Input <state; ctx >.

(** **** [Result]: the result of a fallible computation of a value of type [A] *)
Inductive Result (A : Type) :=
  (** The computation succeeded, resulting in state [s] and producing the value
  [a]. *)
  | Ok (s : State) (a : A)
  (** The computation terminated in state [s] but failed to produce a value. *)
  | Err (s : State)
  (** The computation diverged because it exceeded its depth limit while in
  state [s]. *)
  | Diverge (s : State)
.
Arguments Ok [A].
Arguments Err [A].
Arguments Diverge {A}.

(** *** the bl:mp machine

[Machine A] is a bl:mp computation that produces a value of type [A] if it
succeeds (or produces a [Result A] in general).

The computation is a function that takes a [nat] (the depth limit) and an
[Input] (the current [State]/[Context]) and produces a new [State] along with a
fallible result. *)
Definition Machine A := nat -> Input -> Result A.

(** **** monadic identity

[ret a] succeeds with the value [a] and does not change the state. *)
Definition ret {A} (a : A) : Machine A := fun d i => Ok (state i) a.

(** **** monadic sequencing

[bind m f] executes [m], and then, if it succeded, applies [f] to the result and
executes the resulting computation in the new state. *)
Definition bind {A B} (m : Machine A) (f : A -> Machine B) : Machine B :=
fun d i => match m d i with
| Ok s a => f a d (i<|state := s|>)
| Err s => Err s
| Diverge s => Diverge s
end.
Notation "m >>= f" :=
  (bind m f) (at level 42, right associativity) : blimp_scope.
Notation "m >> n" :=
  (bind m $ fun _ => n) (at level 42, right associativity) : blimp_scope.
Notation "'do' x <- a ; b" :=
  (bind a (fun x => b)) (at level 100, x pattern, right associativity) : blimp_scope.

(** **** failure

[fail] unconditionally produces an [Err] result no matter the state. *)
Definition fail {A} : Machine A := fun d i => Err (state i).

(** **** backtracking

[choice l r] executes [l]. If it succeeds, the result is propagated. If it
fails, then [r] is executed in the new state. *)
Definition choice {A} (l : Machine A)  (r : Machine A) : Machine A := fun d i =>
match l d i with
| Ok s a => Ok s a
| Err s => r d $ i<|state := s|>
| Diverge s => Diverge s
end.
Notation "l <|> r" :=
  (choice l r) (at level 43, left associativity) : blimp_scope.

(** *** accessing state

**** [with_state] : apply a function [f] to the current machine [State] *)
Definition with_state {A} (f : State -> (State * A)) : Machine A := fun d i =>
  let (s, a) := f $ state i in
  Ok s a
.
(** **** [put] : [put proj] is a setter for a field [proj] *)
Definition put {R T} (proj : R -> T) (v : T) {setter_proj : Setter proj} :=
  set proj (fun _ => v).
(** **** [gets] : read a projection of the current state *)
Definition gets {A} (f : State -> A) : Machine A :=
  with_state $ fun s => (s, f s).
(** **** [puts] : set a projection of the current state to [a] *)
Definition puts {A}
  (proj : State -> A) (a : A) {setter_proj : Setter proj} : Machine A :=
  with_state $ fun s => (put proj a s, a).
(** **** [with_ctx] : execute a computation [m] in a modified local context *)
Definition with_ctx {A} (f : Context -> Context) (m : Machine A) : Machine A :=
  fun d i => m d (i<|ctx ::= f|>).
(** **** [reads] : get the local context *)
Definition reads {A} (f : Context -> A) : Machine A :=
  fun d i => Ok (state i) $ f (ctx i).
(** **** [fixM] : a fixpoint combinator which automatically decreases on [d]

[fixM f a d] is the least fixpoint of [f], applied to [a], except that if it
recurses [d] times or more, it terminates with a [Diverge] result instead of
continuing the (possibly infinite) computation. *)
Fixpoint fixM {A R} (f : (A -> Machine R) -> A -> Machine R) (a : A) (d : nat)
  : Input -> Result R
:= match d with
| S d' => f (fun a' _ => fixM f a' d') a d'
| O => fun i => Diverge $ state i
end.
Notation "'fix-machine' f pat => body" :=
  (fixM $ fun f arg => let pat := arg in body)
  (at level 100, f name, pat pattern).

End Machine.
Import Machine.

(** ** semantics of bl:mp execution *)

(** **** [alloc] : get a fresh address *)
Definition alloc : Machine Address := 
  do p <- reads scope;
  with_state $ fun s =>
    let a := next_addr s in
    (s<|next_addr ::= S|><|heap ::= substitute a (empty_scope $ Some p)|>, a).

(** **** [new] : allocate a new block [\x.e] *)
Definition new (x : Symbol) (e : Expr) : Machine Value :=
  do a <- alloc;
  ret $ VBlock a x e
.

(** **** [lookup] : get the value of a symbol in the current scope *)
Definition lookup : Symbol -> Machine (option Value) := fix-machine lookup x =>
  do a <- reads scope;
  do h <- gets heap;
  match contents (h a) x with
  | Some v => ret $ Some v
  | None => match parent (h a) with
    | Some p => with_ctx (put scope p) $ lookup x
    | None => ret None
    end
  end
.

(** **** [pair combinators]: construct a pair, destruct with [car] and [cdr] *)
Definition P (x : Expr) (y : Expr) : Expr :=
  EBlock "^" $ EChoice (ESend (ESymbol "^") x) y.
Definition A (p : Expr) : Expr := ESend p (EBlock "^" $ ESymbol "^").
Definition D (p : Expr) : Expr := ESend p (EBlock "^" $ EPrimitive PrimFail).

(** **** [Z] : a fixpoint combinator on bl:mp objects *)
Definition Z (g : Expr) : Expr :=
ESend (EBlock "g" $
  ESend (EBlock "x" (ESend (ESymbol "g") (EBlock "v" (ESend (ESend (ESymbol "x") (ESymbol "x")) (ESymbol "v")))))
        (EBlock "x" (ESend (ESymbol "g") (EBlock "v" (ESend (ESend (ESymbol "x") (ESymbol "x")) (ESymbol "v")))))
) g.

(** **** evaluation helpers

For simplicity of presentation, there are many sub-operations of the evaluation
procedure #&mdash;# for example, sending and parsing #&mdash;# that we want to
present as standalone functions. However, all of these functions would be
mutually recursive with [eval], but the [fixM] combinator does not directly
support mutual recursion. Instead, we parameterize each of these sub-operations
on the operation it should use to evaluate sub-expressions. In the definition of
[eval], we then instantiate each of these with the recursive argument of the
fixpoint. In this way, we effectively turn mutual recursion into direct
recursion.

We adopt the convention of naming the parameterized helpers with a ['], e.g.
[send'] and [parse']. The instantiated versions, defined after [eval], are
notated without the ['], e.g. [send] and [parse].
*)

(** **** send a message to an object *)
Definition send' (eval : Expr -> Machine Value)
  (rcv : Value) (msg : Value) : Machine Value :=
(fix-machine send (rcv, msg) => match rcv with
  (** to send to a symbol, first get the value of the symbol... *)
  | VSymbol x => do v <- lookup x; match v with
    (** if it is in scope, recursively send the message to the value *)
    | Some rcv' => send (rcv', msg)
    | None =>
        (** otherwise, interpret [msg] as an initializer for [x], create a setter
        for [x] in the current scope, and send it to [msg] *)
        do a <- reads scope;
        send (msg, VBlock a "^" $ EPrimitive $ PrimSet a x $ ESymbol "^")
    end
  | VBlock a x e =>
      (** to send to a block, simply evaluate the body (with [x] replaced by the
      message) in the block's scope *)
      with_ctx (put scope a) $ eval e[x := msg]
  end
) (rcv, msg).

(** **** parse based on the current ambient grammar, using [stream] as a stream *)
Definition parse' (eval : Expr -> Machine Value)
  (stream : Value) : Machine Value :=
do g <- reads grammar;
(** specialize the grammar to an input stream *)
do g' <- send' eval g stream;
(** compute the fixpoint of the grammar to generate a parser *)
do p <- eval $ Z (EValue g');
(** invoke the parser on the input stream *)
send' eval p stream.

(** **** parse a symbol literal using the built-in concrete syntax

In the built-in syntax, a symbol literal is denoted by a delimiter character
followed by sequence of characters not including the delimiter and then the
delimiter again.

This function assumes that the opening delimiter has already been read from the
input stream, and interprets the first delimiter character yielded by the stream
to be the closing delimiter.
*)
Definition parse_symbol' (eval : Expr -> Machine Value)
  (delim : Symbol) (stream : Value) : Machine (Symbol * Value) :=
(fix-machine parse_symbol (delim, stream) =>
  (** send to the stream to read a character of input *)
  do c <- eval $ A $ EValue stream;
  do cs <- eval $ D $ EValue stream;
  match c with
  | VSymbol x => if x == delim then ret ("", cs) else
      (** if the character is not the closing delimiter, recursively parse the
      rest of the symbol and then prepend the character *)
      do (xs, cs) <- parse_symbol (delim, cs);
      ret (append x xs, cs)
  | _ => fail
  end
) (delim, stream).

Definition parse_ret' (eval : Expr -> Machine Value)
                      (e : Expr) (stream : Value) : Machine Value :=
do tree <- new "^" e;
eval $ P (EValue tree) (EValue stream).

(** **** parse an expression

Send [stream] to [parser] and interpret the result as a pair of a tethered
expression and a new stream. Extract the expression fr  om the body of the block\
and returning it along with the new stream. *)
Definition parse_sub_expr' (eval : Expr -> Machine Value)
  (stream : Value) (parser : Value) : Machine (Expr * Value) :=
do v <- send' eval parser stream;
do tree <- eval (A $ EValue v);
do stream <- eval (D $ EValue v);
match tree with
| VBlock _ _ e => ret (e, stream)
| VSymbol _ => fail
end.

(** **** evaluate a bl:mp expression in the current state *)
Definition eval : Expr -> Machine Value := fix-machine eval e =>
let send := send' eval in
let parse := parse' eval in
let parse_sub_expr := parse_sub_expr' eval in
let parse_ret := parse_ret' eval in
(** [primitive_parse] : parse a stream using the built-in grammar *)
let primitive_parse
  (x : Value) (stream : Value) (recurse : Value) : Machine Value :=
match x with
| VSymbol ">" => (** macro : [<expr> ::= ">" <expr> ...]

    Parse a _grammar overlay_ as a subexpression, and derive a new grammar by
    appying it to the current grammar. *)
    do (overlay, stream) <- parse_sub_expr stream recurse;
    do overlay' <- eval overlay;
    do g <- reads grammar;
    do g' <- send overlay' g;
    (** Parse more input using the modified grammar. *)
    with_ctx (put grammar g') $ parse stream
| VSymbol "\" => (** block : [<expr> ::= "\" <symbol> "." <expr>] *)
    do (x, stream) <- parse_symbol' eval "." stream;
    do (e, stream) <- parse_sub_expr stream recurse;
    parse_ret (EBlock x e) stream
| VSymbol "<" => (** send : [<expr> ::= "<" <expr> <expr>] *)
    do (rcv, stream) <- parse_sub_expr stream recurse;
    do (msg, stream) <- parse_sub_expr stream recurse;
    parse_ret (ESend rcv msg) stream
| VSymbol ";" => (** choice : [<expr> ::= ";" <expr> <expr>] *)
    do (l, stream) <- parse_sub_expr stream recurse;
    do (r, stream) <- parse_sub_expr stream recurse;
    parse_ret (EChoice l r) stream
| VSymbol delim => (** symbol : [<expr> ::= <symbol>] *)
    do (x, stream) <- parse_symbol' eval delim stream;
    parse_ret (ESymbol x) stream
| VBlock _ _ _ => 
    (** If the stream yields a non-symbol object, just pass it through as-is. *)
    parse_ret (EValue x) stream
end in
(** [eval_primitive] : built-in functionality *)
let eval_primitive (p : Primitive) : Machine Value := match p with
| PrimSet a x e =>
    do v <- eval e;
    with_state $ fun s =>
      (s<|heap ::= update a (substitute x $ Some v)|>, VSymbol x)
| PrimParse stream recurse =>
    do c <- eval $ A stream;
    do stream' <- eval $ D stream;
    do recurse' <- eval recurse;
    primitive_parse c stream' recurse'
| PrimFail => fail
end in
(** expression interpretation *)
match e with
| ESymbol x => ret $ VSymbol x
| EBlock x e => new x e
| ESend rcv msg =>
    do rcv' <- eval rcv;
    do msg' <- eval msg;
    send rcv' msg'
| EChoice l r => eval l <|> eval r
| EValue v => ret v
| EPrimitive p => eval_primitive p
end.

Definition send : Value -> Value -> Machine Value := send' eval.
Definition parse : Value -> Machine Value := parse' eval.
Definition parse_symbol : Symbol -> Value -> Machine (Symbol * Value) :=
  parse_symbol' eval.
Definition parse_sub_expr : Value -> Value -> Machine (Expr * Value) :=
  parse_sub_expr' eval.

(** **** the [Input] used to evaluate all complete bl:mp programs

In the initial input, the heap is empty, the input stream consists of the text
of the program being evaluated, the ambient grammar is the built-in grammar, and
execution proceeds in scope [0], the global scope.
*)
Definition initial_input : Input :=  {|
  state := {|
    heap := fun _ => empty_scope None;
    next_addr := 1;
  |};
  ctx := {|
    scope := 0;
    grammar := VBlock 0 "^" $ EBlock "p" $ EBlock "s" $
      EPrimitive $ PrimParse (ESymbol "s") (ESymbol "p");
  |}
|}.

Fixpoint mk_stream (s : string) : Expr := match s with
| "" => EBlock "_" $ EPrimitive PrimFail
| String c cs => P (ESymbol $ String c "") (mk_stream cs)
end.

(** **** execute a complete bl:mp program from its source code *)
Definition run_blimp (program : string) (depth : nat) : Result Value :=
  let m :=
    do stream <- eval $ mk_stream program;
    parse stream
  in m depth initial_input
.
