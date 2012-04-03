(* For 2 points, include your name and matric no.
Name: Benjamin Tan Wei Hao 
Matric No. : U077129N
*)

(*
                                     HomeJoy 2 - Induction & Formalism

CS3234 Sem 2 Ay 11/12
This homejoy is biased towards those who will work hard rather than work smart.
Because hey; that's the name of the game in fomalism.
Though, working smart can't hurt.
 - Sally Chak Hanrui with questions stolen from
    Aqunias Hobor &
    Benjamin Pierce

Instructions:
    All of the following problems are provable; please replace the "admit" 
    tactics below with other tactics.

    You now have access to solvers such as:
    exact
    assumption
    trivial
    auto
    omega (*However, for this homejoy, cannot be used for certain questions*)
                (*These are explicitly marked out*)

    You will find that sometimes tactics like
    destruct, apply and inversion also solve goals.

    Here is a list of currently banned tactics:
      admit
      firstorder
      tauto
      intuition

Also banned are any other tactics not mentioned that do copious
amounts of work (logically).
*)


Require Import CS3234_inductionLab CS3234_inductionPractice.

Print lt.
Print le.
Lemma tricho: (*Here are some helpful facts about less than*)
forall x y, (*less than obeys trichotomy*)
x < y \/ x = y \/ y < x.
Proof with auto.
  intros.
  destruct (le_or_lt x y)... (*For those who want to learn*)
  destruct (le_lt_or_eq x y H)... (*more about Coq,*)
Qed. (*try googling "Coq le_or_lt"*)

Lemma ltspo:
forall x y z, (*less than is a strict partial order*)
(~ x < x) /\ ((x < y /\ y < z) -> x < z).
Proof with auto. intros;split. apply lt_irrefl.
intro. transitivity y;destruct H... Qed.

Lemma Peano:
forall x , ~ x = S x.
Proof. apply n_Sn. Qed.


(*888888888888888888888888888888888888888*)
(* nat induction and Formalism Problems 45 points*)
(*888888888888888888888888888888888888888*)
(* no omega for this section *)


(*888888888888NO OMEGA888888888888*)
(*888888888888NO OMEGA888888888888*)
(*888888888888NO OMEGA888888888888*)

Lemma predS: forall x, (*2 points*)
pred (S x) = x.
Proof.
intro.
simpl.
trivial.  
Qed.

Lemma Spred: forall x, (*3 points*)
(x <> 0) ->
S (pred x) = x.
Proof.
intros.
induction x.
induction 0.
trivial.
rewrite (predS(x)).
trivial.
Qed.

Lemma both0: (*3 points*)
forall x y, x + y = 0 -> x=0 /\ y=0.
Proof.
intros.
induction x.
simpl in H.
split.
trivial.
trivial.
destruct IHx.
simpl in H.
discriminate. (*like contradiction?*)
split.
discriminate.
trivial.
Qed.

Definition mdop: (*8 points*)
forall x y z,
x*(y+z) = x*y + x*z.
intuition.

Proof.
intros.
induction x.
simpl.
trivial.
simpl.
rewrite pass.
rewrite IHx.
rewrite pass.
rewrite <- (pass y (x * y)).
rewrite (pcomm(x*y)).
rewrite pass.
trivial.
Qed.


Lemma difference: (*10 points*)
forall x y,
(x < y) -> exists z, y = x + z.
Proof with auto.
intros.
induction x.
exists y.
simpl.
trivial.
assert(x < S x).
auto.
assert(x<y).
destruct (ltspo x (S x) y).
apply H2.
auto.
destruct(IHx H1).
induction x0.
rewrite H2 in H1.
destruct (ltspo x (S x) y).
rewrite(xp0) in H1.
contradiction.
exists x0.
simpl.
rewrite pcomm in H2.
simpl in H2.
rewrite pcomm.
trivial.
Qed.
(*Actually, it is more common to take this as the definition
of 'less than' or 'less than or equal' and to prove that it is a
strict/non-strict total order*)


Lemma pcan: (*3 points*)
forall x y z,
x + y = x + z <-> y=z.
Proof.
intros.
induction x.
simpl.
split.
intro.
trivial.
intro.
trivial.
destruct IHx.
simpl.
split.
auto.
auto.
Qed.


Lemma mcan: (*14 points*)
forall x y z, (lt 0 x) ->  (x*y = x*z) -> (y=z).
Proof.
induction x.
simpl.
intros.
inversion H.
intros.
destruct (tricho y z).
destruct (difference y z H1).
rewrite H2 in H0.
simpl in H0.
rewrite mdop in H0.
repeat (rewrite <- pass in H0).
rewrite pcan in H0.
rewrite <- (xp0 (x*y) ) in H0.
repeat (rewrite pass in H0).
rewrite (pcomm x0) in H0.
repeat (rewrite <- pass in H0).
rewrite pcan in H0.
simpl in H0.
generalize (both0 x0 (x*x0)); intro.
destruct H3.
auto.
rewrite H3 in H2.
rewrite (xp0) in H2.
auto.
destruct H1.
trivial.
apply (difference z y) in H1.
destruct H1.
clear -H0 H1.
simpl in H0.
rewrite H1 in H0.
rewrite (mdop x z x0) in H0.
repeat (rewrite pass in H0).
rewrite <- pass in H0.
repeat (rewrite <- pass in H0).
rewrite (pcan z) in H0.
rewrite <- (xp0 (x*z)) in H0.
rewrite pass in H0.
rewrite pass in H0.
rewrite (pcomm x0) in H0.
rewrite <- pass in H0.
rewrite <- pass in H0.
rewrite (pcan (x*z)) in H0.
simpl in H0.
generalize (both0 (x0) (x*x0)).
intros.
destruct H.
trivial.
rewrite H in H1.
rewrite (xp0) in H1.
auto.
Qed.


(* list induction Problems 40 points*)
(* In this section, We'll be looking at lists. We haven't gone through lists
    in class, so pay attention to the "Prints" below. These are the "canonical"
    linked lists from functional programming favourites such as LISP, SML and
    Haskell. Things like map have their obvious meanings here.

Here is a possibly helpful reference.
http://en.wikipedia.org/wiki/Map_(higher-order_function)
*)

Print list.
Print map.
Definition doub x : nat := x + x.


Fixpoint smartget (A:Type) (l:list A) (index:nat) : option A :=
(*get is what you might expect. It takes a number and a list
and returns the element indexed by that list. Indexes start at 1
get 1 x
returns the first element of x *)
match l, index with
  |nil, _ => None
  | _ , 0 => None (* :: is syntatic sugar for "cons"*)
  | h :: t, S n => match n with      (* cons h t = h :: t*)
                           |O => Some h
                           |S x => (smartget A t n)
                         end
end.  Implicit Arguments smartget [A].

(*let's have a look at this stuff in action*)

Goal smartget (cons 1 (cons 2 nil)) 0 = None.
unfold smartget. reflexivity. Qed.

Goal smartget (1:: 2:: nil) 1 = Some 1.
unfold smartget. reflexivity. Qed.

Goal smartget (1:: 2:: nil) 2 = Some 2.
unfold smartget. reflexivity. Qed.

Goal map doub (cons 1 (cons 2 nil)) = (cons 2 (cons 4 nil)).
simpl.  unfold doub.  simpl.  auto.  Qed.

Goal
smartget (map doub (cons 1 (cons 2 (cons 3 nil)))) 1 = Some 2.
simpl. unfold doub. simpl. auto. Qed.


(*   Now would be a good time to figure out what these "Some" and "None" things
      we keep seeing. They are a functional programming way of defining a partial
      function. For the inputs that we want to disregard or don't care about, we
      return "None". Like what is    get -10 list? or division by zero? These should
      intuitively be "None". For everything else, we return "Some" followed by the
      expected value. But why can't we return the value, x rather than "Some" x?
      Because then the function wouldn't be returning a single type. It would
      return "None" (: Option) in some cases and x(:whatever_type) in others.
      For why our function must return a single type, consult any PL textbook

      If you're having a lot of trouble, here are some links
      http://en.wikipedia.org/wiki/Option_type
      http://ocaml-lib.sourceforge.net/doc/Option.html
      http://www.codecommit.com/blog/scala/the-option-pattern
*)


Fixpoint get (A:Type) (l: list A) (index:nat) : option A :=
match l, index with
  |nil, _ => None
  |cons a tl, O => Some a
  |cons a tl, S n => get A tl n
end.
Implicit Arguments get [A].

(*The difference between get and smartget is that
with get, the first element is indexed with 0, whereas with smartget,
some smarty pants wrote it such that the first element is indexed with 1*)



Lemma factAbtSGet:(*XC: prove this without our banned, fancy solvers for 8points*)
forall (A:Type) (a:A) (L : list A) n, n <> 0 -> (smartget (a :: L) (S n) = smartget L n).
Proof. 
destruct n. 
intros.
destruct H. (* ztf *)
trivial.
intro.
simpl.
trivial.
Qed.


(*  If you get a hypothesis like None = Some x, then you can
    use the "inversion" or "discriminate" tactics to expose the
    contradiction. "inversion" can also be useful if you have a
    hypothesis like Some x = Some y, since it will give you x=y. *)
Lemma map_double: forall n L o, (*14 Points*)
  smartget (map doub L) n = Some o ->
  exists o',
      smartget L n = Some o' /\ o = doub o'.
Proof.
intro.
intro.
revert n.
induction L.
simpl.
intros.
inversion H.
intros.
destruct n as [x |?].
simpl in H.
discriminate.
inversion H.
induction n.
simpl in IHL.
simpl in H1.
simpl in H.
inversion H.
exists a.
split; trivial.
simpl in IHn.
exact (IHL (S n) o H).
Qed.


(*We now define a new fixpoint on lists.
snoc, which behaves like a cons on the right*)

Fixpoint snoc (A:Type) (l : list A) (a:A) : list A := 
match l with
  | nil    => a :: nil
  | h :: t => h :: (snoc A t a)
end. Implicit Arguments snoc [A].

(*and use it to define a list-reversing function, rev
like this *)

Fixpoint rev (A: Type) (l: list A) : list A:= 
match l with
  | nil    => nil
  | h :: t => snoc (rev A t) h
end. Implicit Arguments rev [A].

Lemma rev_involutive : forall l : list nat,
  rev (rev l) = l. (* 12 points*)
Proof with omega.
induction l.
simpl.
trivial.
simpl.
assert (forall L, rev (snoc L a) = a :: (rev L)).
induction L.
auto.
simpl.
rewrite IHL.
auto.
rewrite H.
rewrite IHl.
trivial.
Qed.

Print app.
Lemma distr_rev : forall l1 l2 : list nat, (*14 points*)
  rev (l1 ++ l2) = (rev l2) ++ (rev l1).
Proof.
intros.
induction l1.
simpl.
induction (rev l2). 
auto. 
simpl.
rewrite <- IHl. 
trivial.
simpl.
assert (forall x y, x ++ (snoc y a) = snoc (x ++ y) a).
induction x.
simpl.
intros.
auto.
simpl.
intro.
generalize (IHx y).
intros.
rewrite H.
auto.
rewrite H.
rewrite IHl1.
auto.
Qed.


(*8888888888888888888888888888*)
(* tree induction Problems 30 points *)
(*8888888888888888888888888888*)


Module treestreestrees.
Import tree_induction.

Print bT.

Fixpoint swap (t : bT) : bT :=
match t with 
 | leaf => leaf
 | node t1 t2 => node t2 t1
end.


Fixpoint deep_swap (t : bT) : bT :=
match t with 
 | leaf => leaf
 | node t1 t2 => node (deep_swap t2) (deep_swap t1)
end.


Lemma swap_size: forall t, (* 8 points *)
  size t = size(deep_swap t).
Proof.
  intros.
  induction t.
  simpl.
  trivial.
  simpl...
  rewrite pcomm.
  rewrite IHt1.
  rewrite IHt2.
  trivial.
Qed.

Fixpoint swaperina t :=
match t with
  |leaf => leaf
  |node lc rc => swap (node (deep_swap lc) rc)
end.

Lemma swaparoo : forall t, (* 8 points *)
swaperina (swaperina (swaperina (swaperina t))) = t.
Proof.
intros.
induction t.
simpl...
trivial.
assert(forall t, deep_swap(deep_swap(t)) = t).
intro.
induction t.
simpl...
trivial.
simpl...
rewrite IHt3.
rewrite IHt4.
trivial.
simpl...
generalize(H t1);intro.
generalize(H t2);intro.
rewrite H0, H1.
trivial.
Qed.


Print ntree.
Fixpoint weave t1 t2 :=
match t2 with
  |nleaf x => nnode t1 (nleaf x)
  |nnode lc rc => nnode t1 (weave lc rc)
end.

Fixpoint rightmost t :=
match t with
  |nleaf x => x
  |nnode lc rc => rightmost rc
end.

Lemma mostRight:forall t1 t2, (*14 points*)
rightmost t2 = rightmost (weave t1 t2).
Proof.
induction t1.
induction t2.
simpl.
auto.
simpl.
destruct t2_2.
auto.
simpl.
auto.
induction t2.
simpl.
trivial.
auto.
destruct t2_2.
simpl.
trivial.
simpl.
auto.
Qed.


























(*888888888888888888*)
(* XC material 55 points *)
(*888888888888888888*)


(*these next 3 questions are real irritating to do and
frankly, if I were you, I wouldn't bother unless I 
desperately needed the marks.*)

(*Formalism XC*)
Lemma SinceIWas15:  (*5 points*)
forall a b, (a+b)*(a+b) = a*a + 2*a*b + b*b.
Proof with auto. (*Think hard about what (if anything) you want to induct on*)
  
admit.
Qed.


Lemma NotAfraidOfSymbols:   (*4 points*)
forall x, exists k,
x = 4*k \/ x = 4*k + 1 \/ x = 4*k + 2 \/ x = 4*k + 3.
Proof with auto.
  
admit.
Qed.


Lemma NotAfraidOfPain:    (*6 points*)
forall x, exists k:nat,
(x*x = 4*k) \/ (x*x = 4*k +1).
Proof with auto.
  
admit.
Qed.
(*Formalism XC*)


(*List XC*)
(************************************************)
(* For 8 points, Scroll up and prove "FactAbtget"*)
(************************************************)


Print pair.
Print fst.
Print snd.

Section MOI.
Variables (Obj:Type) (x:Obj).

(*Ask yourself what this lemma is saying.*)
Lemma MasterOfInduction:
  forall (L : list (prod nat Obj)) n,
  get L n = Some (pair n x) ->
  get (map (@snd nat Obj) L) n = Some x.
(*In this case. (nat * Obj) means a pair where the first element is nat and the
second is a Obj. 

(0, x) : (nat*Obj)

*)
Proof. (*12 points*)
  
admit.
Qed.
End MOI.
(*List XC*)


(*Tree XC*)

Fixpoint left_skewed (t : bT) : Prop := (* 20 points *)
match t with
  | leaf => True
  | node (node t11 t12) t2 => 
          left_skewed t11 /\ left_skewed t12 /\ left_skewed t2
  | _ => False
end.

Fixpoint right_skewed (t : bT) : Prop :=
match t with
  | leaf => True
  | node t1 (node t21 t22) => 
    
      right_skewed t1 /\ right_skewed t21 /\ right_skewed t22
  | _ => False
end.

(*  It's not really fair of us to ask you to do this, since it is quite
    tricky. Do everything else first. You might find a way forward by
    cleverly strengthening the induction hypothesis; alternatively you
    can investigate the "fix" tactic (look it up in Coq reference manual).
    Note that "fix" can be very tricky to use; make sure that you do a
    "Qed" before celebrating since it is an "unsafe" tactic---you can get
    to "Proof completed." only to have the "Qed" fail. *)

Lemma swap_skew:  (*20 points*)
  forall t, left_skewed t -> right_skewed (deep_swap t).
Proof with trivial.
  
admit.
Qed.
(*Tree XC*)
End treestreestrees.