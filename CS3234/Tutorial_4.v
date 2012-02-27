Parameter term : Type.

Require Import Classical.

Lemma Q5a: forall (S:Prop), forall (Q: term -> Prop) ,
(exists x, S -> Q x) -> S -> (exists x, Q x).
Proof.
intros.
destruct H.
apply H in H0.
exists x.
trivial.
Qed.

Lemma Q5b: forall (P Q: term -> Prop),
(forall x, P x \/ Q x) -> (forall x, P x) \/ (exists x, Q x).
Proof.
intros.
apply NNPP.
intro.
apply H0.
left.
intro.
generalize (H x); clear H; intro H.
destruct H.
trivial.
exfalso.
apply H0.
right.
exists x.
trivial.
Qed.


Parameter b: term.
Variable y: term.

Lemma Q5d: forall (P: term -> Prop),
(exists x, (x = y) /\ P x) -> P y.
Proof.
intros.
destruct H.
destruct H.
rewrite H in H0.
trivial.
Qed.

Parameter x: term.

Lemma Q5c: forall (P Q: term -> Prop),
(forall x, exists y, P x \/ Q y) -> (exists y, forall x, P x \/ Q y).
Proof.
intros.
apply NNPP.
intro.
apply H0.
exists b.
intros.
generalize (H x0); clear H; intro H.
destruct H.
destruct H.
left;trivial.
exfalso.
apply H0.
exists x1.
intro.
right;trivial.
Qed.