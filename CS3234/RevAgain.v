Variable term:Type.
Variable S: Prop.
Variable (P :term -> Prop).
Variable (Q :term -> Prop).
Variable b:term.

Require Import Classical.

Goal (forall x, (P x \/ Q x)) -> (forall x, P x) \/ (exists x, Q x).
Proof.
intro.
apply NNPP.
intro.
apply H0.
left.
intro.
generalize(H x);clear H; intro H.
destruct H.
trivial.
exfalso.
apply H0.
right.
exists x.
trivial.
Qed.

Goal (forall x, (P x \/ Q x)) -> (forall x, P x) \/ (exists x, Q x).
Proof.
intro.
apply NNPP.
intro.
apply H0.
left.
intro.
generalize(H x).
intro.
destruct H1.
trivial.
exfalso.
apply H0.
right.
exists x.
trivial.
Qed.


Goal (forall x, exists y, (P x \/ Q y)) -> (exists y, forall x, (P x \/ Q y)).
Proof.
admit.
Qed.

Goal (forall x, (P x /\ Q x)) -> (forall x, P x -> Q x).
Proof.

Qed.

Goal (exists x, (~P x /\ ~Q x)) -> (exists x, (~(P x /\ Q x))).
Proof.
Qed.

Goal (exists x, (~P x \/ Q x)) -> (exists x, (~(P x /\ ~Q x))).
Proof.

Qed.

Goal (forall x, (P x /\ Q x)) -> forall x, P x /\ forall y, Q y.
Proof.

Qed.

Goal (forall x, P x) \/ (forall x, Q x) -> forall x, (P x \/ Q x).
Proof.

Qed.

Goal (exists x, (P x /\ Q x)) -> (exists x, P x) /\ (exists x, Q x).
Proof.

Qed.

Goal (exists x, P x) /\ (exists x, Q x) -> (exists x, (P x \/ Q x)).
Proof.

Qed.

Goal (forall x, forall y, (P y -> Q x)) -> ((exists y, P y) -> (forall x, Q x)).
Proof. 

Qed.

Goal (~forall x, ~P x) -> (exists x, P x).
Proof.

Qed.

Goal (forall x, ~P x) -> (~exists x, P x).
Proof.

Qed.

Goal (~exists x, P x) -> (forall x, ~P x).
Proof.

Qed. 
