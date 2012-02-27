Variable term:Type.
Variable S: Prop.
Variable (P :term -> Prop).
Variable (Q :term -> Prop).
Variable b:term.

Require Import Classical.

Goal S -> (exists x, Q x) -> exists x, (S -> Q x).
Proof.
intros.
destruct H0.
exists x.
intro.
trivial.
Qed.

Goal ((exists x, P x) -> S) -> (forall x, (P x -> S)).
Proof.
intros.
apply H.
exists x.
trivial.
Qed.

Goal ((forall x, P x) -> S) -> exists x, (P x -> S).
Proof.
intros.
apply NNPP.
intro.
apply H0.
exists b.
intro.
apply H.
intro.
apply NNPP.
intro.
apply H0.
exists x.
intro.
contradiction.
Qed.


Goal (forall x, (P x \/ Q x)) -> (forall x, P x) \/ (exists x, Q x).
Proof.
intros.
apply NNPP.
intro.
apply H0.
left.
intro.
generalize(H x);clear H;intro H.
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
generalize(H x);clear H; intro H.
destruct H.
trivial.
exfalso.
apply H0.
right.
exists x.
trivial.
Qed.

Goal (forall x, exists y, (P x \/ Q y)) -> (exists y, forall x, (P x \/ Q y)).
Proof.
intros.
apply NNPP.
intro.
apply H0.
exists b.
intro.
generalize(H x);clear H;intro H.
destruct H.
destruct H.
left.
trivial.
right.
exfalso.
apply H0.
exists x0.
intro.
right.
trivial.
Qed.

Goal (forall x, (~P x /\ Q x)) -> (forall x, P x -> Q x).
Proof.
intro.
intro.
intro.
generalize(H x);clear H;intro H.
destruct H.
contradiction.
Qed.

Goal (forall x, (P x /\ Q x)) -> (forall x, P x -> Q x).
Proof.
intros.
generalize(H x);clear H; intro H.
destruct H.
trivial.
Qed.

Goal (exists x, (~P x /\ ~Q x)) -> (exists x, (~(P x /\ Q x))).
Proof.
intro.
destruct H.
destruct H.
exists x.
intro.
destruct H1.
contradiction.
Qed.

Goal (exists x, (~P x \/ Q x)) -> (exists x, (~(P x /\ ~Q x))).
Proof.
intros.
destruct H.
destruct H.
exists x.
intro.
destruct H0.
contradiction.
exists x.
intro.
destruct H0.
contradiction.
Qed.

Goal (forall x, (P x /\ Q x)) -> forall x, P x /\ forall y, Q y.
Proof.
intros.
split.
generalize(H x); clear H; intro H.
destruct H.
trivial.
intro.
generalize(H y); clear H; intro H.
destruct H.
trivial.
Qed.

Goal (forall x, P x) \/ (forall x, Q x) -> forall x, (P x \/ Q x).
Proof.
intro.
intro.
destruct H.
left.
generalize(H x);clear H;intro H.
trivial.
right.
trivial.
Qed.

Goal (exists x, (P x /\ Q x)) -> (exists x, P x) /\ (exists x, Q x).
Proof.
intro.
split.
destruct H.
destruct H.
exists x.
trivial.
destruct H.
destruct H.
exists x.
trivial.
Qed.

Goal (exists x, P x) /\ (exists x, Q x) -> (exists x, (P x \/ Q x)).
Proof.
intros.
destruct H.
destruct H.
destruct H0.
exists x.
left.
trivial.
Qed.

Goal (forall x, forall y, (P y -> Q x)) -> ((exists y, P y) -> (forall x, Q x)).
Proof. 
intros.
destruct H0.
generalize(H x); clear H; intro H.
generalize(H x0); clear H; intro H.
apply H.
trivial.
Qed.

Goal (~forall x, ~P x) -> (exists x, P x).
Proof.
intro.
apply NNPP.
intro.
apply H.
intro.
intro.
apply H0.
exists x.
trivial.
Qed.

Goal (forall x, ~P x) -> (~exists x, P x).
Proof.
intros.
intro.
destruct H0.
generalize(H x); clear H; intro H.
contradiction.
Qed.

Goal (~exists x, P x) -> (forall x, ~P x).
Proof.
intros.
intro.
apply H.
exists x.
trivial.
Qed. 
