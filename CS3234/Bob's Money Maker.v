Require Import Classical.
Variable (t:Type) (b:t) (P:t -> Prop). (*In his lectures, I believe Aquinas calls "t", 
"universe" but it makes little difference here.*)

Lemma Bob:
exists x,
(P x -> forall x, P x).
Proof with contradiction.
  apply NNPP.
  let x := fresh in (intro x;apply x).
  exists b.
  intros.
  apply NNPP;intro.
  apply H.
  exists x.
  intro... (*I went through the 3 dots in the workshop*)
Qed.

(*It's important to distinguish between the tricks learnt here and the bob lesson.
The bob lesson is merely to teach us that in our tool (Coq) the universe starts out
as empty, and we throw bob into it just to ensure it's not (empty).

The "tricks" on the other hand, involve getting variables above the bar (by having
an existential above or a universal below, in order to use them to prove existential
formulas or utilise universal formulas.
*)

(*Now let's look at LEM vs NNPP and ask ourselves if we really need LEM*)

Lemma Bob1:
exists x,
(P x -> forall x, P x).
Proof with contradiction.
  destruct (classic (forall x , P x)).
  exists b;intro;auto.
  apply NNPP;intro.
  apply H.
  intro.
  apply NNPP;intro.
  apply H0.
  exists x;intro.
  contradiction.
Qed.


Goal
(~ forall x, ~ P x)  <->  exists x, P x.
Proof with auto.
split.
intro.
apply NNPP.
intro.
apply H.
intro.
intro.
apply H0.
exists x;auto.
intro.
intro.
destruct H.
generalize (H0 x).
intro.
contradiction.
Qed.

Goal
(~ exists x, ~ P x)  <->  forall x, P x.
Proof with auto.
split.
intro.
intro.
apply NNPP.
intro.
apply H.
exists x;auto.
intro.
intro.
destruct H0.
generalize (H x).
intro.
contradiction.
Qed.

Goal
(~ forall x, P x)  <->  exists x, ~ P x.
Proof with auto.
split.
intro.
apply NNPP.
intro.
apply H.
intro.
apply NNPP.
intro.
apply H0.
exists x.
trivial.
intro.
intro.
destruct H.
generalize(H0 x).
intro.
contradiction.
Qed.

Goal
(~ exists x, P x)  <->  forall x, ~ P x.
Proof.
split.
intro.
intro.
intro.
apply H.
exists x;auto.
(* Note how to do below *)
intro.
intro.
destruct H0.
generalize (H x). (* Generalize -> forall elimination ?*)
intro.
contradiction.
Qed.


























