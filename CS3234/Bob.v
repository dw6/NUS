Variable term: Type.
Variable bob: term.
Variable (P :term -> Prop).


Require Import Classical.


Goal exists x, 
(P x -> forall x, P x).
Proof.
apply NNPP. (* I want to throw the bottom to above the bar *)
intro.
apply H.
exists bob;intro.
intro.
destruct(classic(forall x, P x)).
trivial.
apply NNPP.
intro.
apply H.
exists x;intro.
contradiction.
(* apply NNPP.
intro.
apply H.
destruct(classic(forall x, P x)). (* LEM *)
exists bob;intro.
trivial. (* exact H0. *)
exists bob;intro.
intro.
apply NNPP.
intro.
apply H.
exists x;intro.
contradiction.
*)
Qed.


































