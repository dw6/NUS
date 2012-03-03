Variable term: Type.
Variable bob: term.
Variable (P :term -> Prop).


Require Import Classical.


Goal exists x, 
(P x -> forall x, P x).
Proof.
apply NNPP.
intro.
apply H.
exists bob;intro.
destruct(classic(forall x, P x)).
trivial.
intro.
apply NNPP.
intro.
apply H. exists x.
intro.
contradiction.
Qed.































