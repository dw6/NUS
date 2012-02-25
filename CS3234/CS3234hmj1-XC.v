(*
                                     HomeJoy 1 XC - Propositional Logic

In this extrac credit assignment you will learn completely new material on
propositional logic. It is advised that you attempt your normal homework first.
It is also advised that if you are not coping well with the regular material, you
focus on that first.

CS3234 Sem 2 Ay 11/12
Sally Chak Hanrui*)

(* Benjamin Tan Wei Hao U077129N *) 

Variable p q r:Prop.
Section HB.

Axiom L1: forall (A B :Prop), A -> B -> A.
Axiom L2: forall (A B C:Prop),
(A -> B -> C) -> (A -> B) -> (A -> C).
Axiom L3: forall (A B:Prop), (~A ->B) -> (~A -> ~B) -> A.

Tactic Notation "L1" constr(x) constr(y) := generalize (L1 x y);intro.
Tactic Notation "L2" constr(x) constr(y) constr(z) := generalize (L2 x y z);intro.
Tactic Notation "L3" constr(x) constr(y) := generalize (L3 x y);intro.
Tactic Notation "MP" hyp(x) hyp(y) := generalize (y x);intro.
Tactic Notation "end" := assumption.
Tactic Notation "show" := generalize L1 L2 L3; do 3 intro.
Tactic Notation "c" := idtac "note that we have no other premisses beside our 3 axioms in this proof".
Tactic Notation "c1" := idtac "here we gave an implication as the second argument rather than a atom.".
Tactic Notation "c2" := idtac "type

show.

if you need to remind yourself of the axioms.".
Tactic Notation "c3" := idtac "For you guys, you'll just have to delete show after refreshing your memory.".

Tactic Notation "c4" := idtac  "now I use axiom L2 with the given arguments, and we notice that we're ready to use modus ponens!".
Tactic Notation "c5" := idtac "like so.".
Tactic Notation "c6" := idtac "and with that you should be getting the hang of it".
Tactic Notation "c7" := idtac "now that we're at the end, we just type

end.".



(*Extra Credit 25 points*)

(***************)
(* START HERE *)
(***************)

(*In this section we will only use custom tactics. they are

L1 <Prop> <Prop>.
L2 <Prop> <Prop> <Prop>.
L3 <Prop> <Prop>.
MP <Hyp> <Hyp>.
end.
show.



axiom L1: forall (A B :Prop), A -> B -> A.
axiom L2: forall (A B C:Prop),
(A -> B -> C) -> (A -> B) -> (A -> C).
axiom L3: forall (A B:Prop), (~A ->B) -> (~A -> ~B) -> A.


Look at the 3 axioms above. The tactic L1 corresponds to  generalizing the axiom
L1 with the 2 arguments: Hence

                  L1 P Q.

gives the arguments P and Q to

                 forall (A B :Prop), A -> B -> A.

which will place P -> Q -> P above the bar.
If you have 2 hypotheses

H: P
H0 : P -> Q.

you can type

         MP H H0.  (the antecedent, followed by the implication)

to get Q above the bar.
The solver we will use for this homework is "end." (i.e. instead of typing trivial, type end)

You will always have these axioms as premisses in addtion to whatever premisses
are explicit in the proof environment.
*)



(*Example*)
Lemma Example:
 p -> p.
Proof.  c.

L1 p (p -> p).

(*watch the lower right window!*) c1. c2.

show.

clear -H. c3.  c4.

L2 p (p->p) p.  c5.

MP H H0.   c6.
L1 p p.
MP H2 H1.   c7.
end.
Qed.


(*Instructions: Do not use the normal tactics we learn in class.
Do not alter the proof script outside of the comments.
*)


(*5 points*)
Lemma Prob1:
(p -> q) -> (q -> r) -> p -> q -> r.
Proof.
do 2 intro.

(*Your proof starts here*)
show.
L1 (q->r) (p).
MP H0 H4.
end.
(*Your proof ends here*)
Qed.

(*10 points*)
Lemma Prob2:
(p -> q) -> (q -> r) -> p -> r.
Proof.
do 2 intro.
show.
L1 (q->r) p.
MP H0 H4.
L2 p q r.
MP H5 H6.
MP H H7.
end.
Qed.

(*10 points*)
Lemma Prob3:
p -> ~p -> q.
Proof.
do 2 intro.
show.
L1 (p) (~q).
MP H H4.
L3 q (p).
MP H5 H6.
L1 (~p) (~q).
MP H0 H8.
MP H9 H7.
end.
Qed.