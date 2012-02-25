(* For 2 points, include your name and matric no.
Name: Benjamin Tan Wei Hao
Matric No. : U077129N
*)

(*
                                     HomeJoy 1 - Propositional Logic

CS3234 Sem 2 Ay 11/12
This homejoy is biased towards those who will work smart rather than work hard.
Though working hard can't hurt.
 - Sally Chak Hanrui

Instructions:
    All of the following problems are provable; please replace the "admit" 
    tactics below with other tactics.

    The only solvers you should use are, exact, assumption and trivial. More specifically
    Here is a list of explicitly banned tactics:
      auto
      admit
      firstorder
      tauto
      intuition

Also banned are any other tactics not mentioned that do copious amounts of work (logically).
*)

Variables a b c d e f g k p q r x y z:Prop.




Module A_Little_Class.
Require Import Classical.


(*Remember how to use "generalize" and classic
and Peirce etc. Look up old the labs if you have to.
This URL may also be handy.

http://coq.inria.fr/stdlib/Coq.Logic.Classical_Prop.html

*)

(*Let's warm up for 3 points*)
Lemma I_am_not_afraid_of_symbols:
  ((p \/ q) /\ (~p \/ ~q)) -> ((p /\ ~q) \/ (~p /\ q)).
Proof.
  intros.
  destruct H.
  destruct H.
  destruct H0.
  contradiction.
  left.
  split;trivial.
  destruct H0.
  right.
  split;trivial.
  contradiction.
Qed.


Lemma Ok_maybe_a_little_afraid_of_symbols:
(~p -> a) ->
(p \/ a -> b) ->
(~c -> ~b) ->
(~c \/ (x \/ y)) ->
(x -> z) ->
(y \/ z) /\ (~p \/ b) /\ (~p -> b).
Proof.
intros.
split.
apply NNPP.
intro.
destruct H2.
Focus 2.
destruct H2.
Focus 2.
apply H4.
left;trivial.
apply H4.
right.
apply H3;trivial.
(* unfold not in *. *)
apply H1.
trivial.
apply H0.
apply NNPP.
intro.
apply H5.
right.
apply H.
intro.
apply H5.
left;trivial.
split.
right.
apply H0.
apply NNPP.
intro.
apply H4.
right.
apply H.
intro.
apply H4.
left.
trivial.
intro.
apply H0.
apply NNPP.
intro.
apply H5.
right.
apply H.
trivial.
Qed.

(*For 5, lets revise DeMorgans.
Yes, 5 for both, because we've done them before*)

Lemma DeMorgan's_and:
~(p /\ q) <-> (~p \/ ~q).
Proof.
  intros.
  split.
  intro.
  destruct(classic(q)).
  left.
  intro.
  apply H.
  split;trivial.
  right;trivial.
  intro.
  destruct H.
  intro.
  apply H.
  destruct H0.
  contradiction.
  intro.
  destruct H0.
  contradiction.
Qed.

Lemma DeMorgan's_or:
~(p \/ q) <-> (~p /\ ~q).
Proof.
  intros.
  split.
  intro.
  split.
  intro.
  apply H.
  left.
  trivial.
  intro.
  apply H.
  right.
  trivial.
  intro.
  destruct H.
  intro.
  destruct H1.
  contradiction.
  contradiction.
Qed.




(*For 10 points, prove LEM with Peirce's law. Using NNPP or LEM/classic will
net you 0 points for this problem*)

Lemma Prob4:
p \/ ~p.
Proof.
generalize Peirce. (* Whats the use of generalize here? *)
intro PL.
apply Peirce.
intros.
right.
intro.
apply H.
left.
trivial.
Qed.
End A_Little_Class.

(*      Beginning Intuitionistic Logic
While it is much much more, let us assume for now that Int Logic is just
Classical logic - NNPP - LEM - Peirce

You will find that you no longer have access to these rules.
Using any of them for the following questions will result in
0 points for that question.
*)




(*Problem 5, 10 points*)
(*Now, try and decide which DeMorgan's law is intuitionictically valid and provide
a intuitionistic proof for it. i.e. no NNPP, LEM/classic or Peirce.. *)
Goal ~(p /\ q) <-> (~p \/ ~q).
Proof.
admit.
Qed.

(*or*)

Goal ~(p \/ q) <-> (~p /\ ~q).
Proof.
  intros.
  split.
  intro.
  split.
  intro.
  apply H.
  left.
  trivial.
  intro.
  apply H.
  right.
  trivial.
  intro.
  destruct H.
  intro.
  destruct H1.
  contradiction.
  contradiction.
Qed.

(*5 points for a proof, 5 points for an explanation of why this is not double
negation elimination*)
Lemma Prob6:
~~~p -> ~p.
Proof.
  intros.
  intro.
  apply H.
  intro.
  contradiction.
Qed.





(*Your explanation goes here:

Double negation elimination requires a proof of "p".
On the other hand, ~~~p -> ~p only requires that p is not contradictory.
*)


(*10*)
Lemma Prob7:
(~~(p/\ q)) <->(~~ p /\~~ q).
Proof.
  intros.
  split.
  intro.
  split.
  intro.
  apply H.
  intro.
  destruct H1.
  contradiction.
  intro.
  apply H.
  intro.
  destruct H1.
  contradiction.
  intro.
  intro.
  destruct H.
  apply H.
  intro.
  apply H1.
  intro.
  apply H0.
  split;trivial.
Qed.

(*10*)
Lemma Prob8:
~(p /\ q) <-> ~~(~p \/ ~q).
Proof.
  intros.
  split.
  intro.
  intro.
  apply H0.
  left.
  intro.
  apply H0.
  right.
  intro.
  apply H.
  split;trivial.
  intro.
  intro.
  apply H.
  intro.
  destruct H0.
  destruct H1.
  contradiction.
  contradiction.
Qed.


Section Hey.
(*15*)
Axiom Hey: forall P Q: Prop,    ~(P /\ Q) -> (~P \/ ~Q).

Lemma Prob9:
~~(p \/ q) <-> ~~p \/ ~~q.
Proof.
  intros.
  split.
  intro.
  apply Hey.
  intro.
  destruct H.
  destruct H0.
  intro.
  destruct H1.
  contradiction.
  contradiction.
  intro.
  destruct H.
  intro.
  apply H.
  intro.
  apply H0.
  left;trivial.
  intro.
  apply H.
  intro.
  apply H0.
  right;trivial.
Qed.

End Hey.


Section Ting.
(*20*)
Axiom Ting: forall P Q: Prop,     ~~(P \/ Q) -> ~~P \/ ~~Q.

Lemma Prob10:
~(p /\ q) <-> (~p \/ ~q).
Proof.
  split.
  intro.
  assert ((~~~p \/ ~~~q) -> ~p \/ ~q).
  intro.
  destruct H0.
  left.
  intro.
  apply H0.
  intro.
  contradiction.
  right.
  intro.
  apply H0.
  intro.
  contradiction.
  apply H0.
  apply Ting.
  intro.
  apply H1.
  left.
  intro.
  apply H1.
  right.
  intro.
  apply H.
  split;trivial.
(* Proof for second part 
*)  
  intro.
  intro.
  destruct H.
  destruct H0.
  contradiction.
  destruct H0.
  contradiction.
Qed.

End Ting.