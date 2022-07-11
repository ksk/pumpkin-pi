Require Import Coq.Program.Tactics.
Require Import List Bool.
Import Equivalence.
Require Import Coq.Classes.SetoidClass.
Require Import setoid_equivalence.

Inductive one : Type :=
  | xI : one.

Instance one_setoid : Setoid one :=
  {equiv := eq; setoid_equiv := eq_equivalence}.

Definition dep_constr_one_0 : one := xI.
Check one_rect.
Definition dep_elim_one := fun (P : one -> Type) (x : P xI) (o : one) => one_rect P x o.

Inductive two : Type :=
  | first : two
  | second : two.

Definition two_equiv (x1 x2 : two) : Prop :=
  True.

Instance two_equiv_refl : Reflexive (@two_equiv).
Proof.
  intros x. apply I.
Qed.

Instance two_equiv_sym : Symmetric (@two_equiv).
Proof.
  intros x y H. apply I.
Qed.

Instance two_equiv_trans : Transitive (@two_equiv).
Proof.
  intros x y z H1 H2. apply I.
Qed.

Instance two_equiv_equiv : Equivalence (@two_equiv).
Proof.
  split.
  - apply two_equiv_refl.
  - apply two_equiv_sym.
  - apply two_equiv_trans.
Qed.

Instance two_setoid : Setoid two :=
  {equiv := two_equiv; setoid_equiv := two_equiv_equiv}.

Definition f (x : one) : two := first.

Definition g (y : two) : one := xI.

Theorem one_two_equ : @SetoidEquivalence.setoid_equiv one one_setoid two two_setoid. 
Proof.
  apply (@SetoidEquivalence.mkEquiv one one_setoid two two_setoid f g).
  - intros. reflexivity.
  - intros. reflexivity.
  - intros. destruct a. reflexivity.
  - intros. reflexivity.
Qed.

Definition dep_constr_two_0 : two := first.
Check two_rect.
Program Definition dep_elim_two := fun (P : two -> Type) (x : P first) (t : two) => two_rect P x (_ x) t. 

Definition out_of (x : one) : nat :=
  match x with
  | xI => 0
  end.

Definition out_of' (x : two) : nat :=
  match x with
  | first => 0
  | second => 0
  end.

Definition in_to (n : nat) : one := xI.

Definition in_to' (n : nat) : two := first.

Definition both (x : one) : one := xI.

Definition both' (x : two) : two :=
  match x with
  | first => first
  | second => first
  end.
