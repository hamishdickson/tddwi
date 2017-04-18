AdderTypeInt : (numargs : Nat) -> Type
AdderTypeInt Z = Int
AdderTypeInt (S k) = (next : Int) -> AdderTypeInt k

adder1 : (numargs : Nat) -> (acc : Int) -> AdderTypeInt numargs
adder1 Z acc = acc
adder1 (S k) acc = \next => adder1 k (next + acc)

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType => (numargs : Nat) -> numType -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
