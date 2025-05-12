

type Lemma[A] = Proof[A]
type Corollary[A] = Proof[A]
type Example[A] = Proof[A]
type Definition[A] = Proof[A]

def runProof[A](proof: Proof[A]): A = proof.of(_ => ())

def admitted[A]: Axiom[A] = ???

def noncomputable[A]: Axiom[A] = ???

trait Axiom[A]

trait TacticScope[A]

case class Proof[A](of: Tactic[A, Unit, Unit])

object Proof:
  def apply[A](fn: () => Tactic[A, Unit, Unit]) = new Proof(fn())

  def proof[A](fn: TacticScope[A] ?=> Tactic[A, Unit, Unit]): Proof[A] =
    given scope: TacticScope[A] = new TacticScope[A] {}

    Proof(fn)