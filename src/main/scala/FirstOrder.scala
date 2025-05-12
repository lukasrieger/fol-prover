import =:=.Refl


type ∃[A, P[_]] = Exists[A, P]
type ∀[F[_]] = Forall[F]

def existsIntro[P[_], A](witness: A, proof: P[A]): Exists[A, P] = Exists(witness, proof)

def existsElim[P[_], B](
                         f: [A] => (A, P[A]) => B,
                         ex: ∃[B, P]
                       ): B = ex match
  case Exists(witness, proof) => f(witness, proof)

def forallIntro[P[_]](forall: [B] => Unit => P[B]): ∀[P] =
  new Forall:
    override def apply[A]: P[A] = forall(())

def forallElim[P[_], A](forall: ∀[P]): P[A] = forall.apply

def eqRefl[A]: A =:= A = Refl()

def eqSym[A, B](refl: A =:= B): B =:= A = refl match
  case Refl() => refl

def eqTrans[A, B, C](reflA: A =:= B, reflB: B =:= C): A =:= C = (reflA, reflB) match
  case (Refl(), Refl()) => Refl()

def eqRewrite[A, B, P[_]](refl: A =:= B, proof: P[A]): P[B] = refl match
  case Refl() => proof

trait Forall[F[_]]:
  def apply[A]: F[A]

case class Exists[A, P[_]](witness: A, proof: P[A])

enum =:=[A, B]:
  case Refl[X]() extends =:=[X, X]
