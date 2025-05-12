import =:=.Refl

object Tactics:
  def intro[A, B]: Tactic[A => B, B, A] = Tactic(identity)

  def applyF[P[_], A](f: ∀[P]): Tactic[P[A], Unit, Unit] = Tactic(_ => f.apply)

  def left[A, B]: Tactic[A \/ B, A, Unit] = Tactic(f => Left(f(())))

  def right[A, B]: Tactic[A \/ B, B, Unit] = Tactic(f => Right(f(())))

  def exact[A](a: A): Tactic[A, Unit, Unit] = Tactic(_ => a)

  def split[A, B](f: Tactic[A, Unit, Unit], g: Tactic[B, Unit, Unit]): Tactic[A /\ B, Unit, Unit] =
    Tactic(nil => (f(nil), g(nil)))

  def assert[M, A](f: Tactic[A, Unit, Unit]): Tactic[M, M, A] =
    Tactic(am => am(f(_ => ())))

  def enough[A, B](transform: A => Tactic[B, Unit, Unit]): Tactic[B, A, Unit] =
    Tactic: f =>
      val g = transform(f(()))
      g(_ => ())

  def exists[P[_], A](witness: A): Tactic[∃[A, P], P[A], Unit] =
    Tactic(proofOfPA => Exists(witness, proofOfPA(())))

  def generalize[P[_], A]: Tactic[P[A], ∀[P], Unit] = Tactic(f => f(()).apply)

  def reflexivity[A, B, C](
                            f: Tactic[A =:= B, Unit, Unit],
                            g: Tactic[B =:= C, Unit, Unit]
                          ): Tactic[A =:= C, Unit, Unit] =
    Tactic(nil => eqTrans(f(nil), g(nil)))

  def rewrite[P[_], A, B](eq: A =:= B): Tactic[P[A], P[B], Unit] = eq match
    case Refl() => apply(eqRewrite(eqSym(eq), _))

  def apply[A, B](f: A => B): Tactic[B, A, Unit] =
    Tactic(unit_a => f(unit_a(())))

  def rewriteRev[P[_], A, B](eq: A =:= B): Tactic[P[B], P[A], Unit] = eq match
    case Refl() => apply(eqRewrite(eq, _))

  def qed: Tactic[Unit, Unit, Unit] = Tactic(_ => ())