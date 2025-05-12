import Proof.proof
import Tactics.*

type ->>[P[_], Q[_]] = [A] =>> P[A] => Q[A]

def orComm[A, B]: Lemma[(A \/ B) => (B \/ A)] =
  proof:
    for
      aOrB <- intro
      _ <- aOrB match
        case Left(a) =>
          for
            _ <- right
            _ <- exact(a)
          yield ()
        case Right(b) =>
          for
            _ <- left
            _ <- exact(b)
          yield ()
      q <- qed
    yield q

def composition[A, B, C]: Definition[(A => B) => (B => C) => A => C] =
  proof:
    for
      aImplB <- intro
      bImplC <- intro
      a <- intro
      _ <- apply(bImplC)
      _ <- apply(aImplB)
      r <- exact(a)
    yield r

def existsAndForallImpliesExists[A, P[_], Q[_]]: Proof[∃[A, P] => ∀[P ->> Q] => ∃[A, Q]] =
  proof:
    for
      Exists(witness, p_witness) <- intro
      forall <- intro
      _ <- exists(witness)
      _ <- apply(forall.apply)
      r <- exact(p_witness)
    yield r

