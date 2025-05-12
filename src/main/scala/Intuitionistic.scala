

type True = Unit

type False = Nothing

type /\[A, B] = (A, B)

type \/[A, B] = Either[A, B]

type Not[A] = A => False

type <->[A, B] = (A => B) /\ (B => A)


def andIntro[A, B](a: A, b: B): A /\ B = (a, b)

def andElimLeft[A, B](ab: A /\ B): A = ab._1

def andElimRight[A, B](ab: A /\ B): B = ab._2

def orIntroLeft[A, B](a: A): A \/ B = Left(a)

def orIntroRight[A, B](b: B): A \/ B = Right(b)

def orElim[A, B, C](
                     fromA: A => C,
                     fromB: B => C,
                     or: A \/ B
                   ): C = or.fold(fromA, fromB)

def trueIntro: True = ()

def exFalso[A](f: False): A = f

def iffIntro[A, B](verse: A => B, converse: B => A): A <-> B =
  (verse, converse)

def iffElimLeft[A, B](iff: A <-> B, a: A): B = iff._1(a)

def iffElimRight[A, B](iff: A <-> B, b: B): A = iff._2(b)