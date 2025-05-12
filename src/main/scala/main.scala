

type Taut = [X] =>> True

@main
def main(): Unit = {
  val x = runProof(existsAndForallImpliesExists[Int, Taut, Taut])
  val y = x(
    Exists(23, (_: Int) => ())
  )(
    new Forall[Taut ->> Taut]:
      override def apply[A]: (Taut ->> Taut)[A] = (_ => ())
  )
  println(
    y
  )
}