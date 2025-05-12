


case class Tactic[From, To, A](run: (A => To) => From) extends ((A => To) => From):
  override def apply(a: A => To): From = run(a)

  def map[B](fn: A => B): Tactic[From, To, B] =
    Tactic(f => run(fn andThen f))

  def flatMap[O, B](fn: A => Tactic[To, O, B]): Tactic[From, O, B] =
    Tactic(b_o => run(a => fn(a).run(b_o)))

  def foreach(fn: A => Unit): Tactic[From, To, Unit] = Tactic(f => run(fn andThen f))


