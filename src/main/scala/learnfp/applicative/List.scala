package learnfp.applicative

object ListInstance {

  import learnfp.functor.ListInstance._

  implicit def appListInstance = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def <*>[A, R](fxs: List[A => R])(as: List[A]): List[R] = fxs match {
      case x :: xs => as.map(x) ++ <*>(xs)(as)
      case Nil => Nil
    }
  }
}
