package learnfp.monad

import learnfp.functor.Writer
import learnfp.functor.WriterInstance._
import learnfp.monoid.Monoid
import learnfp.monoid.MonoidOps._

object WriterInstance {
  implicit def writerMonadInstance[W](implicit monoid: Monoid[W]) = new Monad[({type E[X] = Writer[W, X]})#E] {
    override def pure[A](a: A): Writer[W, A] = Writer(() => (monoid.mzero, a))

    override def flatMap[A, B](a: Writer[W, A])(fx: A => Writer[W, B]): Writer[W, B] = {
      Writer(() =>
        a match {
          case Writer(x) =>
            val first: (W, A) = x()
            val flat: Writer[W, B] = fx(first._2)
            flat match {
              case Writer(xx) =>
                val second: (W, B) = xx()

                (monoid.mappend(first._1, second._1), second._2)
            }
        }
      )
    }
  }

  implicit def writerToMonadOps[W, A](w: Writer[W, A])(implicit monoid: Monoid[W]) = new MonadOps[A, ({type E[X] = Writer[W, X]})#E](w)
}
