package learnfp.monad

import learnfp.functor.State
import learnfp.functor.StateInstance._

object StateInstance {
  implicit def stateMonadInstance[S] = new Monad[({type E[X] = State[S, X]})#E]() {
    override def pure[A](a: A): State[S, A] = State(x => (x, a))

    override def flatMap[A, B](a: State[S, A])(fx: A => State[S, B]): State[S, B] = {

      State(s => {

        a match {
          case State(x) =>
            val z: (S, A) = x(s)
            val b: State[S, B] = fx(z._2)

            b match {
              case State(xx) =>
                xx(z._1)
            }
        }
      })
    }
  }

  implicit def stateToMonadOps[S, A](a: State[S, A]) = new MonadOps[A, ({type E[X] = State[S, X]})#E](a)
}
