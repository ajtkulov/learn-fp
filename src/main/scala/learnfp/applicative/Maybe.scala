package learnfp.applicative

import learnfp.functor.Disjunction.RightDisjunction
import learnfp.functor.Maybe.Maybe
import learnfp.functor.{MaybeInstance => MaybeFunctorInstance}
import learnfp.functor.Maybe.{Just, Maybe, Nothing}


object MaybeInstance {
  import MaybeFunctorInstance._
  import learnfp.functor.FunctorOps._
  implicit val idApplicativeInstance = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] = Just(a)
    override def <*>[A, R](fx: Maybe[A => R])(a: Maybe[A]): Maybe[R] = {
      (fx, a) match {
        case (Just(ffx), Just(aa)) => Just(ffx(aa))
        case _ => Nothing[R]()
      }
    }
  }
}
