package learnfp.applicative

import learnfp.functor.Functor

abstract class Applicative[F[_]](implicit functor: Functor[F]) {
  def <*>[A, R](fx: F[A => R])(a: F[A]): F[R]

  def pure[A](a: A): F[A]

  def app[A, R](a: F[A])(fx: F[A => R]): F[R] = <*>(fx)(a)

  def ap2[A, B, C](fa: F[A], fb: F[B])(f: F[(A, B) => C]): F[C] = {
    app(fb)(app(fa)(functor.fmap(f)(_.curried)))
  }

  def ap3[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A,B,C) => D]): F[D] =
    app(fc)(app(fb)(app(fa)(functor.fmap(f)(_.curried))))

  def apply2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap2(fa, fb)(pure(f))

  def apply3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
    app(fc)(app(fb)(functor.fmap(fa)(f.curried)))

}

class FxApplicativeOps[A, R, F[_]](fx: F[A => R]) {
  def <*>(a: F[A])(implicit applicative: Applicative[F]): F[R] = applicative.<*>(fx)(a)
}

object ApplicativeOps {
  implicit def fxToApplicativeOps[A, R, F[_]](fx: F[A => R])(implicit applicative: Applicative[F]) = new FxApplicativeOps[A, R, F](fx)
}