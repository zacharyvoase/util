package com.twitter.util.testing

import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import scala.language.implicitConversions

/**
 * Stub Mockito answers with plain Scala functions.
 *
 * Mix this trait into your test classes and you'll be able to stub out mock methods with Scala functions, without
 * having to write a custom `org.mockito.stubbing.Answer` subclass by hand. Note that a mismatch in arity or type
 * signature between your answer function and the actual mock method will happen when the method is called, and will
 * not be caught by the compiler.
 *
 * An example:
 *
 * {{{
 * when(myMock.method(any[String], any[Long])).thenAnswer({ (s: String, l: Long) => "foo" })
 * assert(myMock.method("key", 123L) === "foo")
 * }}}
 */
trait FunctionAnswers {
  /** Special case `Function0 -> Answer` conversion */
  implicit def fun0ToAnswer[Ret](f: => Ret): Answer[Ret] =
    new Answer[Ret] {
      def answer(invocation: InvocationOnMock): Ret = f
    }

  /** Special case `Function1 -> Answer` conversion, as `.tupled` isn't defined on [[scala.Function1]] */
  implicit def fun1ToAnswer[A : Manifest, Ret](f: A => Ret): Answer[Ret] =
    makeAnswer[Tuple1[A], Ret]({ case Tuple1(a) => f(a) })

  /** Special case `InvocationOnMock -> Tuple1` conversion, as there is no syntactic sugar for [[scala.Tuple1]] */
  def invocationToTuple1[A](inv: InvocationOnMock): Tuple1[A] = {
    val args = inv.getArguments
    Tuple1(args(0).asInstanceOf[A])
  }

  implicit def fun2ToAnswer[A: Manifest, B: Manifest, Ret](f: (A, B) => Ret): Answer[Ret] =
    makeAnswer[(A, B), Ret](f.tupled)

  def invocationToTuple2[A, B](inv: InvocationOnMock): (A, B) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B])
  }

  implicit def fun3ToAnswer[A: Manifest, B: Manifest, C: Manifest, Ret](f: (A, B, C) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C), Ret](f.tupled)

  def invocationToTuple3[A, B, C](inv: InvocationOnMock): (A, B, C) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C])
  }

  implicit def fun4ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, Ret](f: (A, B, C, D) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D), Ret](f.tupled)

  def invocationToTuple4[A, B, C, D](inv: InvocationOnMock): (A, B, C, D) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D])
  }

  implicit def fun5ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, Ret](f: (A, B, C, D, E) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E), Ret](f.tupled)

  def invocationToTuple5[A, B, C, D, E](inv: InvocationOnMock): (A, B, C, D, E) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E])
  }

  implicit def fun6ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, Ret](f: (A, B, C, D, E, F) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F), Ret](f.tupled)

  def invocationToTuple6[A, B, C, D, E, F](inv: InvocationOnMock): (A, B, C, D, E, F) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F])
  }

  implicit def fun7ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, Ret](f: (A, B, C, D, E, F, G) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G), Ret](f.tupled)

  def invocationToTuple7[A, B, C, D, E, F, G](inv: InvocationOnMock): (A, B, C, D, E, F, G) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G])
  }

  implicit def fun8ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, Ret](f: (A, B, C, D, E, F, G, H) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H), Ret](f.tupled)

  def invocationToTuple8[A, B, C, D, E, F, G, H](inv: InvocationOnMock): (A, B, C, D, E, F, G, H) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H])
  }

  implicit def fun9ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I), Ret](f.tupled)

  def invocationToTuple9[A, B, C, D, E, F, G, H, I](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I])
  }

  implicit def fun10ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J), Ret](f.tupled)

  def invocationToTuple10[A, B, C, D, E, F, G, H, I, J](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J])
  }

  implicit def fun11ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K), Ret](f.tupled)

  def invocationToTuple11[A, B, C, D, E, F, G, H, I, J, K](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K])
  }

  implicit def fun12ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L), Ret](f.tupled)

  def invocationToTuple12[A, B, C, D, E, F, G, H, I, J, K, L](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L])
  }

  implicit def fun13ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M), Ret](f.tupled)

  def invocationToTuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M])
  }

  implicit def fun14ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Ret](f.tupled)

  def invocationToTuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N])
  }

  implicit def fun15ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Ret](f.tupled)

  def invocationToTuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O])
  }

  implicit def fun16ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, P: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Ret](f.tupled)

  def invocationToTuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O], args(15).asInstanceOf[P])
  }

  implicit def fun17ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, P: Manifest, Q: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Ret](f.tupled)

  def invocationToTuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O], args(15).asInstanceOf[P], args(16).asInstanceOf[Q])
  }

  implicit def fun18ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, P: Manifest, Q: Manifest, R: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Ret](f.tupled)

  def invocationToTuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O], args(15).asInstanceOf[P], args(16).asInstanceOf[Q], args(17).asInstanceOf[R])
  }

  implicit def fun19ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, P: Manifest, Q: Manifest, R: Manifest, S: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Ret](f.tupled)

  def invocationToTuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O], args(15).asInstanceOf[P], args(16).asInstanceOf[Q], args(17).asInstanceOf[R], args(18).asInstanceOf[S])
  }

  implicit def fun20ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Ret](f.tupled)

  def invocationToTuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O], args(15).asInstanceOf[P], args(16).asInstanceOf[Q], args(17).asInstanceOf[R], args(18).asInstanceOf[S], args(19).asInstanceOf[T])
  }

  implicit def fun21ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest, U: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Ret](f.tupled)

  def invocationToTuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O], args(15).asInstanceOf[P], args(16).asInstanceOf[Q], args(17).asInstanceOf[R], args(18).asInstanceOf[S], args(19).asInstanceOf[T], args(20).asInstanceOf[U])
  }

  implicit def fun22ToAnswer[A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest, F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest, K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest, P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest, U: Manifest, V: Manifest, Ret](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Ret): Answer[Ret] =
    makeAnswer[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Ret](f.tupled)

  def invocationToTuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](inv: InvocationOnMock): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = {
    val args = inv.getArguments
    (args(0).asInstanceOf[A], args(1).asInstanceOf[B], args(2).asInstanceOf[C], args(3).asInstanceOf[D], args(4).asInstanceOf[E], args(5).asInstanceOf[F], args(6).asInstanceOf[G], args(7).asInstanceOf[H], args(8).asInstanceOf[I], args(9).asInstanceOf[J], args(10).asInstanceOf[K], args(11).asInstanceOf[L], args(12).asInstanceOf[M], args(13).asInstanceOf[N], args(14).asInstanceOf[O], args(15).asInstanceOf[P], args(16).asInstanceOf[Q], args(17).asInstanceOf[R], args(18).asInstanceOf[S], args(19).asInstanceOf[T], args(20).asInstanceOf[U], args(21).asInstanceOf[V])
  }

  private[this] def makeAnswer[Args <: Product, Ret](f: (Args => Ret))(implicit argManifest: Manifest[Args]): Answer[Ret] = {
    new Answer[Ret] {
      def answer(invocation: InvocationOnMock) = {
        val expectedArgs = argManifest.typeArguments.length
        val calledArgs = invocation.getArguments.length
        if (expectedArgs != calledArgs) {
          throw new FunctionAnswerArityMismatchException(
            "Expected %d arguments, got %d".format(expectedArgs, calledArgs))
        }
        expectedArgs match {
          case 1 => f(invocationToTuple1(invocation).asInstanceOf[Args])
          case 2 => f(invocationToTuple2(invocation).asInstanceOf[Args])
          case 3 => f(invocationToTuple3(invocation).asInstanceOf[Args])
          case 4 => f(invocationToTuple4(invocation).asInstanceOf[Args])
          case 5 => f(invocationToTuple5(invocation).asInstanceOf[Args])
          case 6 => f(invocationToTuple6(invocation).asInstanceOf[Args])
          case 7 => f(invocationToTuple7(invocation).asInstanceOf[Args])
          case 8 => f(invocationToTuple8(invocation).asInstanceOf[Args])
          case 9 => f(invocationToTuple9(invocation).asInstanceOf[Args])
          case 10 => f(invocationToTuple10(invocation).asInstanceOf[Args])
          case 11 => f(invocationToTuple11(invocation).asInstanceOf[Args])
          case 12 => f(invocationToTuple12(invocation).asInstanceOf[Args])
          case 13 => f(invocationToTuple13(invocation).asInstanceOf[Args])
          case 14 => f(invocationToTuple14(invocation).asInstanceOf[Args])
          case 15 => f(invocationToTuple15(invocation).asInstanceOf[Args])
          case 16 => f(invocationToTuple16(invocation).asInstanceOf[Args])
          case 17 => f(invocationToTuple17(invocation).asInstanceOf[Args])
          case 18 => f(invocationToTuple18(invocation).asInstanceOf[Args])
          case 19 => f(invocationToTuple19(invocation).asInstanceOf[Args])
          case 20 => f(invocationToTuple20(invocation).asInstanceOf[Args])
          case 21 => f(invocationToTuple21(invocation).asInstanceOf[Args])
          case 22 => f(invocationToTuple22(invocation).asInstanceOf[Args])
          case _ => throw new NotImplementedError("FunctionAnswers only handles up to 22-ary functions at the moment")
        }
      }
    }
  }
}

class FunctionAnswerArityMismatchException(msg: String) extends RuntimeException(msg)

