<%!
TYPE_VARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
MAX_ARITY = 22
%>\
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

% for i in xrange(2, MAX_ARITY + 1):
<%
  types = ', '.join(TYPE_VARS[:i])
  types_with_manifests = ', '.join('{}: Manifest'.format(t) for t in TYPE_VARS[:i])
  coerce_args = ', '.join(
    'args({}).asInstanceOf[{}]'.format(j, TYPE_VARS[j]) for j in xrange(i))
%>\
  implicit def fun${i}ToAnswer[${types_with_manifests}, Ret](f: (${types}) => Ret): Answer[Ret] =
    makeAnswer[(${types}), Ret](f.tupled)

  def invocationToTuple${i}[${types}](inv: InvocationOnMock): (${types}) = {
    val args = inv.getArguments
    (${coerce_args})
  }

% endfor
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
          % for i in xrange(1, MAX_ARITY + 1):
          case ${i} => f(invocationToTuple${i}(invocation).asInstanceOf[Args])
          % endfor
          case _ => throw new NotImplementedError("FunctionAnswers only handles up to ${MAX_ARITY}-ary functions at the moment")
        }
      }
    }
  }
}

class FunctionAnswerArityMismatchException(msg: String) extends RuntimeException(msg)
