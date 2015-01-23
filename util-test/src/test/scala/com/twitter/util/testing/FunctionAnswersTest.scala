package com.twitter.util.testing

import org.junit.runner.RunWith
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar

@RunWith(classOf[JUnitRunner])
class FunctionAnswersTest extends FunSuite with MockitoSugar with FunctionAnswers {
  class MockSubject {
    def method0(): Long = 0L
    def method1(arg1: String): Long = 0L
    def method2(arg1: String, arg2: Long): Long = 0L
    def method5(arg1: String, arg2: Long, arg3: Int, arg4: Seq[Int], arg5: Boolean): Long = 0L
  }

  test("FunctionAnswers should raise an exception at the call site on arity mismatch") {
    val subject = mock[MockSubject]
    when(subject.method0()).thenAnswer({ s: String => 123L })
    intercept[FunctionAnswerArityMismatchException] { subject.method0() }
  }

  test("FunctionAnswers should raise an exception at the call site on type mismatch") {
    val subject = mock[MockSubject]
    when(subject.method1(any[String])).thenAnswer({ l: Long => l })
    intercept[java.lang.ClassCastException] { subject.method1("foo") }
  }

  test("FunctionAnswers should be able to convert a Function0 into an Answer") {
    val subject = mock[MockSubject]
    when(subject.method0()).thenAnswer({ 456L })
    assert(subject.method0() === 456L)
  }

  test("FunctionAnswers should be able to convert a Function1 into an Answer") {
    val subject = mock[MockSubject]
    when(subject.method1(any[String])).thenAnswer({ string: String => string.length.toLong })
    assert(subject.method1("foo") === 3L)
  }

  test("FunctionAnswers should be able to convert a Function2 into an Answer") {
    val subject = mock[MockSubject]
    when(subject.method2(any[String], any[Long])).thenAnswer({ (s: String, n: Long) =>
      s.length * n
    })
    assert(subject.method2("foo", 2L) === 6L)
  }

  test("FunctionAnswers should be able to convert a Function5 into an Answer") {
    val subject = mock[MockSubject]
    when(subject.method5(any[String], any[Long], any[Int], any[Seq[Int]], any[Boolean])).thenAnswer({
      (s: String, n1: Long, n2: Int, numbers: Seq[Int], flag: Boolean) =>
        if (flag) { (s.length * (n1 + n2)) + numbers.sum }
        else { 12L }
    })
    assert(subject.method5("foo", 2, 5, Seq(4, 9), arg5 = true) === 34L)
  }
}

