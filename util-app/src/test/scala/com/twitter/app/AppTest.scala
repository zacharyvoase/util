package com.twitter.app

import java.lang.reflect.InvocationTargetException
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

class TestApp(f: () => Unit) extends App {
  def main() = f()
}

@RunWith(classOf[JUnitRunner])
class AppTest extends FunSuite {
  test("App: propagate underlying exception from app") {
    val throwApp = new TestApp(() => throw new RuntimeException)

    intercept[RuntimeException] {
      throwApp.main(Array.empty[String])
    }
  }

  test("App: flags should be parsed") {
    val a = new App {
      val f = flag("test", 3, "whatever")
      def main() {
        assert(f() === 4)
      }
    }
    a.main(Array("-test=4"))
  }

  test("App: flags should be parsed in the constructor") {
    val a = new App {
      val f = flag("test", 3, "whatever")
      assert(f() === 4)
      def main() {}
    }
    a.main(Array("-test=4"))
  }

  test("App: should get you help correctly") {
    val a = new App {
      val f = flag("test", 3, "whatever")
      def main() {}
    }
    val e = intercept[FlagUsageError] { a.main(Array("-help")) }
    assert(e.usage.contains("-test='3': whatever"))
  }

  test("App: should work without specifying any flags") {
    val a = new App {
      def main() {}
    }
    a.main()
  }

  test("Every constructor is respected") {
    abstract class Foo extends App {
      var x: List[Int] = Nil
      x ::= 0
    }

    class Bar extends Foo {
      x ::= 1
    }

    val bar = new Bar()
    bar.main(Array())
    assert(bar.x === List(1, 0))
  }
}
