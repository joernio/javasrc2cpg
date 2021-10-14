package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.dataflowengineoss.language._

class LambdaTests extends JavaDataflowFixture {

  behavior of "Dataflow through lambdas"

  override val code: String =
    """
      |import java.util.function.Function;
      |import java.util.function.BiFunction;
      |import java.util.function.Supplier;
      |
      |public class Foo {
      |
      |    public static void printParam(String s) {
      |        Runnable r = () -> System.out.println(s);
      |        r.run();
      |    }
      |
      |    public static Runnable getParamPrinter(String s) {
      |        return () -> System.out.println(s);
      |    }
      |
      |    public static void test1() {
      |        String s = "MALICIOUS";
      |        Runnable r = () -> System.out.println(s);
      |        r.run();
      |    }
      |
      |    public static void test2() {
      |        String s = "MALICIOUS";
      |        printParam(s);
      |    }
      |
      |    public static void test3() {
      |        String s = "MALICIOUS";
      |        Runnable r = getParamPrinter(s);
      |        r.run();
      |    }
      |
      |    public static void test4() {
      |        Function<String, String> identity = (s) -> s;
      |        String s = "MALICIOUS";
      |        String t = identity.apply(s);
      |        System.out.println(t);
      |    }
      |
      |    public static void test5() {
      |        Function<String, String> safe = (s) -> s.length() > 5 ? "SAFE" : "ALSO SAFE";
      |        String s = "MALICIOUS";
      |        String t = safe.apply(s);
      |        System.out.println(t);
      |    }
      |
      |    public static void test6() {
      |        BiFunction<String, String, String> cat = (s, t) -> s + t;
      |        String s = cat.apply("MALICIOUS", "SAFE");
      |        System.out.println(s);
      |    }
      |
      |    public static void test7() {
      |        Supplier<String> badFn = () -> "MALICIOUS";
      |        String s = badFn.get();
      |        System.out.println(s);
      |    }
      |
      |    public static void test8() {
      |        Function<String, String> withPrefix = (s) -> "MALICIOUS" + s;
      |        String s = withPrefix.apply("SAFE");
      |        System.out.println(s);
      |    }
      |
      |    public static void test9() {
      |        Supplier<String> badSup = () -> "MALICIOUS";
      |        Function<String, String> withBadPrefix = (s) -> badSup.get() + s;
      |        String s = withBadPrefix.apply("SAFE");
      |        System.out.println(s);
      |    }
      |}
      |""".stripMargin

  it should "find a path through a captured local" in {
    val (source, sink) = getMultiFnSourceSink("test1", ".*")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path through a captured parameter" in {
    val (source, sink) = getMultiFnSourceSink("test2", ".*")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path through a captured parameter but executed later" in {
    val (source, sink) = getMultiFnSourceSink("test3", ".*")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path through a single lambda argument with ret val" in {
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "not find a path through a lambda returning a safe value" in {
    val (source, sink) = getConstSourceSink("test5")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path through a multi-argument lambda with a return value" in {
    val (source, sink) = getConstSourceSink("test6")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path from a malicious sink in a trivial lambda" in {
    val (source, sink) = getMultiFnSourceSink(".*", "test7")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path from a malicious sink as part of an operation in a lambda" in {
    val (source, sink) = getMultiFnSourceSink(".*", "test8")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path through nested lambdas where one is captured as a local" in {
    val (source, sink) = getMultiFnSourceSink(".*", "test9")
    sink.reachableBy(source).size shouldBe 1
  }
}
