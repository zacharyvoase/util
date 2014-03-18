package com.twitter.app

import com.twitter.util._
import java.net.InetSocketAddress
import scala.collection.immutable.TreeSet
import scala.collection.mutable

/**
 * A typeclass providing evidence for parsing type `T`
 * as a flag.
 */
trait Flaggable[T] {
  def parse(s: String): T
  def show(t: T): String = t.toString
  def default: Option[T] = None
}

/**
 * Default `Flaggable` implementations.
 */
object Flaggable {
  def mandatory[T](f: String => T) = new Flaggable[T] {
    def parse(s: String) = f(s)
  }

  implicit val ofBoolean = new Flaggable[Boolean] {
    override def default = Some(true)
    def parse(s: String) = s.toBoolean
  }

  implicit val ofString = mandatory(identity)
  implicit val ofInt = mandatory(_.toInt)
  implicit val ofLong = mandatory(_.toLong)
  implicit val ofFloat = mandatory(_.toFloat)
  implicit val ofDouble = mandatory(_.toDouble)
  implicit val ofDuration = mandatory(Duration.parse(_))
  implicit val ofStorageUnit = mandatory(StorageUnit.parse(_))

  private val defaultTimeFormat = new TimeFormat("yyyy-MM-dd HH:mm:ss Z")
  implicit val ofTime = mandatory(defaultTimeFormat.parse(_))

  implicit object ofInetSocketAddress extends Flaggable[InetSocketAddress] {
    def parse(v: String) =  v.split(":") match {
      case Array("", p) =>
        new InetSocketAddress(p.toInt)
      case Array(h, p) =>
        new InetSocketAddress(h, p.toInt)
      case _ =>
        throw new IllegalArgumentException
    }

    override def show(addr: InetSocketAddress) =
      "%s:%d".format(
        Option(addr.getAddress) match {
          case Some(a) if a.isAnyLocalAddress => ""
          case _ => addr.getHostName
        },
        addr.getPort)
  }

  implicit def ofTuple[T: Flaggable, U: Flaggable] = new Flaggable[(T, U)] {
    private val tflag = implicitly[Flaggable[T]]
    private val uflag = implicitly[Flaggable[U]]

    assert(!tflag.default.isDefined)
    assert(!uflag.default.isDefined)

    def parse(v: String) = v.split(",") match {
      case Array(t, u) => (tflag.parse(t), uflag.parse(u))
      case _ => throw new IllegalArgumentException("not a 't,u'")
    }

    override def show(tup: (T, U)) = {
      val (t, u) = tup
      tflag.show(t)+","+uflag.show(u)
    }
  }

  implicit def ofSeq[T: Flaggable] = new Flaggable[Seq[T]] {
    private val flag = implicitly[Flaggable[T]]
    assert(!flag.default.isDefined)
    def parse(v: String): Seq[T] = v.split(",") map flag.parse
    override def show(seq: Seq[T]) = seq map flag.show mkString ","
  }

  implicit def ofMap[K: Flaggable, V: Flaggable] = new Flaggable[Map[K, V]] {
    private val kflag = implicitly[Flaggable[K]]
    private val vflag = implicitly[Flaggable[V]]

    assert(!kflag.default.isDefined)
    assert(!vflag.default.isDefined)

    def parse(in: String): Map[K, V] = {
      val tuples = in.split(',').foldLeft(Seq.empty[String]) {
        case (acc, s) if !s.contains('=') =>
          // In order to support comma-separated values, we concatenate
          // consecutive tokens that don't contain equals signs.
          acc.init :+ (acc.last + ',' + s)
        case (acc, s) => acc :+ s
      }

      tuples map { tup =>
        tup.split("=") match {
          case Array(k, v) => (kflag.parse(k), vflag.parse(v))
          case _ => throw new IllegalArgumentException("not a 'k=v'")
        }
      } toMap
    }

    override def show(out: Map[K, V]) = {
      out.toSeq map { case (k, v) => k.toString + "=" + v.toString } mkString(",")
    }
  }
}

case class FlagParseException(which: String, cause: Throwable)
  extends Exception(cause)
case class FlagUsageError(usage: String) extends Exception
class FlagValueRequiredException extends Exception("flag value is required")
class FlagUndefinedException extends Exception("flag undefined")

/**
 * A single flag, instantiated by a [[com.twitter.app.Flags]] instance.
 * Its current value is extracted with `apply()`.
 *
 * @see [[com.twitter.app.Flags]]
 */
class Flag[T: Flaggable] private[app](val name: String, val help: String, defaultOrUsage: Either[() => T, String]) {

  private[app] def this(name: String, help: String, default: => T) = this(name, help, Left(() => default))
  private[app] def this(name: String, help: String, usage: String) = this(name, help, Right(usage))

  protected val flaggable = implicitly[Flaggable[T]]
  @volatile private[this] var value: Option[T] = None
  protected def getValue: Option[T] = value

  private def default: Option[T] = defaultOrUsage.left.toOption map { d => d() }
  private def valueOrDefault: Option[T] = getValue orElse default

  /**
   * Return this flag's current value. The default value is returned
   * when the flag has not otherwise been set.
   */
  def apply(): T = valueOrDefault getOrElse { throw new IllegalArgumentException }

  /** Reset this flag's value */
  def reset() { value = None }
  /** True if the flag has been set */
  def isDefined = getValue.isDefined
  /** Get the value if it has been set */
  def get: Option[T] = getValue
  /** String representation of this flag's default value */
  def defaultString = flaggable.show(default getOrElse { throw new IllegalArgumentException })

  def usageString =
    defaultOrUsage match {
      case Left(_) => "  -%s='%s': %s".format(name, defaultString, help)
      case Right(usage) => "  -%s=<%s>: %s".format(name, usage, help)
    }

  /**
   * String representation of this flag in -foo='bar' format,
   * suitable for being used on the command line.
   */
  override def toString = {
    valueOrDefault match {
      case None => "-" + name + "=<unset>"
      case Some(v) => "-" + name + "='" + flaggable.show(v).replaceAll("'", "'\"'\"'") + "'"
    }
  }

  /** Parse value `raw` into this flag. */
  def parse(raw: String) {
    value = Some(flaggable.parse(raw))
  }

  /** Parse this flag with no argument. */
  def parse() {
    value = flaggable.default
  }

  def noArgumentOk = flaggable.default.isDefined
}

/**
 * A simple flags implementation. We support only two formats:
 *
 *    for flags with optional values (booleans):
 *      -flag, -flag=value
 *    for flags with required values:
 *      -flag[= ]value
 *
 * That's it. These can be parsed without ambiguity.
 *
 * There is no support for mandatory arguments: That is not what
 * flags are for.
 *
 * Flags' `apply` adds a new flag to to the flag set, so it is idiomatic
 * to assign instances of `Flags` to a singular `flag`:
 *
 * {{{
 *   val flag = new Flags("myapp")
 *   val i = flag("i", 123, "iteration count")
 * }}}
 *
 * Global flags, detached from a particular `Flags` instance, but
 * accessible to all, are defined by [[com.twitter.app.GlobalFlag]].
 */
class Flags(argv0: String, includeGlobal: Boolean) {
  private[this] trait State
  private[this] case class Parsed(remaining: Array[String]) extends State {
    def unusedArguments: Seq[String] =
      remaining.toSeq.filterNot(_ == "--").filterNot(_ == null)

    def params: Map[String, (String, Seq[Int])] = {
      val result = mutable.Map[String, (String, Seq[Int])]()
      var i = 0
      val size = remaining.size
      while (i < size) {
        val a = remaining(i)
        if (a == "--") {
          i = size
        }
        else if (isParam(a)) {
          val arr = a.tail split("=", 2)
          if (arr.size == 2)
            result += arr(0) -> ((arr(1), Seq(i)))
          else {
            if (i + 1 == size || remaining(i + 1) == null)
              result += arr(0) -> ((null, Seq(i)))
            else if (isParam(remaining(i + 1)))
              result += arr(0) -> ((null, Seq(i)))
            else {
              result += arr(0) -> ((remaining(i + 1), Seq(i, i + 1)))
              i += 1
            }
          }
        }
        i += 1
      }
      result.toMap
    }
  }
  private[this] case object Ready extends State
  private[this] case object Finished extends State

  def this(argv0: String) = this(argv0, false)

  private[this] val flags: mutable.Map[String, Flag[_]] = mutable.Map.empty

  @volatile private[this] var cmdUsage = ""

  private[this] var state: State = Ready

  private[this] def satisfy(f: Flag[_]): Unit = synchronized {
    if (flags contains f.name)
      System.err.printf("Flag %s already defined!\n", f.name)
    state match {
      case p@Parsed(remaining) =>
        flags += f.name -> f
        p.params.get(f.name).foreach { case (value, list) =>
          handleFlag(f, value)
          list.foreach(remaining(_) = null)
        }
      case _ =>
        throw new IllegalStateException("Flags can only be satisfied after parse and before finish")
    }
  }

  private[this] def isParam(f: String): Boolean = (f != null) && (f startsWith "-")

  private[this] def handleFlag(flag: Flag[_], value: String) {
    //no argument supplied
    if (value == null) {
      // optional argument
      if (flag.noArgumentOk)
        flag.parse()
      // mandatory argument
      else
        throw FlagParseException(flag.name, new FlagValueRequiredException)
    }
    else
      flag.parse(value)
  }

  private[this] def parseGlobals(map: Map[String, (String, Seq[Int])]): Seq[Int] = {
    (for ((key, (value, indices)) <- map if GlobalFlag.get(key).isDefined) yield {
      handleFlag(GlobalFlag.get(key).get, value)
      indices
    }).toSeq.flatten
  }

  /**
   * Parses arguments for GlobalFlags, and stashes the remaining parameters
   * for later use by local Flags.
   */
  def parse(args: Array[String]) {
    synchronized {
      state match {
        case Ready =>
          val remaining = args.clone()
          val p = Parsed(remaining)
          if (includeGlobal)
            parseGlobals(p.params).foreach(remaining(_) = null)
          state = p
        case Parsed(_) =>
          throw new IllegalStateException("You cannot call Flags#parse twice")
        case Finished =>
          throw new IllegalStateException("You cannot call Flags#parse after Flags#finish")
      }
    }
  }

  /**
   * Declares that no more flags will be added.
   */
  def finish(): Seq[String] = synchronized {
    state match {
      case p@Parsed(remaining) =>
        state = Finished
        p.unusedArguments
      case Ready =>
        throw new IllegalStateException("You cannot call Flags#finish before parsing")
      case Finished =>
        throw new IllegalStateException("You cannot call Flags#finish twice")
    }
  }

  def parseOrExit1(args: Array[String]): Unit =
    try parse(args) catch {
      case FlagUsageError(usage) =>
        System.err.println(usage)
        System.exit(1)
        throw new IllegalStateException
      case e@FlagParseException(k, cause) =>
        System.err.println("Error parsing flag %s: %s".format(k, cause.getMessage))
        System.err.println(usage)
        System.exit(1)
        throw new IllegalStateException
      case e =>
        System.err.println("Error parsing flags: %s".format(e.getMessage))
        System.err.println(usage)
        System.exit(1)
        throw new IllegalStateException
    }

  def apply[T: Flaggable](name: String, default: => T, help: String) = {
    val f = new Flag[T](name, help, default)
    satisfy(f)
    f
  }

  def apply[T](name: String, help: String)(implicit _f: Flaggable[T], m: Manifest[T]) = {
    val f = new Flag[T](name, help, m.toString)
    satisfy(f)
    f
  }
  
  /**
   * Set the flags' command usage; this is a message printed
   * before the flag definitions in the usage string.
   */
  def setCmdUsage(u: String) {
    cmdUsage = u
  }

  def usage: String = synchronized {
    val lines =
      for (k <- flags.keys.toArray.sorted)
      yield flags(k).usageString
    val globalLines = if (!includeGlobal) Seq.empty else {
      GlobalFlag.getAll(getClass.getClassLoader).map(_.usageString).sorted
    }
    
    val cmd = if (cmdUsage.nonEmpty) cmdUsage+"\n" else ""

    cmd+argv0+"\n"+(lines mkString "\n")+(
      if (globalLines.isEmpty) "" else "\nglobal flags:\n"+(globalLines mkString "\n")
    )
  }

  def throwUsage() {
    throw FlagUsageError(usage)
  }

  /**
   * Get all the flags known to this this Flags instance
   *
   * @param includeGlobal defaults to the includeGlobal settings of this instance
   * @param classLoader   needed to find global flags, defaults to this instance's class loader
   * @return all the flags known to this this Flags instance
   */
  def getAll(includeGlobal: Boolean = this.includeGlobal,
             classLoader: ClassLoader = this.getClass.getClassLoader): Iterable[Flag[_]] = synchronized {

    var flags = TreeSet[Flag[_]]()(Ordering.by(_.name)) ++ this.flags.valuesIterator

    if (includeGlobal) {
      flags ++= GlobalFlag.getAll(classLoader).iterator
    }

    flags
  }

  /**
   * Formats all the values of all flags known to this instance into a format suitable for logging
   *
   * @param includeGlobal see getAll above
   * @param classLoader   see getAll above
   * @return all the flag values in alphabetical order, grouped into (set, unset)
   */
  def formattedFlagValues(includeGlobal: Boolean = this.includeGlobal,
                          classLoader: ClassLoader = this.getClass.getClassLoader):
                          (Iterable[String], Iterable[String]) = {

    val (set, unset) = getAll(includeGlobal, classLoader).partition { _.get.isDefined }

    (set.map { _ + " \\" }, unset.map { _ + " \\" })
  }

  /**
   * Creates a string containing all the values of all flags known to this instance into a format suitable for logging
   *
   * @param includeGlobal set getAll above
   * @param classLoader   set getAll above
   * @return A string suitable for logging
   */
  def formattedFlagValuesString(includeGlobal: Boolean = this.includeGlobal,
                                classLoader: ClassLoader = this.getClass.getClassLoader): String = {
    val (set, unset) = formattedFlagValues(includeGlobal, classLoader)
    val lines = Seq("Set flags:") ++
      set ++
      Seq("Unset flags:") ++
      unset

    lines.mkString("\n")
  }

}

/**
 * Declare a global flag by extending this class with an object.
 *
 * {{{
 * object MyFlag extends GlobalFlag("my", "default value", "my global flag")
 * }}}
 *
 * All such global flag declarations in a given classpath are
 * visible, and are used by, [[com.twitter.app.App]].
 *
 * The name of the flag is the fully-qualified classname, for
 * example, the flag
 *
 * {{{
 * package com.twitter.server
 *
 * object port extends GlobalFlag(8080, "the TCP port to which we bind")
 * }}}
 *
 * is accessed by the name `com.twitter.server.port`.
 *
 * Global flags may also be set by Java system properties with keys
 * named in the same way, however values supplied by flags override
 * those supplied by system properties.
 *
 */
@GlobalFlagVisible
class GlobalFlag[T] private[app](defaultOrUsage: Either[() => T, String], help: String)(implicit _f: Flaggable[T])
    extends Flag[T](null, help, defaultOrUsage) {

  private[this] var accessed = false

  def this(default: T, help: String)(implicit _f: Flaggable[T]) = this(Left(() => default), help)
  def this(help: String)(implicit _f: Flaggable[T], m: Manifest[T]) = this(Right(m.toString), help)

  // Unfortunately, `getClass` in the the extends... above
  // doesn't give the right answer.
  override val name = getClass.getName.stripSuffix("$")

  protected override def getValue = synchronized {
    accessed = true
    super.getValue orElse {
      Option(System.getProperty(name)) flatMap { p =>
        try Some(flaggable.parse(p)) catch {
          case NonFatal(exc) =>
            java.util.logging.Logger.getLogger("").log(
              java.util.logging.Level.SEVERE,
              "Failed to parse system property "+name+" as flag", exc)
            None
        }
      }
    }
  }

  def getGlobalFlag: Flag[_] = this

  override def parse(arg: String): Unit = synchronized {
    if (accessed)
      throw new IllegalStateException("You cannot parse a global flag after accessing its value")

    super.parse(arg)
  }
}

private object GlobalFlag {
  def get(f: String): Option[Flag[_]] = try {
    val cls = Class.forName(f)
    val m = cls.getMethod("getGlobalFlag")
    Some(m.invoke(null).asInstanceOf[Flag[_]])
  } catch {
    case _: ClassNotFoundException
      | _: NoSuchMethodException
      | _: IllegalArgumentException => None
  }

  def getAll(loader: ClassLoader) = {
    val markerClass = classOf[GlobalFlagVisible]
    val flags = new mutable.ArrayBuffer[Flag[_]]

    for (info <- ClassPath.browse(loader)) try {
      val cls = info.load()
      if (cls.isAnnotationPresent(markerClass) && (info.name endsWith "$")) {
        get(info.name.dropRight(1)) match {
          case Some(f) => flags += f
          case None => println("failed for "+info.name)
        }
      }
    } catch {
      case _: IllegalStateException
        | _: NoClassDefFoundError
        | _: ClassNotFoundException =>
    }

    flags
  }
}
