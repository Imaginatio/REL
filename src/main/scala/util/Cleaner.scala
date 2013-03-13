package fr.splayce.rel.util

import scala.util.matching.Regex


/** Cleaner: composable `String => String` function.
 *  Its aim is to clean a `String` before parsing.
 *
 *  It may have a `TrackString => TrackString` function
 *  to track position shifts when using `TrackString`s.
 *  Its default implementation is to track nothing when
 *  the resulting `String` keeps the same length, else
 *  register a single whole-`String` replacement.
 */
case class Cleaner(val clean: String => String, val cleanTrack: TrackString => TrackString)
extends Function1[String, String] {

  def this(cleaner: Cleaner) = this(cleaner.clean, cleaner.cleanTrack)

  def this(clean: String => String) = this(clean, { in =>
    val out = clean(in.current)
    val (ilen, olen) = (in.current.length, out.length)
    in.edit(out, if (olen == ilen) TrackString.Repl() else TrackString.Repl(0, ilen, 0, olen))
  })

  def apply(in: String) = clean(in)

  def apply(in: TrackString) = cleanTrack(in)

  // function-like syntax: chain = Third(Second(First))
  def apply(previous: Cleaner) = Cleaner(clean compose previous.clean, cleanTrack compose previous.cleanTrack)
  def compose(previous: Cleaner) = this(previous)

  // Unix/pipe-like syntax: chain = First | Second | Third
  def |(then: Cleaner) = Cleaner(clean andThen then.clean, cleanTrack andThen then.cleanTrack)
  def andThen(then: Cleaner) = this | then

}


object Cleaner {

  def apply(clean: String => String) = new Cleaner(clean)

  def regexReplaceAll(re: Regex, replacement: String) = Cleaner(
    { in => re.replaceAllIn(in, replacement) },
    { in => in.replaceAll(re, replacement) })

  def regexReplaceFirst(re: Regex, replacement: String) = Cleaner(
    { in => re.replaceFirstIn(in, replacement) },
    { in => in.replaceFirst(re, replacement) })

}