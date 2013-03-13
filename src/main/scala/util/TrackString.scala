package fr.splayce.rel.util

import collection.mutable.ListBuffer
import util.matching.Regex
import Regex.Match

import TrackString._


/** A String that can keep track of the shifts in position when edited.
 *  One can obtain the `Interval` in the source String corresponding to
 *  any `Interval` in the edited version.
 *
 *  @param source  The original String
 *  @param current The current edited String
 *  @param repl    The shift tracking information
 *  @note Cannot track reordering: `abc` => `bca` will be treated as a single replacement operation,
 *        meaning that when asked for the original position of `c`, it will return an `Interval`
 *        corresponding to `abc` in the source String.
 *  @note Does not support Java 7+ named groups, regex-replace operations use Scala named groups.
 */
case class TrackString(val source: String, val current: String, val repl: Repl = Repl()) {

  /** The current String representation */
  override val toString = current

  /** The Interval in the source that corresponds to the given Interval in the current String. */
  def srcPos(start: Int, end: Int): Interval = srcPos(Interval(start, end))
  /** The Interval in the source that corresponds to the given Match in the current String. */
  def srcPos(m: Match): Interval = srcPos(m.start, m.end)
  /** The Interval in the source that corresponds to the given Interval in the current String. */
  def srcPos(interval: Interval): Interval = {
    require(interval.start < current.length,
      "interval " + interval + " is out of bounds of \"" + current + "\"=[0," + current.length + "[")
    repl.srcPos(interval)
  }
  /** The Interval in the source that corresponds to the given position in the current String. */
  def srcPos(pos: Int): TrackString.Interval = {
    require(pos >= 0 && pos < current.length,
      "pos " + pos + " is out of bounds of \"" + current + "\"=[0," + current.length + "[")
    repl.srcPos(pos)
  }

  /** Build a new TrackString with an additional edit.
   *
   *  @param to   The new edited String
   *  @param repl The tracking information for this edit
   */
  def edit(to: String, repl: Repl): TrackString = TrackString(source, to, this.repl + repl)
  /** Build a new TrackString with an additional edit.
   *
   *  @param editOp The edit operation: edited String + tracking info.
   */
  def edit(editOp: (String, Repl)): TrackString = edit(editOp._1, editOp._2)
  /** Build a new TrackString with an additional custom edit.
   *  The `replacer` method is responsible for maintaining a resulting `Repl`
   *  by adding the corresponding `Subst`s to it.
   *
   *  @param replacer The edit operation: edited String + tracking info.
   */
  def edit(replacer: String => (String, Repl)): TrackString = edit(replacer(current))


  /** Edit with a per-Char replacer.
   *
   *  @param replacer The replacement operation for each Char.
   */
  def replaceAll(replacer: Char => Option[String]): TrackString = edit(perCharReplacer(replacer))
  /** Edit with a Regex replacement.
   *  Each effective replacement (match whose replacement is different from the matched String)
   *  will append a corresponding `Subst` to the resulting `Repl`.
   *
   *  @param regex       The regex
   *  @param replacement The replacement String
   *  @see scala.util.matching.Regex#replaceAllIn
   */
  def replaceAll(regex: Regex, replacement: CharSequence): TrackString =
    replaceAll(regex, TrackString.defaultReplacer(replacement))
  /** Edit with multiple Regex replacements.
   *  Each effective replacement (match whose replacement is different from the matched String)
   *  will append a corresponding `Subst` to the resulting `Repl`.
   *
   *  @param steps The regex/replacement pairs
   *  @see scala.util.matching.Regex#replaceAllIn
   */
  def replaceAll(steps: (Regex, CharSequence)*): TrackString =
    (this /: steps) { (as, r) => as.replaceAll(r._1, r._2) }
  /** Edit with a Regex replacement.
   *  The `replacer` method is responsible for maintaining a resulting `Repl`
   *  by adding the corresponding `Subst`s to it.
   *
   *  @param regex    The regex
   *  @param replacer The replacement method
   *  @see scala.util.matching.Regex#replaceAllIn
   */
  def replaceAll(regex: Regex, replacer: (Match, Repl) => (CharSequence, Repl)): TrackString = {
    var newRepl = Repl()
    val result = regex.replaceAllIn(current, aMatch => {
      val result = replacer(aMatch, newRepl)
      newRepl = result._2
      result._1.toString
    })
    edit(result, newRepl)
  }

  /** Edit with a Regex replacement (regex is matched only once).
   *  The `replacer` method is responsible for maintaining a resulting `Repl`
   *  by adding the corresponding `Subst`(s) to it.
   *
   *  @param regex    The regex
   *  @param replacer The replacement method
   *  @see scala.util.matching.Regex#replaceFirstIn
   */
  def replaceFirst(regex: Regex, replacer: (Match, Repl) => (CharSequence, Repl)): TrackString =
    regex.findFirstMatchIn(current) map { m =>
      val r = replacer(m, Repl())
      edit(current.subSequence(0, m.start) + r._1.toString + current.subSequence(m.end, current.length), r._2)
    } getOrElse this

  /** Edit with a Regex replacement (regex is matched only once).
   *  Each effective replacement (match whose replacement is different from the matched String)
   *  will append a corresponding `Subst` to the resulting `Repl`.
   *
   *  @param regex       The regex
   *  @param replacement The replacement String
   *  @see scala.util.matching.Regex#replaceFirstIn
   */
  def replaceFirst(regex: Regex, replacement: CharSequence): TrackString =
    replaceFirst(regex, TrackString.defaultReplacer(replacement))

}

object TrackString {

  /** `true` if current Java implementation supports inline-named capturing groups in regex Pattern.
   *  Should be `true` for Java >= 7, false otherwise.
   *
   *  @see java.util.regex.Pattern
   */
  val supportsNamedGroup: Boolean = try {
    "(?<g>y)".r.replaceFirstIn("y", "${g}") == "y"
  } catch {
    case iae: IllegalArgumentException => false
  }

  def apply(source: String): TrackString = TrackString(source, source)

  /** Produces a replacer function for a regex-replace-compatible replacement `CharSequence`.
   *  Each effective replacement (match whose replacement is different from the matched String)
   *  will append a corresponding `Subst` to the resulting `Repl`.
   *
   *  @see scala.util.matching.Regex#replaceFirstIn
   */
  def defaultReplacer(replacement: CharSequence, useNamedGroup: Boolean = supportsNamedGroup): (Match, Repl) => (String, Repl) = {
    val replacer = regexReplacer(replacement, useNamedGroup)

    { (m, repl) =>
      val replaced = replacer(m)
      val track =
        if (replaced == m.matched) repl
        else repl.+(m.start, m.end - m.start, replaced.length)
      (replaced, track)
    }
  }

  /** Produces a replacer function for a char-per-char replacement.
   *  Whenever `replacer` yields `Some(str)`, `str` will replace the input Char
   *  in the resulting edit, maintaining the `Repl` tracking info.
   */
  def perCharReplacer(replacer: Char => Option[String]): String => (String, Repl) = s => {
    var repl = Repl()
    val result = new StringBuilder
    for {
      (c, pos) <- s.zipWithIndex
      val tco = replacer(c)
      val tc  = tco.getOrElse(c.toString)
      val lag = tc.length != 1
    } {
      if (lag) {
        repl = repl.+(pos, 1, tc.length)
      }
      result.append(tc)
    }
    (result.toString, repl)
  }

  /** Preprocess a regex replacement template, providing `Match => String` replacer.
    *
    *  @param template      Replacement String template
    *  @param useNamedGroup If true, will try to parse named groups in template
    *  @return              A function yielding a consolidated replacement String for a given `Regex.Match`
    *  @see java.util.regex.Matcher#replaceAll(java.lang.String) for replacement template syntax
    *  @note In current implementation, differs from Java's regex-replace, in that it replaces named
    *        groups with Scala's Regex named groups, which are not Java's.
    *  @see scala.util.matching.Regex#groupNames
    */
  def regexReplacer(template: CharSequence, useNamedGroup: Boolean = supportsNamedGroup): Match => String = {
    val length = template.length
    val pieces = new ListBuffer[Match => String]
    var cur = 0

    while (cur < length) {
      template.charAt(cur) match {
        case '$' =>
          cur += 1
          require(cur < length, "Empty group reference")
          template.charAt(cur) match {

            case '{' =>
              require(useNamedGroup, "Named group references not supported")
              cur += 1
              var subCur = cur
              var stop = false
              while (!stop && subCur < length) {
                stop = template.charAt(subCur) == '}'
                subCur += 1
              }
              require(stop, "Unterminated named group reference")
              require(subCur > cur + 1, "Empty named group reference")
              val gn = template.subSequence(cur, subCur - 1).toString
              require("[a-zA-Z][a-zA-Z0-9]*".r.pattern.matcher(gn).matches(), "Invalid named group reference")
              pieces += { m =>
                require(m.groupNames.contains(gn), "No group with name {" + gn + "}")
                m.group(gn) // FIXME should fetch Java 7 named group
              }
              cur = subCur

            case '0' =>
              pieces += { m => m.matched }
              cur += 1
            case c if c.isDigit =>
              var subCur = cur + 1
              while (subCur < length && template.charAt(subCur).isDigit) {
                subCur += 1
              }
              val gn = Integer.parseInt(template.subSequence(cur, subCur).toString, 10)
              pieces += { m =>
                var suffix = ""
                var n = gn
                while (n > m.groupCount) {
                  suffix = (n % 10).toString + suffix
                  n = n / 10
                }
                require(n > 0, "Invalid group reference")
                m.group(n) + suffix
              }
              cur = subCur

            case _ => throw new IllegalArgumentException("Invalid group reference, literal $ should be escaped")
          }

        case '\\' =>
          require(cur + 1 < length, "Truncated escape sequence")
          val piece = template.charAt(cur + 1).toString
          pieces += { m => piece }
          cur += 2

        case c =>
          var subCur = cur + 1
          while (subCur < length && template.charAt(subCur) != '\\' && template.charAt(subCur) != '$') {
            subCur += 1
          }
          val piece = template.subSequence(cur, subCur).toString
          pieces += { m => piece }
          cur = subCur
      }
    }

    pieces.toList match { // OPTIMIZE merge adjacent literal parts
      case Nil       => { m => "" }
      case f1 :: Nil => f1
      case fs        => { m => fs.map(_(m)).mkString }
    }
  }


  /** Interval: left-inclusive, right-exclusive (priority).
   *  Implements [[http://en.wikipedia.org/wiki/Allen%27s_interval_algebra Allen's interval algebra]].
   *  Natural integers only, since it corresponds to indices in Strings.
   *
   *  @param start Beginning of the interval, inclusive.
   *  @param end   End of the interval, exclusive (has priority, i.e. `[0, 0)` is considered empty).
   */
  case class Interval(val start: Int, val end: Int) {
    require(start >= 0, "Only intervals with positive starting position are valid in this context (start = " + start + ")")
    require(end >= start, "Starting position " + start + " cannot be greater than ending position " + end)

    override lazy val toString: String = "[%d,%d)".format(start, end)
    val isEmpty: Boolean = start == end

    def before  (that: Interval): Boolean = end < that.start
    def meets   (that: Interval): Boolean = end == that.start
    def overlaps(that: Interval): Boolean = start < that.start &&
                                              end > that.start &&
                                              end < that.end
    def starts  (that: Interval): Boolean = start == that.start && end < that.end
    def during  (that: Interval): Boolean = start > that.start && end < that.end
    def finishes(that: Interval): Boolean = start > that.start && end == that.end
    def contains(that: Interval): Boolean = start <= that.start && end >= that.end

    def < (that: Interval) =  this before that
    def > (that: Interval) =  that before this

    def >>(shift: Int): Interval = Interval(start + shift, end + shift)
  }


  /** A Substitution from an Interval in the original String to an Interval in the new String. */
  case class Subst(val from: Interval, val to: Interval) {
    override lazy val toString: String = "{%s -> %s}".format(from, to)

    /** Build a new `Subst`, carving `this` if it crosses the boundaries
     *  of `by` (additional edit), or simply shift or absorb it otherwise.
     *
     *  For example, a simple shift happens when:
     *  {{{
     *  this = [5,7) -> [5,8)
     *  by   = [1,4) -> [1,2)
     *  this.to is after by.from
     *  thus, shift this.to, by by.to.end - by.from.end = -2
     *  this.to will be [5 - 2, 8 - 2) = [3, 6)
     *  and there is no unprocessed part
     *  }}}
     *
     *  But with a border crossing, it becomes:
     *  {{{
     *  this = [1,6) -> [1,8)
     *  by   = [3,5) -> [3,7)
     *  this.to overlaps by.from
     *  thus, this.to will be cut (and its right piece, shifted by 2):
     *  - unprocessed yet: [1,6) -> [1,3)
     *  - (in the middle is by.from)
     *  - processed: [1,6) -> [5 + 2, 8 + 2)
     *  }}}
     *
     *  @return (the unprocessed part of `this`, the processed part of `this`)
     */
    protected[util] def carved(by: Subst): (Option[Subst], Option[Subst]) = {
      val (bFrom, bTo) = (by.from, by.to)
      val shift = bTo.end - bFrom.end

      // this (current Subst) is ...
      if ((bFrom before to) || (bFrom meets to))                   // after, doesn't cross
        (None, Some(Subst(from, to >> shift)))                     //   => just shift this.to
      else if ((bFrom overlaps to) || (bFrom starts to))           // larger at right side only
        (None, Some(Subst(from, Interval(bFrom.end, to.end) >> shift))) // => cut & shift right piece
      else if (bFrom during to)                                    // larger at both sides
        (Some(Subst(from, Interval(to.start, bFrom.start))),       //   => cut left piece to process
          Some(Subst(from, Interval(bFrom.end, to.end) >> shift))) //   => cut & shift right piece
      else if ((bFrom finishes to) || (to overlaps bFrom))         // larger at left side only
        (Some(Subst(from, Interval(to.start, bFrom.start))), None) //  => cut left piece to process
      else if ((to before bFrom) || (to meets bFrom))              // before, doesn't cross
        (Some(this), None)                                         //   => whole that to process
      else if (bFrom contains to)                                  // inside
        (None, None)                                               //   => absorbed in this
      else throw new RuntimeException("Unexpected interval carving situation: " + this + " carve " + by)
    }

    /** Build a new `Subst`, carving it according to the given additional `Subst`s.
     *
     *  @param by remaining additional `Subst`s when `this` is reached
     *  @return   (carved pieces, the tail part of `by` that is still relevant to the carving of the following `Subst`s)
     */
    protected[util] def carved(by: List[Subst]): (List[Subst], List[Subst]) = {
      val carved = new ListBuffer[Subst]
      var toCarve: Option[Subst] = Some(this)
      var remainingBy: List[Subst] = by
      var lateRemainingBy: List[Subst] = by

      while (toCarve.isDefined && !remainingBy.isEmpty) {
        val (todo, done) = toCarve.get.carved(remainingBy.head)
        done.foreach(carved += _)
        lateRemainingBy = remainingBy
        remainingBy = remainingBy.tail
        toCarve = todo
      }

      toCarve.foreach(carved += _)
      (carved.toList, lateRemainingBy)
    }
  }

  object Subst {

    def apply(startFrom: Int, endFrom: Int, startTo: Int, endTo: Int): Subst =
      Subst(Interval(startFrom, endFrom), Interval(startTo, endTo))

    implicit def from(coords: (Int, Int, Int, Int)): Subst =
      apply(coords._1, coords._2, coords._3, coords._4)

  }

  /** Holder for position tracking information across edits.
   *
   *  @param substs Accumulated individual substitutions, sorted by **descending** position
   */
  case class Repl(val substs: List[Subst] = Nil) {

    override lazy val toString: String = substs.mkString("[", ", ", "]")

    /** Current shift in position accumulated by the underlying `Subst`s. */
    lazy val currentShift: Int = if (substs.isEmpty) 0 else substs.head.to.end - substs.head.from.end

    /** Builds a new `Repl` with an added `Subst`.
     *
     *  @param subst The substitution to append
     */
    def +(subst: Subst): Repl = {
      if (substs.isEmpty) {
        require(subst.to.start - subst.from.start == 0,
                "no previous substs, subst's from and to should be aligned")
      } else {
        val head = substs.head
        require(subst.to.start >= head.to.end,
                "substs must be append in order in 'to' point of view")
        require(subst.from.start >= head.from.start && subst.from.end >= head.from.end,
                "substs must be append in order in 'from' point of view")
        require(subst.to.start - subst.from.start == head.to.end - head.from.end,
                "start of subst " + subst + " must be aligned with end of " + head)
      }
      Repl(subst :: substs)
    }
    /** Builds a new `Repl` with an added `Subst` built from the given substitution information.
     *
     *  @param startFrom  starting position of the replacement in the current (old) String
     *  @param fromLength length of the substring being replaced
     *  @param toLength   length of the replacing substring
     */
    def +(startFrom: Int, fromLength: Int, toLength: Int): Repl = {
      val startTo = startFrom + currentShift
      this + (startFrom, startFrom + fromLength, startTo, startTo + toLength)
    }

    /** Merge two `Repl` into one.
     *
     *  Merging is done in 3 parts:
     *  - Previous (current) `Subst`s' `to` are carved (splitted) to abide
     *    by new (additional) `Subst`s' `from`
     *  - New `Subst`s' `from` are updated with the previous `Subst`s's shift
     *  - Both lists are merged into a new `Repl`
     *
     * @param that the other `Repl` to be merged
     */
    def +(that: Repl): Repl = Repl.merge(carve(that.substs), update(that.substs))

    /** Compute the `Interval` in the original `String` corresponding the given position in the current edit.
     *
     *  @param pos Position in the current version of the `TrackString`
     *  @return    Corresponding `Interval` in the original `String`
     */
    def srcPos(pos: Int): Interval = substs match {
      case Nil =>                                       // no subst => same position
        Interval(pos, pos + 1)
      case Subst(from, to) :: _ if (to.end <= pos) =>   // pos after the last subst => shift
        val newPos = from.end + (pos - to.end)
        Interval(newPos, newPos + 1)
      case l => l.dropWhile(_.to.start > pos) match {
        case Nil =>                                     // pos before the first subst => same position
          Interval(pos, pos + 1)
        case Subst(from, to) :: _ if (to.end > pos) =>  // pos between to.start and to.end => from
          from
        case Subst(from, to) :: _ =>                    // pos after a subst.to.end => same position
          val newPos = from.end + (pos - to.end)
          Interval(newPos, newPos + 1)
      }
    }
    /** Compute the `Interval` in the original `String` corresponding the given `Interval` in the current edit.
     *
     *  @param interval `Interval` in the current version of the `TrackString`
     *  @return         Corresponding `Interval` in the original `String`
     */
    def srcPos(interval: Interval): Interval = Interval(srcPos(interval.start).start, srcPos(interval.end - 1).end)
    /** Compute the `Interval` in the original `String` corresponding the given interval in the current edit.
     *
     *  @return         Corresponding `Interval` in the original `String`
     */
    def srcPos(start: Int, end: Int): Interval = srcPos(Interval(start, end))

    /** Builds a new `Repl` with all its `Subst`s shifted .
     *
     *  @param shift The position shift to append to all `Subst`s.
     */
    def >>(shift: Int): Repl = Repl(substs map { s => Subst(s.from >> shift, s.to >> (shift + currentShift)) })


    /** Carve this `Subst`s according to given additional ones for merging.
     *
     *  @param toAdd Additional `Subst`s (ordered by descending `to` position)
     *  @return      This `Repl`'s `Subst`s (ordered by descending `to` position)
     */
    protected[util] def carve(toAdd: List[Subst]): List[Subst] = {
      var toAddLeft = toAdd
      substs flatMap { subst =>
        val result = subst.carved(toAddLeft)
        toAddLeft = result._2
        result._1
      }
    }

    /** Update additional `Subst`s' positions according to previous ones (`this.substs`) for merging.
     *
     *  @param toAdd Additional `Subst`s (ordered by descending `to` position)
     *  @return      Updated additional `Subst`s
     */
    protected[util] def update(toAdd: List[Subst]): List[Subst] =
      toAdd map { subst => Subst(srcPos(subst.from), subst.to) }

  }

  object Repl {

    def apply(subst: Subst): Repl = Repl(List(subst))

    def apply(startFrom: Int, endFrom: Int, startTo: Int, endTo: Int): Repl =
      Repl(Subst from (startFrom, endFrom, startTo, endTo))


    /** Merge carved and updated `Subst`s into a single ordered `List`.
     *  Basically a merge of two already-ordered lists, plus an optimization:
     *  merging adjacent `Subst`s (same `from`, adjacent `to`).
     *
     *  @param carved  Carved `Subst`s (ordered by descending `to` position)
     *  @param updated Updated `Subst`s (ordered by descending `to` position)
     *  @return        Merged `Subst`s (ordered by descending `to` position)
     */
    protected[util] def merge(carved: List[Subst], updated: List[Subst]): Repl = {
      val result = new ListBuffer[Subst]
      var carved2 = carved
      var updated2 = updated

      while (!carved2.isEmpty && !updated2.isEmpty) {
        if (carved2.head.to.start > updated2.head.to.start ||
            (carved2.head.to.start == updated2.head.to.start && carved2.head.to.end > updated2.head.to.end)) {
          result += carved2.head
          carved2 = carved2.tail
        } else {
          result += updated2.head
          updated2 = updated2.tail
        }
      }

      result ++= carved2 ++= updated2

      if (result.isEmpty) Repl()
      else {
        val substs = new ListBuffer[Subst]
        val last = (result.head /: result.toList.tail) { (head, e) =>
          if (head.from == e.from && ((e.to before head.to) || (e.to meets head.to)))
            Subst(e.from, Interval(e.to.start, head.to.end))
          else {
            substs += head
            e
          }
        }
        substs += last
        Repl(substs toList)
      }
    }

  }

}

