package fr.splayce.rel

package object matchers {

  import RE.escapeRegex

  def ncgEsc(expr: String): String =
    if (expr.length > 1) "(?:" + escapeRegex(expr) + ")"
    else escapeRegex(expr)

  /** Build a RE term matching an escaped expression
   * @param expr expression to match
   * @param by   escape prefix
   * @param lb   use LookBehind (WARNING: unbound LookBehind, only works
   *             in .NET flavor, will throw a PatternSyntaxException in Java)
   */
  def   escaped(expr: RE, by: String = "\\", lb: Boolean = false): RE = {
    val ncgBy = ncgEsc(by)
    val prefix = RE("(?:" + ncgBy + "{2})*" + ncgBy)
    (if (lb) ?<=(prefix) else prefix) ~ expr
  }

  /** Build a RE term matching an unescaped expression
   * @param expr expression to match
   * @param by   escape prefix
   * @param lb   use LookBehind (WARNING: unbound LookBehind, only works
   *             in .NET flavor, will throw a PatternSyntaxException in Java)
   */
  def unescaped(expr: RE, by: String = "\\", lb: Boolean = false): RE = {
    val prefix = RE("(?:" + ncgEsc(by) + "{2})*")
    (if (lb) ?<=(prefix) else prefix) ~ expr
  }

}