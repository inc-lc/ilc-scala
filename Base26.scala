/**
 * Canonical way of converting integers into names
 *
 * 1. numbers are rendered into base-26 representation
 *
 * 2. each number between 0 and 25 is mapped to a character,
 *    starting at 0 -> 'x'.
 *
 * 3. if the number is negative to start with, then its name
 *    will be written in ALL CAPS.
 */

import scala.math._

object Base26 {

  // alphabet ORDER BY appropriateness as variable name DESC
  val alphabet = "xyzuvwpqmnijrstabcdefghklo".toCharArray
  assert(alphabet.length == alphabet.distinct.length)
  assert(alphabet.length == 26)

  val base = alphabet.length

  // Convert the absolute value of an integer to
  // big-endian base-26 digits
  def toBase26(i: Int): Array[Int] = {
    var n = i.abs
    var res : Array[Int] = null
    if (n < 1)
      res = Array(0)
    else {
      // n >= 1, logarithm is safe
      // The amount of digits of a number with a given base
      // is equal to 1 plus the floor of the logarithm of that
      // number w.r.t. the base.
      val digits = 1 + (log(n) / log(base)).toInt
      res = Range(0, digits).map { _ =>
        val digit = n % base
        n = n / base
        digit
      }.reverse.toArray
    }
    res
  }

  // Convert an integer to a usable name
  def name(i: Int): String = {
    val digits = toBase26(i)
    if(digits.length > 1) digits(0) = digits(0) - 1
    val name = digits.map{ j => alphabet(j) }.mkString
    if (i >= 0) name
    else        name.toUpperCase
  }
}
