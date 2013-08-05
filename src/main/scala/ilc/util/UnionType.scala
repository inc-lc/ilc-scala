package ilc
package util

// Union type for scala
//
// Source (search for Miles Sabin):
// http://stackoverflow.com/questions/3508077/
//
// Usage:
//
//   def toIntArray[T: Or[Int, String]#Type](value: T): Array[Int] =
//     value match {
//       case i: Int => Array(i)
//       case s: String => s.toCharArray.map(_.toInt)
//     }

object UnionType {
  private type NOT[P] = P => Nothing
  private type OR[P, Q] = NOT[NOT[P] with NOT[Q]]
  type Or[S, T] = { type Type[X] = NOT[NOT[X]] <:< OR[S, T] }
}
