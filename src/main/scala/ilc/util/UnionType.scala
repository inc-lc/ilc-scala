package ilc
package util

// Union type for scala
//
// Source (search for Miles Sabin):
// http://stackoverflow.com/a/6312508
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
  private type OR3[P, Q, R] = NOT[NOT[P] with NOT[Q] with NOT[R]]
  private type OR4[P, Q, R, S] =
    NOT[NOT[P] with NOT[Q] with NOT[R] with NOT[S]]

  type Or[S, T] = { type Type[X] = NOT[NOT[X]] <:< OR[S, T] }
  type Or3[R, S, T] = { type Type[X] = NOT[NOT[X]] <:< OR3[R, S, T] }
  type Or4[Q, R, S, T] = { type Type[X] = NOT[NOT[X]] <:< OR4[Q, R, S, T] }
}
