package ilc
package feature
package integers

object Library {
  import abelianGroups.Library._


  // stamp for addition = big-endian encoding of "add!"
  val additionIndexedGroupStamp : Int = {
    val bytes = Array('a', 'd', 'd', '!').map(_.toInt)
    (0 until bytes.length).map(i =>
      bytes(i) << 8*(bytes.length - i - 1)
    ).sum
  }

  assert(additionIndexedGroupStamp == 0x61646421)


  //This is supposed to match additiveGroupOnIntegers, but this isn't tested
  //(except indirectly in bags.DeltaBagSuite).
  val additiveGroupOnIntegers =
    IndexedGroup[Int](additionIndexedGroupStamp, x => x + _, -_, 0)
}
