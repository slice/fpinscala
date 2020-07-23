package zone.slice.fpinscala

class Chapter3Suite extends munit.FunSuite {
  import chapter3._

  val three        = List(1, 2, 3)
  val threeDoubles = List(1.0, 2.0, 3.0)

  test("3.2") {
    assertEquals(List.tail(three), List(2, 3))
  }

  test("3.3") {
    assertEquals(List.setHead(three, 2), List(2, 2, 3))
  }

  test("3.4") {
    assertEquals(List.drop(three, 1), List.tail(three))
    assertEquals(List.drop(three, 2), List(3))
  }

  test("3.5") {
    assertEquals(List.dropWhile(three)(_ < 3), List(3))
  }

  test("3.6") {
    assertEquals(List.init(three), List(1, 2))
  }

  test("3.9") {
    assertEquals(List.length(three), 3)
  }

  test("3.10") {
    assertEquals(List.foldLeft(three, 0)(_ + _), 6)
  }

  test("3.11") {
    assertEquals(List.sum3(three), 6)
    assertEquals(List.product3(threeDoubles), 6.0)
    assertEquals(List.length2(three), 3)
  }

  test("3.12") {
    assertEquals(List.reverse(three), List(3, 2, 1))
    assertEquals(List.reverse2(three), List(3, 2, 1))
  }

  test("3.14") {
    assertEquals(List.append2(three, three), List(1, 2, 3, 1, 2, 3))
  }

  test("3.15") {
    assertEquals(
      List.flatten(List(three, three, three)),
      List(1, 2, 3, 1, 2, 3, 1, 2, 3)
    )
    assertEquals(
      List.flatten2(List(three, three, three)),
      List(1, 2, 3, 1, 2, 3, 1, 2, 3)
    )
  }

  test("3.16") {
    assertEquals(List.add1(three), List(2, 3, 4))
  }

  test("3.17") {
    assertEquals(List.doublesToString(threeDoubles), List("1.0", "2.0", "3.0"))
  }

  test("3.18") {
    assertEquals(List.map(three)(_ + 1), List(2, 3, 4))
    assertEquals(List.map2(three)(_ + 1), List(2, 3, 4))
  }

  test("3.19") {
    assertEquals(List.filter(three)(_ == 2), List(2))
  }

  test("3.20") {
    assertEquals(List.flatMap(three)(n => List(n, n)), List(1, 1, 2, 2, 3, 3))
  }

  test("3.21") {
    assertEquals(List.filter2(three)(_ == 2), List(2))
  }

  test("3.22") {
    assertEquals(List.addCorresponding(three, three), List(2, 4, 6))
  }

  test("3.23") {
    assertEquals(
      List.zipWith(three, three)(_ + _),
      List.addCorresponding(three, three)
    )
  }

  test("3.24") {
    assertEquals(List.hasSubsequence(three, List(2, 3)), true)
    assertEquals(
      List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 5, 4), List(5, 6, 5)),
      true
    )
  }

  val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
  val deepTree =
    Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))

  test("3.25") {
    assertEquals(Tree.size(tree), 5)
  }

  test("3.26") {
    assertEquals(Tree.maximum(tree), 3)
  }

  test("3.27") {
    assertEquals(Tree.depth(tree), 2)
    assertEquals(Tree.depth(deepTree), 3)
  }

  test("3.28") {
    assertEquals(
      Tree.map(tree)(_.toString),
      Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3")))
    )
  }

  test("3.29") {
    assertEquals(
      Tree.map2(tree)(_.toString),
      Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3")))
    )
    assertEquals(Tree.size2(tree), Tree.size(tree))
    assertEquals(Tree.maximum2(tree), Tree.maximum(tree))
    assertEquals(Tree.depth2(tree), Tree.depth(tree))
    assertEquals(Tree.depth2(deepTree), Tree.depth(deepTree))
  }
}
