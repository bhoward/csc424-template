package assignment1

import org.scalatest.flatspec._
import org.scalatest.matchers.should._

class GroupSpec extends AnyFlatSpec with Matchers:
  "An empty Group" should "not contain any elements" in {
    val empty = Group[Int]
    val sample = 0 to 10
    sample.exists(x => empty.has(x)) shouldBe false
  }

  it should "allow deleting a non-existent element" in {
    val empty = Group[Int]
    noException should be thrownBy empty.delete(0)
  }

  "A group created from a List" should "contain those elements" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    sample.forall(x => group.has(x)) shouldBe true
  }

  it should "not contain other elements" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    val others = List(0, 7, 8)
    others.exists(x => group.has(x)) shouldBe false
  }

  "An element added to a group" should "be contained in the group" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    group.add(7)
    group.has(7) shouldBe true
  }

  it should "not change the other elements" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    group.add(7)
    sample.forall(x => group.has(x)) shouldBe true
  }

  it should "not add other elements" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    group.add(7)
    val others = List(0, 8)
    others.exists(x => group.has(x)) shouldBe false
  }

  "An element deleted from a group" should "not be contained in the group" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    group.delete(1)
    group.has(1) shouldBe false
  }

  it should "not change the other elements" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    group.delete(1)
    val rest = List(2, 3, 4, 5, 6, 9)
    rest.forall(x => group.has(x)) shouldBe true
  }

  it should "not add other elements" in {
    val sample = List(3, 1, 4, 1, 5, 9, 2, 6)
    val group = Group.from(sample)
    group.delete(1)
    val others = List(0, 7, 8)
    others.exists(x => group.has(x)) shouldBe false
  }

