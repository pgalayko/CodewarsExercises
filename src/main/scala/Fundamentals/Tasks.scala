package Fundamentals

import scala.annotation.tailrec

object Tasks extends App {
  /*
  Given a set of numbers, return the additive inverse of each.
  Each positive becomes negatives, and the negatives become positives.
   */
  def invert(lst: List[Int]): List[Int] = {
    lst.map(x => -x)
  }
//  def invert(lst: List[Int], acc: List[Int]): List[Int] = {
//    if (lst.isEmpty) acc
//    else invert(lst.tail, acc :+ -lst.head)
//  }
//  println(invert(List(1,2,3,4,5), Nil))
  println(invert(List(1,2,3,4,5)))

  /*
  In this kata, you are asked to square every digit of a number and concatenate them.
   */
  def squareDigits(n: Int): Int = {
    val l = s"$n".split("").toList
    @tailrec
    def help(list: List[String], acc: String): String = {
      if (list.isEmpty) acc
      else {
        val num = list.head.toInt
        help(list.tail, acc + num * num)
      }
    }
    help(l, "").toInt
  }
  println(squareDigits(9119))

  /*
  The Western Suburbs Croquet Club has two categories of membership, Senior and Open.
  They would like your help with an application form that will tell prospective members which category they will be placed.
  To be a senior, a member must be at least 55 years old and have a handicap greater than 7.
  In this croquet club, handicaps range from -2 to +26; the better the player the lower the handicap.
   */
  def openOrSenior(data: List[(Int, Int)]): List[String] = {
    data.map {x =>
      if (x._1 >= 55 && x._2 > 7) "Senior"
      else "Open"
    }
  }
  println(openOrSenior(List((45, 12),(55,21),(19, -2),(104, 20))))
  /*
  If a name has exactly 4 letters in it, you can be sure that it has to be a friend of yours!
  Otherwise, you can be sure he's not...
   */
  def friend(xs: Seq[String]): Seq[String] = xs.filter(_.length == 4)
  println(friend(Seq("Ryan", "Kieran", "Mark")))
}
