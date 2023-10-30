package Fundamentals

import scala.annotation.tailrec
import scala.math.abs

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

  /*
  You get an array of numbers, return the sum of all of the positives ones.
   */
  def positiveSum(arr: Array[Int]): Int = {
    arr.filter(_ > 0).sum
  }
  println(positiveSum(Array(1,-2,3,4,5)))

  /*
  Write a program that finds the summation of every number from 1 to num.
  The number will always be a positive integer greater than 0.
   */
  @tailrec
  def summation(n: Int, res: Int): Int = {
    if (n == 0) res
    else summation(n - 1, res + n)
  }
  // Better solution:
  // def summation(n: Int): Int = (1 to n).sum

  println(summation(8, 0))

  /*
  Imagine you start on the 5th floor of a building, then travel down to the 2nd floor,
  then back up to the 8th floor. You have travelled a total of 3 + 6 = 9 floors of distance.
   */
  // Shorter and better solution:
  // def elevatorDistance(xs: Seq[Int]): Int = (xs zip xs.tail).map(_ - _).map(_.abs).sum
  def elevatorDistance(xs: Seq[Int]): Int = {
    @tailrec
    def h(start: Int, acc: Int, tail: Seq[Int]): Int = {
      if (tail.isEmpty) acc
      else start - tail.head match {
        case num if num > 0 => h(tail.head, acc + start - tail.head, tail.tail)
        case num => h(tail.head, acc - num, tail.tail)
      }
    }
    h(xs.head, 0, xs.tail)
  }

  println(elevatorDistance(Seq(5, 8, 2)))
}
