//import scala.util.control.Exception._
import scala.annotation.tailrec

trait List[+A] {
	def head: A
	def tail: List[A]
	//def length: Int
	def isEmpty: Boolean
	def add[B >: A] (elem: B): List[B]
	def ++[B >: A] (other: List[B]): List[B]
	def reverse: List[A]
	def filter[B >: A](predicate: Predicate[B]): List[B]
}

case object Nil extends List[Nothing] {
	override def head: Nothing = throw new NoSuchElementException
	override def tail: List[Nothing] = throw new UnsupportedOperationException
	//override def length = 0
	override def isEmpty: Boolean = true
	override def add[B] (elem: B): List[B] = new ::(elem, Nil)  //add element
	override def ++[B] (other: List[B]): List[B] = other  //add list of elements
	override def reverse: List[Nothing] = Nil
	override def toString: String = "[]"
	override def filter[B] (predicate: Predicate[B]): List[Nothing] = Nil
}

case class ::[A] (val head: A, val tail: List[A]) extends List[A] {
	/*def length: Int = {
		var num = 0
		var x = this
		
		while (x != null) {
			num += 1
			x = this.tail.head
		}
	}*/

	override def isEmpty: Boolean = false
	override def add[B >: A] (elem: B): List[B] = new ::(elem, this)
	override def ++[B >: A] (other: List[B]): List[B] = new ::(head, tail ++ other)

	override def reverse: List[A] = {
		@tailrec
		def reverseUtil(input: List[A], output: List[A]): List[A] = {
			if (input.isEmpty) output
			else reverseUtil (input.tail, new ::(input.head, output))
		}

		reverseUtil(this, Nil)

	}

	override def toString: String = {
		def enumerateAll(list: List[A]): String =
			if (list.isEmpty) ""
			else if (list.tail.isEmpty) "" + list.head
			else list.head + " " + enumerateAll(list.tail)

		"[" + enumerateAll(this) + "]"
	}

	override def filter[B >: A] (predicate: Predicate[B]): List[B] = {
		if (predicate.apply(head)) new ::(head, tail filter predicate)
		else tail filter predicate
	}
}

object List{
	def flatten[T] (deepList: List[List[T]]): List[T] = {
//		if (deepList.isEmpty) Nil
//		else deepList.head ++ flatten(deepList.tail)

		def flattenUtil(remaining: List[List[T]], currentListExpanding: List[T], acc: List[T]): List[T] = {
			if (currentListExpanding.isEmpty) {
				if (remaining.isEmpty) acc
				else flattenUtil(remaining.tail, remaining.head, acc)
			} else flattenUtil(remaining, currentListExpanding.tail, new ::(currentListExpanding.head, acc))
		}

		flattenUtil(deepList, Nil, Nil).reverse

	}
}

trait Predicate[T] {
	def apply(elem: T): Boolean
}

object Main extends App {
	val listA = Nil
	val listB = new ::(2, new ::(4, Nil))

	println(listA)
	println(listB)

	println(listB.reverse)

//	println(listB ++ new ::(5, new ::(1, new ::(9, Nil))))
	val listC = listB ++ new ::(5, new ::(1, new ::(9, Nil)))
	val deepList = new ::(listB, new ::(listA, new ::(listC, Nil)))

//	println(List.flatten(deepList))
	val listD = List.flatten(deepList)
	val predicate: Predicate[Int] = new Predicate[Int] {
		override def apply(elem: Int): Boolean = elem % 2 == 0
	}

	println(listD filter predicate)

}