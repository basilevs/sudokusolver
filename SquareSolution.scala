package sudokusolver;
import scala.collection.Set;
import scala.collection.BitSet;

class SquareSolution (field: Seq[BitSet]) extends Solution {
	import SquareSolution._
//	def this() = this(fillMapWith(initialBitSet))
	def valuesInPosition(x:Int , y:Int) = field(width*y + x)
	override def toString = {
		def formatSet(set:Set[Int]): String = set.map(_.toString).mkString("(", ",", ")")
		for (y <- 0 until height) yield {
			field.slice(width*y, width*y+width).map(formatSet).mkString(", ")
		}
	}.mkString("", "\n", "\n")
			
	def setValuesInPosition(x:Int , y:Int, values: BitSet) = {
		val position = width*y + x
		val array:Array[BitSet] = field.toArray
		array.update(position, values)
		new SquareSolution(array)
	}
	def excludeValue(x: Int, y:Int, value: Int) = {
		val position = width*y + x
		val oldValues: BitSet = field(position)
		if (! oldValues.contains(value)) {
			this
		} else {
			val newValues = new scala.collection.mutable.BitSet(9)
			for (i <- oldValues) {
				if (i != value)
					newValues+=i
			}
			setValuesInPosition(x, y, newValues)
		}
	}
	def setValue(x: Int, y:Int, value: Int) = {
		val position = width*y + x
		val oldValues: BitSet = field(position)
		if (oldValues.contains(value) && oldValues.size==1) {
			this
		} else {
			val newValues = new scala.collection.mutable.BitSet(9)
			newValues+=value
			setValuesInPosition(x, y, newValues)
		}
	}
	def possibleValues(x:Int , y:Int) = field(width*y + x)
};

object SquareSolution {
	val width = 9;
	val height = 9;
	implicit def setToBitSet(that: Set[Int]): BitSet = {
		val temp = new collection.mutable.BitSet
		temp++=that
		temp
	}
	def valueToBitSet(value: Int): BitSet = setToBitSet(Set(value))
	val initialBitSet = new scala.collection.mutable.BitSet(9)
	for (i <- 1 until 10) {initialBitSet+=i}
	def fillMapWith(s:Set[Int]) = {
		for (i <- 0 until width*height) yield s
	}
	def fromValues(setFields: Seq[Int]) = new SquareSolution(setMapValues(setFields))
	def setMapValues(values: Seq[Int]) = {
		assert(values.size == width*height)
		for (value <- values) yield {
			if (value >= 1)
				valueToBitSet(value)
			else 
				initialBitSet.toImmutable
		}
	}	
};
