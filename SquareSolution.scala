package sudokusolver;
import scala.collection.Set;
import scala.collection.mutable.BitSet;

class SquareSolution (field: Seq[BitSet]) extends Solution {
	import SquareSolution._
//	def this() = this(fillMapWith(initialBitSet))
	def valuesInPosition(x:Int , y:Int) = field(width*y + x)
	override def toString = {
		for (y <- 0 until height) yield {
			field.slice(width*y, width*y+width).mkString(", ")
		}
	}.mkString("\n")
			
	def setValuesInPosition(x:Int , y:Int, values: Set[Int]) = {
		val position = width*y + x
		val array = field.toArray
		array.update(position, values)
		new SquareSolution(array)
	}
	def excludeValue(x: Int, y:Int, value: Int) = {
		val position = width*y + x
		val oldValues: BitSet = field(position)
		if (! oldValues.contains(value)) {
			this
		} else {
			setValuesInPosition(x, y, oldValues - value)
		}
	}
	def setValue(x: Int, y:Int, value: Int) = {
		val position = width*y + x
		val oldValues: BitSet = field(position)
		if (oldValues.contains(value) && oldValues.size==1) {
			this
		} else {
			setValuesInPosition(x, y, oldValues+value)
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
	val initialBitSet = new BitSet(9)
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
				initialBitSet
		}
	}	
};
