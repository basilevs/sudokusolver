package sudokusolver;
import scala.collection.Set;

class SquareSolver extends Solver {
	import Solver._
	val width = 9;
	val height = 9;
	val blockSize = 3;
	def groupContainsValue(iSolution:Solution, group:Group, value:Int): Boolean = {
		for (cell <- group) {
			if (cell.possibleValues(iSolution).contains(value))
				return true
		}
		return false
	}
	case class SquareCell(x: Int, y: Int) extends Cell {
		def groups = Set[Group](verticalGroups(x), horizontalGroups(y), blockGroups(y/blockSize * width/blockSize + x/blockSize))
		def possibleValues(iSolution: Solution): Set[Int] = iSolution.possibleValues(x, y);
		def excludeValue(iSolution: Solution, value: Int):Solution  = {
			val values = possibleValues(iSolution)
//			print("%d, %d values are ".format(x, y)+values+"\n")
			if (!values.contains(value))
				return null // Already excluded
			if (values.size == 1)
				throw new RuntimeException("Unable to exclude %d for %s, possible values: %s".format(value, this.toString, values.toString))
//			print("Before exclusion:\n" + iSolution)
			val curSolution = iSolution.excludeValue(x, y, value)
//			print("After exclusion:\n" + curSolution)
			assert(curSolution != null)
			for (group <- groups)
				if (!groupContainsValue(curSolution, group, value))
					throw new RuntimeException("No %d left in %s".format(value, group.toString))
			print("Successfully excluded %d from %s\n".format(value, this))
//			print(curSolution)
			val nextSolution = listeners.onChange(curSolution, this)
			if (nextSolution != null)
				nextSolution
			else 
				curSolution	
		}
		def setValue(iSolution: Solution, value: Int): Solution = {
			val values = possibleValues(iSolution)
			if (!values.contains(value))
				throw new RuntimeException("Unable to set %d for cell(%d, %d)".format(value, x, y))
			if (values.size == 1)
				return null
			var curSolution = iSolution.setValue(x, y, value)
			assert(curSolution != null)
			val nextSolution = listeners.onChange(curSolution, this)
			if (nextSolution != null)
				nextSolution
			else 
				curSolution	
		}
	}
	
	class VerticalGroup(x:Int) extends Group {
		def elements = (0 until length).map(apply(_)).elements
		def apply(y:Int) = allCells(width*y+x)
		def length = height
	}
	class HorizontalGroup(y:Int) extends Group {
		def elements = (0 until length).map(apply(_)).elements
		def apply(x:Int) = allCells(width*y+x)
		def length = width
	}
	class BlockGroup(x: Int, y:Int) extends Group {
		def elements = (0 until length).map(apply(_)).elements
		def length = blockSize*blockSize
		def apply(n:Int) = {
			val subRow = n/blockSize
			val subColumn = n - subRow*blockSize
			val cellY = y*blockSize+subRow
			val cellX = x*blockSize+subColumn
			allCells(width*cellY+cellX)
		}
	}
	val allCells:Array[Cell] = {for (y <- 0 until height; x <- 0 until width) yield {new SquareCell(x, y)}}.toArray
	val verticalGroups:Array[Group] = {for (x <- 0 until width) yield new VerticalGroup(x)}.toArray;
	val horizontalGroups:Array[Group] = {for (y <- 0 until height) yield new HorizontalGroup(y)}.toArray;
	val blockGroups:Array[Group] = {
		for {	y <- 0 until height/blockSize
				x <- 0 until width/blockSize
		} yield new BlockGroup(x, y);
	}.toArray
	def cells = allCells
	def groups = for {	col <- Seq(horizontalGroups, verticalGroups, blockGroups)
						group <- col
					} yield group
	def start(iSolution:Solution) = {
		var rv = iSolution
		var changed = false
		for (cell <- allCells) {
			val temp = listeners.onChange(rv, cell)
			if (temp!=null) {
				print("SquareSolver result for %s:\n".format(cell))
				print(temp)
				rv =temp
				changed = true
			}
		}
		if (changed)
			rv
		else
			null
	}
};

	
