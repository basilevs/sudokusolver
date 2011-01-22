package sudokusolver;
import scala.collection.Set;
import scala.collection.mutable.ArrayBuffer;

trait Solution {
	def excludeValue(x: Int, y:Int, value: Int): Solution
	def setValue(x: Int, y:Int, value: Int): Solution
	def possibleValues(x: Int, y:Int): Set[Int]
};

/** Represents a set of rules and propagates notifications of solution change.
*/
trait Solver {
	import Solver._
	def groups: Seq[Group]
	def cells: Seq[Cell]
	var listeners = new Listener.Collection
	/**	Futher changes solution on one cell modification.
		All algorithms and heuristics are to implement this.
		Initially all algorithms are notified about change of cells set.
	*/
	def start(iSolution: Solution): Solution
}

object Solver {
	trait Group extends Seq[Cell] {}
	trait Cell {
		def groups: Set[Group]
		def possibleValues(iSolution: Solution): Set[Int]
		def excludeValue(iSolution: Solution, value: Int): Solution
		def setValue(iSolution: Solution, value: Int): Solution
	}
	trait Listener {
		/**	Is called on cell change
			Algorithms may change cell's neighbours from within this callback.
			All algotrithms will be notified of that change too.
			If solution is change by algorithm it should return a new version.
			Otherwse a null should be returned.
		*/
		def onChange(iSolution: Solution, cell:Cell): Solution
	}
	object Listener {
		class Collection extends ArrayBufer[Listener] with Listener {
			def onChange(iSolution: Solution, cell:Cell):Solution = {
				var curSolution = iSolution
				var changed = false
				for (i <- this) {
					assert(cell != null)
					assert(curSolution != null)
					assert(i != null)
					val newSolution = i.onChange(curSolution, cell)
					if (newSolution != null) {
						curSolution = newSolution
						changed = true
					} 
				}
				if (changed)
					return curSolution
				else 
					return null
			}
		}
	}
}


