package sudokusolver;
import scala.collection.Set;
import scala.collection.mutable;

trait Solution {
	def excludeValue(x: Int, y:Int, value: Int): Solution
	def setValue(x: Int, y:Int, value: Int): Solution
	def possibleValues(x: Int, y:Int): Set[Int]
}


/** Represents a set of rules and propagates notifications of solution change.
*/
trait Solver {
	import Solver._
	type SolverSolution <: Solution
	var listeners = new Listener.Collection
	/**	Futher changes solution on one cell modification.
		All algorithms and heuristics are to implement this.
		Initially all algorithms are notified about change of cells set.
	*/
	def start(iSolution: SolverSolution): SolverSolution
}

object Solver {
	trait Group extends Seq[Cell] {}
	trait Cell {
		type SolverSolution <: Solution
		def groups: Set[Group]
		def possibleValues(iSolution: SolverSolution): Set[Int]
		def excludeValue(iSolution: SolverSolution, value: Int): SolverSolution
		def setValue(iSolution: SolverSolution, value: Int): SolverSolution
	}
	trait Listener {
		type SolverSolution <: Solution
		val solver:Solver
		/**	Is called on cell change
			Algorithms may change cell's neighbours from within this callback.
			All algotrithms will be notified of that change too.
			If solution is change by algorithm it should return a new version.
			Otherwse a null should be returned.
		*/
		def onChange(iSolution: Solution, cell:Cell): Solution
	}
	object Listener {
		class Collection extends mutable.ArrayBuffer[Listener] with Listener {
			def onChange(iSolution: SolverSolution, cell:Cell):Solution = {
				var rv = iSolution
				var changed = false
				for (i <- this) {
					assert(cell != null)
					assert(rv != null)
					assert(i != null)
					val newSolution = i.onChange(rv, cell)
					if (newSolution != null) {
						rv = newSolution
						changed = true
					} 
				}
				if (changed)
					return rv
				else {
					print("No change from listeners for %s\n".format(cell));
					return null
				}
			}
		}
	}
}


