package sudokusolver;

class KnownRemover extends sudokusolver.Solver.Listener {
	import Solver._
	def onChange(iSolution: Solution, cell:Cell): Solution = {
		var rv = iSolution
		var changed = false
		if (cell.possibleValues(rv).size == 1) {
			val value = cell.possibleValues(rv).toSeq(0)
			for (group <- cell.groups) {
				for (cell <- group) {
					val temp = cell.excludeValue(rv, value)
					if (temp != null) {
						rv = temp
						changed = true
					}
				}
			}
			if (changed)
				rv
			else
				null
		}
		null
	}
}