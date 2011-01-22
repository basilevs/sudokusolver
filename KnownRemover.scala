package sudokusolver;

class KnownRemover extends sudokusolver.Solver.Listener {
	import Solver._
	def onChange(iSolution: Solution, cell:Cell): Solution = {
		var rv = iSolution
		var changed = false
		if (cell.possibleValues(rv).size == 1) {
			print("KnownRemover start for %s:\n".format(cell.toString))
			print(rv+"\n")
			val value = cell.possibleValues(rv).toSeq(0)
			for (group <- cell.groups) {
				try {
					for (groupCell <- group) {
						if (groupCell != cell) {
//							print("Excluding %d from %s\n".format(value, groupCell))
							val temp = groupCell.excludeValue(rv, value)
							if (temp != null) {
								print("KnownRemover removed %d from %s for %s:\n".format(value, groupCell.toString, cell.toString))
//								print("Before:\n"+rv)
//								print("After:\n"+temp)
								rv = temp
								changed = true
							}
						}
					}
				} catch {
					case e:Throwable => throw new RuntimeException("Can't process group with values: "+group.map(_.possibleValues(rv)).toString, e);
				}
			}
			if (changed) {
				print("KnownRemover result for %s:\n".format(cell.toString))
				print(rv+"\n")
				rv
			} else
				null
		} else 
			null
	}
}