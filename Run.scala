package sudokusolver;
object Run {
	def main(args:Array[String]) {
		val task0  = SquareSolution.fromValues(
			Seq(	0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0,
					0,0,0,0,0,0,0,0,0
			)
		)
		val task12 = SquareSolution.fromValues(
			Seq(	7,0,3,0,1,6,0,9,0,
					9,0,8,0,5,7,0,1,0,
					0,0,0,0,0,0,5,0,4,
					0,6,0,3,0,0,9,0,7,
					0,0,2,0,0,9,3,0,1,
					3,0,0,0,4,0,0,0,0,
					8,0,0,0,6,0,0,0,0,
					0,0,4,0,0,3,7,0,8,
					0,7,0,5,0,0,4,0,6
			)
		)
		val task13 = SquareSolution.fromValues(
			Seq(	7,0,0,1,3,0,0,4,0,
					0,0,1,5,9,0,3,0,0,
					0,2,0,0,0,0,0,0,5,
					0,0,0,9,8,0,0,0,0,
					1,9,0,0,0,6,0,5,4,
					8,5,0,0,0,4,0,3,2,
					0,0,4,8,6,0,2,0,0,
					9,0,0,2,5,0,0,1,0,
					0,1,0,0,0,0,0,0,6
			)
		)
		val task14= SquareSolution.fromValues(
			Seq(	8,0,9,0,1,0,0,2,5,
					4,0,5,0,2,0,0,6,7,
					0,0,0,4,0,9,0,0,0,
					0,0,6,0,0,0,0,5,0,
					3,0,0,7,0,8,0,0,2,
					0,8,0,9,0,5,7,0,0,
					9,0,0,5,0,6,0,0,8,
					0,0,8,0,0,0,0,1,0,
					0,2,0,1,0,4,6,0,0
			)
		)
		val task14_reduced= SquareSolution.fromValues(
			Seq(	0,0,9,0,1,0,0,2,0,
					4,0,5,0,2,0,0,6,7,
					0,0,0,4,0,9,0,0,0,
					0,0,6,0,0,0,0,5,0,
					3,0,0,7,0,8,0,0,2,
					0,8,0,9,0,5,0,0,0,
					9,0,0,5,0,6,0,0,8,
					0,0,8,0,0,0,0,1,0,
					0,2,0,1,0,4,6,0,0
			)
		)
		
		val solver = new SquareSolver()
		solver.listeners.append(new KnownRemover)
		solver.start(task14_reduced)
	}
}