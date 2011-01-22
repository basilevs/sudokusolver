package sudokusolver;
object Run {
	def main(args:Array[String]) {
		val solution = SquareSolution.fromValues(
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
		val solver = new SquareSolver()
		solver.listeners.append(new KnownRemover)
		solver.start(solution)
	}
}