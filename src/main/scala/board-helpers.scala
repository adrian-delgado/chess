package object board {
	type Pos = (Int, Int)
	
	implicit def posOps(pos: Pos) = new {
		def + (otherPos: Pos) = (pos._1 + otherPos._1, pos._2 + otherPos._2)
		def * (otherPos: Pos) = (pos._1 * otherPos._1, pos._2 * otherPos._2)
		def * (n: Int) = (pos._1 * n, pos._2 * n)
		def inBoard = (0 <= pos._1 && pos._1 < 8) && (0 <= pos._2 && pos._2 < 8)
	}

	val rowSize = 8
}