package board


object Board {
	private val rowSize = 8
	
	type Pos = (Int, Int)

	import colour._
	import pieces._

	def empty = {
		val row = Vector.fill(rowSize)(None)
		val rows = Vector.fill(rowSize)(row)
		new Board(rows)
	}

	def initialBoard = {
		def initialPieces(colour: Colour) = Vector(
			Some(Rook(colour)),
			Some(Knight(colour)),
			Some(Bishop(colour)),
			Some(Queen(colour)),
			Some(King(colour)),
			Some(Bishop(colour)),
			Some(Knight(colour)),
			Some(Rook(colour))
		)

		val whitePieces = initialPieces(WHITE)
		val whitePawns = Vector.fill(rowSize)(Some(Pawn(WHITE)))
		val emptyRow = Vector.fill(rowSize)(None)
		val blackPawns = Vector.fill(rowSize)(Some(Pawn(BLACK)))
		val blackPieces = initialPieces(BLACK)

		val rows = Vector(
			whitePieces,
			whitePawns,
			emptyRow,
			emptyRow,
			emptyRow,
			emptyRow,
			blackPawns,
			blackPieces
		)
		new Board(rows)
	}
}

class Board[T](rows: Vector[Vector[T]]) {
	def get(pos: Board.Pos) = {
		val (rowIndex, columnIndex) = pos
		rows(rowIndex)(columnIndex)
	}

	def updated(pos: Board.Pos, elem: T): Board[T] = {
		val (rowIndex, columnIndex) = pos
		val updatedRow = rows(rowIndex).updated(columnIndex, elem)
		val newRows = rows.updated(rowIndex, updatedRow)
		new Board(newRows)
	}

	def updated(updates: List[(Board.Pos, T)]) = {
		val newRows = updates.foldRight(rows) {
			case (curr, acc) => {
				val ((rowIndex, columnIndex), elem) = curr
				val updatedRow = acc(rowIndex).updated(columnIndex, elem)
				acc.updated(rowIndex, updatedRow)
			}
		}
		new Board(newRows)
	}

	override def toString = {
		rows.map {
			row => row.map {
				case Some(piece) => piece.toString
				case _ => " "
			}.mkString
		}.reverse.mkString("\n")
	}
}
