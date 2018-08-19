package board


object Board {
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

class Board[T](rows: Vector[Vector[Option[T]]]) {
	import pieces._

	def get(pos: Pos) = {
		val (rowIndex, columnIndex) = pos
		rows(rowIndex)(columnIndex)
	}

	def makeMove(from: Pos, to: Pos) = {
		val piece = get(from)
		updated(List((from, None), (to, piece)))
	}

	def possibleMoves(pos: Pos) = get(pos) match {
		case None => Nil
		case Some(piece: Piece) => {
			def distanceStream = (1 to piece.distanceLimit).iterator
			def validator(direction: Pos)(dist: Int) = {
				val currentPos = direction * dist + pos
				if (currentPos.inBoard) get(currentPos) match {
					case None => true
					case Some(otherPiece: Piece) =>
						if (piece.colour == otherPiece.colour) false
						else {
							val prevPos = direction * (dist - 1) + pos
							get(prevPos) match {
								case None => true
								case Some(_) => false
							}
						}
				}
				else false
			}

			for (
				direction <- piece.directions;
				distance <- distanceStream.takeWhile(validator(direction))
			) yield direction * distance + pos
		}
	}

	def updated(pos: Pos, elem: Option[T]) = {
		val (rowIndex, columnIndex) = pos
		val updatedRow = rows(rowIndex).updated(columnIndex, elem)
		val newRows = rows.updated(rowIndex, updatedRow)
		new Board(newRows)
	}

	def updated(updates: List[(Pos, Option[T])]) = {
		val newRows = updates.foldRight(rows) {
			case (pos, acc) => {
				val ((rowIndex, columnIndex), elem) = pos
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
				case None => " "
			}.mkString
		}.reverse.mkString("\n")
	}
}
