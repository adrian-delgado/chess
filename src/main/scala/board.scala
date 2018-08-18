package board


object Board {
	private val rowSize = 8
	
	type Pos = (Int, Int)

	import colour._
	import pieces._

	implicit def posOps(pos: Pos) = new {
		def + (otherPos: Pos) = (pos._1 + otherPos._1, pos._2 + otherPos._2)
		def * (otherPos: Pos) = (pos._1 * otherPos._1, pos._2 * otherPos._2)
		def * (n: Int) = (pos._1 * n, pos._2 * n)
		def inBoard = (0 <= pos._1 && pos._1 < 8) && (0 <= pos._2 && pos._2 < 8)
	}

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
	import Board.{ Pos, posOps, rowSize }
	import pieces._

	def get(pos: Pos) = {
		val (rowIndex, columnIndex) = pos
		rows(rowIndex)(columnIndex)
	}

	def makeMove(from: Pos, to: Pos) = {
		val piece = get(from)
		updated(List((from, None), (to, piece)))
	}

	def legalMoves(pos: Pos) = {
		val pieceOption = get(pos)
		pieceOption match {
			case None => Nil
			case Some(piece: Piece) => piece match {
				case King(_) => {
					val directions = List((-1, -1), (-1, 0), (-1, 1),
								   		   (0, -1),			  (0, 1),
								   		   (1, -1),  (1, 0),  (1, 1))
					directions.map(_ + pos).filter(_.inBoard)
					for (
						direction <- directions;
						newPos = direction + pos if newPos.inBoard
					) yield newPos
				}
				case Queen(_) => {
					val directions = List((-1, -1), (-1, 0), (-1, 1),
										   (0, -1),			  (0, 1),
										   (1, -1),  (1, 0),  (1, 1))
					def distanceStream = (1 to rowSize).iterator

					for (
						direction <- directions;
						distance <- distanceStream.takeWhile(dist => {
							val currentPos = direction * dist + pos
							if (currentPos.inBoard) get(currentPos) match {
								case None => true
								case Some(otherPiece: Piece) =>
									if (piece.isWhite == otherPiece.isWhite) false
									else {
										val prevPos = direction * (dist - 1) + pos
										get(prevPos) match {
											case None => true
											case Some(_) => false
										}
									}
							}
							else false
						})
					) yield direction * distance + pos

					/*directions.flatMap(direction => {
						val distances = distanceStream.takeWhile(distance => {
							val currentPos = direction * distance + pos
							if (currentPos.inBoard) get(currentPos) match {
								case None => true
								case Some(otherPiece: Piece) =>
									if (piece.isWhite == otherPiece.isWhite) false
									else {
										val prevPos = direction * (distance - 1) + pos
										get(prevPos) match {
											case None => true
											case Some(_) => false
										}
									}
							}
							else false
						})
						distances.map(distance => direction * distance + pos).toList
					})*/
				}
				case _ => Nil
			}
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
