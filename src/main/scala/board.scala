package board


object Board {
	private val rowSize = 8
	
	type Pos = (Int, Int)

	def empty = {
		val emptyRow = Vector.fill(rowSize)(0)
		val emptyRows = Vector.fill(rowSize)(emptyRow)
		new Board(emptyRows)
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

	override def toString = rows.toString
}
