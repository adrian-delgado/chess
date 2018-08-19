package game

import colour._
import board.{ Board, Pos, posOps }

object Game {
	def init = new Game(WHITE, 1, Board.initialBoard)
}

class Game(turn: Colour, moveNo: Int, board: Board) {
	type Move = (Pos, Pos)

	def play(move: Move): Game = {
		val (from, to) = move

		if (!from.inBoard || !to.inBoard) throw new Exception("move not possible!")

		val piece = board.get(from)
		if (!piece.isDefined) throw new Exception("nothing to move!")

		if (piece.get.colour != turn) throw new Exception(s"${turn} moves!")
		
		if (!board.possibleMoves(from).contains(to)) throw new Exception("move not possible!")

	 	val newGame = new Game(
			turn.other,
			moveNo + 1,
			board.makeMove(from, to)
		)
		println(newGame)
		
		newGame
	}

	override def toString = s"turn: ${if (turn == WHITE) "white" else "black"}\nno:${moveNo}\n\n${board.toString}\n\n"

}