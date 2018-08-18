package main

object Main extends App {
	import board.Board


	val row = Vector(Some(1),Some(2),Some(3))

	val boardRows = Vector(row, row, row)
	// val b = new Board(boardRows)

	// val v = Vector(1,2,3)

	val updates = List(((0, 0), 100), ((1, 1), 101), ((2,2), 102))

	// val out = b.updated(updates)

	val b = Board.initialBoard
	// println(b)
	val out = b.makeMove((0,3), (2,3))

	println(out)

	// println(out.legalMoves(2,3))
	println(out.legalMoves(0,4))
}