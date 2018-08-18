package main

object Main extends App {
	import board.Board


	val row = Vector(1,2,3)

	val boardRows = Vector(row, row, row)
	val b = new Board(boardRows)

	// val v = Vector(1,2,3)

	val updates = List(((0, 0), 100), ((1, 1), 101), ((2,2), 102))

	// val out = b.updated(updates)
	val out = Board.empty.get((0,0))

	println(out)
}