package main

object Main extends App {
	import board.{ Pos, Board, posToString, stringToPos }
	import game.Game


	val row = Vector(Some(1),Some(2),Some(3))

	val boardRows = Vector(row, row, row)
	// val b = new Board(boardRows)

	// val v = Vector(1,2,3)

	val updates = List(((0, 0), 100), ((1, 1), 101), ((2,2), 102))

	// val out = b.updated(updates)

	val b = Board.initialBoard
	// println(b)
	val p = b.makeMove((0,3), (2,3))

	println(p)

	val pos: Pos = if (args.size > 0) args(0).toUpperCase else "E1"

	println(pos)

	val out = p.possibleMoves(pos).map(posToString)

	// println(out.possibleMoves(2,3))
	println(out)

	println("-------------------")

	val g = Game.init
	println(g)
	g.play("e2", "e4").play("e7", "e5")/*.play("e4", "e5")*/
}