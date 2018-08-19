package com.mycompany.myproject

import org.scalatest._

// abstract class UnitSpec extends FlatSpec with Matchers with
//   OptionValues with Inside with Inspectors

import board.Board
import pieces._
import colour._

class MySpec extends FlatSpec {
	val whitePawn = Some(Pawn(WHITE))
	val whiteRook = Some(Rook(WHITE))
	val blackKnight = Some(Knight(BLACK))

	"A Board" should "return return the correct element when queryed by row and column" in {
		val rows = Vector(
			Vector(whitePawn, None, whitePawn),
			Vector(blackKnight, whitePawn, whiteRook)
		)
		val board = new Board(rows)

		assert(board.get(0, 0) === whitePawn)
		assert(board.get(0, 1) === None)
		assert(board.get(0, 2) === whitePawn)
		assert(board.get(1, 0) === blackKnight)
		assert(board.get(1, 1) === whitePawn)
		assert(board.get(1, 2) === whiteRook)
	}


	it should "return a new Board when updated" in {
		val rows = Vector(
			Vector(whitePawn, None, whitePawn),
			Vector(blackKnight, whitePawn, whiteRook)
		)

		val board = new Board(rows)
		val newBoard = board.updated((0,1), whitePawn)

		assert(board.get((0, 1)) === None)
		assert(newBoard.get((0, 1)) === whitePawn)
	}

	it should "throw IndexOutOfBoundException if an index not in the board is accessed stack is popped" in {
    assertThrows[IndexOutOfBoundsException] {
      new Board(Vector(Vector())).get(0,0)
    }
  }

}