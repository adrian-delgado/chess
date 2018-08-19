package colour {
	abstract class Colour
	object WHITE extends Colour
	object BLACK extends Colour
}

package pieces {
	import colour._
	import board.Pos

	abstract class Piece {
		val colour: Colour
		val directions: List[Pos]
		val distanceLimit: Int
	}

	case class King(colour: Colour) extends Piece {
		val distanceLimit = 1
		val directions = List(
			(-1, -1), (-1, 0), (-1, 1),
			 (0, -1), /*     */ (0, 1),
			 (1, -1),  (1, 0),  (1, 1)
		)
		override def toString = if(colour == WHITE) "K" else "k"
	}
	case class Queen(colour: Colour) extends Piece {
		val distanceLimit = 7
		val directions = List(
			(-1, -1), (-1, 0), (-1, 1),
			 (0, -1), /*     */ (0, 1),
			 (1, -1),  (1, 0),  (1, 1)
		)
		override def toString = if(colour == WHITE) "Q" else "q"
	}
	case class Rook(colour: Colour) extends Piece {
		val distanceLimit = 7
		val directions = List(
			/*   */ (-1, 0), /*   */
			(0, -1), /*    */ (0, 1),
			/*    */ (1, 0), /*    */
		)
		override def toString = if(colour == WHITE) "R" else "r"
	}

	case class Bishop(colour: Colour) extends Piece {
		val distanceLimit = 7
		val directions = List(
			(-1, -1), /*    */ (-1, 1),
			/*     */ /*    */ /*    */
			 (1, -1), /*    */  (1, 1)
		)
		override def toString = if(colour == WHITE) "B" else "b"
	}
	case class Knight(colour: Colour) extends Piece {
		val distanceLimit = 1
		val directions = List(
			(1, 2), (2, 1), (2, -1), (1, -2),
			(-1, -2), (-2, -1), (-2, 1), (-1, 2)
		)
		override def toString = if(colour == WHITE) "N" else "n"
	}
	case class Pawn(colour: Colour) extends Piece {
		val distanceLimit = 1
		val directions = if (colour == WHITE) List((1, -1), (1, 0), (1, 1), (2, 0))
						 else List((-1, -1), (-1, 0), (-1, 1), (-2, 0))
		override def toString = if(colour == WHITE) "P" else "p"
	}
}