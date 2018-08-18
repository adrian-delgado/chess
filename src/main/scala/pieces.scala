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
		override def toString = this match {
			case King(colour) => if(colour == WHITE) "K" else "k"
			case Queen(colour) => if(colour == WHITE) "Q" else "q"
			case Rook(colour) => if(colour == WHITE) "R" else "r"
			case Bishop(colour) => if(colour == WHITE) "B" else "b"
			case Knight(colour) => if(colour == WHITE) "N" else "n"
			case Pawn(colour) => if(colour == WHITE) "P" else "p"
		}
	}

	case class King(colour: Colour) extends Piece {
		val distanceLimit = 1
		val directions = List(
			(-1, -1), (-1, 0), (-1, 1),
			 (0, -1), /*     */ (0, 1),
			 (1, -1),  (1, 0),  (1, 1)
		)
	}
	case class Queen(colour: Colour) extends Piece {
		val distanceLimit = 7
		val directions = List(
			(-1, -1), (-1, 0), (-1, 1),
			 (0, -1), /*     */ (0, 1),
			 (1, -1),  (1, 0),  (1, 1)
		)
	}
	case class Rook(colour: Colour) extends Piece {
		val distanceLimit = 7
		val directions = List(
			/*   */ (-1, 0), /*   */
			(0, -1), /*    */ (0, 1),
			/*    */ (1, 0), /*    */
		)
	}
	case class Bishop(colour: Colour) extends Piece {
		val distanceLimit = 7
		val directions = List(
			(-1, -1), /*    */ (-1, 1),
			/*     */ /*    */ /*    */
			 (1, -1), /*    */  (1, 1)
		)			
	}
	case class Knight(colour: Colour) extends Piece {
		val distanceLimit = 1
		val directions = List(
			(1, 2), (2, 1), (2, -1), (1, -2),
			(-1, -2), (-2, -1), (-2, 1), (-1, 2)
		)
	}
	case class Pawn(colour: Colour) extends Piece {
		val distanceLimit = 1
		val directions = if (colour == WHITE) List((1, -1), (1, 0), (1, 1), (2, 0))
						 else List((-1, -1), (-1, 0), (-1, 1), (-2, 0))
	}
}