package colour {
	abstract class Colour
	object WHITE extends Colour
	object BLACK extends Colour
}

package pieces {
	import colour._

	abstract class Piece(colour: Colour) {
		val isWhite = colour == WHITE
		override def toString = this match {
			case King(colour) => if(isWhite) "K" else "k"
			case Queen(colour) => if(isWhite) "Q" else "q"
			case Rook(colour) => if(isWhite) "R" else "r"
			case Bishop(colour) => if(isWhite) "B" else "b"
			case Knight(colour) => if(isWhite) "N" else "n"
			case Pawn(colour) => if(isWhite) "P" else "p"
		}
	}

	case class King(colour: Colour) extends Piece(colour)
	case class Queen(colour: Colour) extends Piece(colour)
	case class Rook(colour: Colour) extends Piece(colour)
	case class Bishop(colour: Colour) extends Piece(colour)
	case class Knight(colour: Colour) extends Piece(colour)
	case class Pawn(colour: Colour) extends Piece(colour)
}