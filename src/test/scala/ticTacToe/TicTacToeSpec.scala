package ticTacToe
/* 
 * Write a game that will take a tic-tac-toe board with X, O, and blank 
 * characters and detect the winner or whether there is a tie or no winner yet.
 * Use classes where appropriate.
 *
 * Bonus problem: Let two players play tic-tac-toe.
 */
import org.specs2.mutable._
class TicTacToeSpec extends Specification {
 	
 	"board" should {
    	"be initialized as a 3 X 3 array of blank characters" in {
      		(new TicTacToe).board.deep must 
      			beEqualTo(Array.fill[Char](3,3){' '}.deep)
    	}
  	}
}