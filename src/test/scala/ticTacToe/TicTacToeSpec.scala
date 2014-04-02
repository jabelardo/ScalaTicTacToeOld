package ticTacToe
/* 
 * Write a game that will take a tic-tac-toe board with X, O, and blank 
 * characters and detect the winner or whether there is a tie or no winner yet.
 * Use classes where appropriate.
 *
 * Bonus problem: Let two players play tic-tac-toe.
 */
import org.specs2.mutable._
import scala.collection.mutable._

class TicTacToeSpec extends Specification {
 	
  	def buildBoard(array: Array[Array[Char]]): Array[Array[Player]] = {
  		var boardBuilder = new ArrayBuffer[Array[Player]]()
		 for (rowIdx <- 0 until array.size) {
		 	val row = array(rowIdx)
		 	var rowBuilder = new ArrayBuffer[Player]()
	   	    for (columnIdx <- 0 until row.size) {
	   	    	if (row(columnIdx) == 'X') {
					rowBuilder += PlayerX
   	    		} else if (row(columnIdx) == 'O') {
					rowBuilder += PlayerO
    			} else {
					rowBuilder += Nobody
    			}
		   }  		
		   boardBuilder += rowBuilder.toArray
	   	}
	   	boardBuilder.toArray
  	}

 	"board" should {
    	"be initialized as a 3 X 3 array of blank characters" in {
    		val ticTacToe = new TicTacToe
    		val blankBoard = buildBoard(Array(Array(' ',' ',' '),
					                          Array(' ',' ',' '),
					                          Array(' ',' ',' ')))
      		ticTacToe.board.deep must beEqualTo(blankBoard.deep)
    	}
  	}

	"detectWinner" should {
  		"return Nobody if there is not a winner" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.detectWinner must beEqualTo(Nobody)
  		}
  		"return PlayerX if it has a winning row" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.board = buildBoard(Array(Array('X','X','X'),
  		                            		   Array('O',' ','O'),
  		                            		   Array(' ',' ',' ')))
  			ticTacToe.detectWinner must beEqualTo(PlayerX)
  		}
  		"return PlayerO if it has a winning column" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.board = buildBoard(Array(Array('O',' ','X'),
  		                            	 	   Array('O','X',' '),
  		                            	 	   Array('O',' ','X')))
  			ticTacToe.detectWinner must beEqualTo(PlayerO)
  		}
  		"return PlayerX if it has a winning descending diagonal" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.board = buildBoard(Array(Array('X',' ',' '),
  		                            		   Array('O','X','O'),
  		                            		   Array(' ',' ','X')))
  			ticTacToe.detectWinner must beEqualTo(PlayerX)
  		}
  		"return PlayerO if it has a winning ascending diagonal" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.board = buildBoard(Array(Array('X',' ','O'),
	  		                            	   Array(' ','O','X'),
	  		                            	   Array('O',' ','X')))
  			ticTacToe.detectWinner must beEqualTo(PlayerO)
  		}
  	}

	"detectTie" should {
  		"return true if the board is full and there is not a winner" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.board = buildBoard(Array(Array('X','O','X'),
  		                            		   Array('O','O','X'),
  		                            		   Array('X','X','O')))
  			ticTacToe.detectTie must beEqualTo(true)
  		}
  	}

  	"currentPlayer" should {
    	"be initialized with PlayerX" in {
    		(new TicTacToe).currentPlayer must 
      			beEqualTo(PlayerX)	
    	}
  	}

  	"move" should {
  		"set current player letter to the move position" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.move(0,0)
  			ticTacToe.board(0)(0) must beEqualTo(PlayerX)
  		}
  		"turn currentPlayer from X to O" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.move(0,0)
  			ticTacToe.currentPlayer must beEqualTo(PlayerO)
  		}
  		"turn currentPlayer from O to X" in {
  			val ticTacToe = new TicTacToe
  			ticTacToe.move(0,0)
  			ticTacToe.move(0,1)
  			ticTacToe.currentPlayer must beEqualTo(PlayerX)
  		}
  	}

}