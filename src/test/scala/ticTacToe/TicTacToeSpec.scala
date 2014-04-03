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
 	
  	def buildCells(array: Array[Array[Char]]): Array[Array[Player]] = {
  		var cellsBuilder = new ArrayBuffer[Array[Player]]()
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
		   cellsBuilder += rowBuilder.toArray
	   	}
	   	cellsBuilder.toArray
  	}

 	"board" should {
    	"be initialized as a 3 X 3 array of blank characters" in {
    		val board = new Board
    		val blankBoard = buildCells(Array(Array(' ',' ',' '),
					                          Array(' ',' ',' '),
					                          Array(' ',' ',' ')))
      		board.cells.deep must beEqualTo(blankBoard.deep)
    	}
  	}

	"detectWinner" should {
  		"return Nobody if there is not a winner" in {
  			val board = new Board
  			board.detectWinner must beEqualTo(Nobody)
  		}
  		"return PlayerX if it has a winning row" in {
  			val board = new Board
  			board.cells = buildCells(Array(Array('X','X','X'),
  		                            		   Array('O',' ','O'),
  		                            		   Array(' ',' ',' ')))
  			board.detectWinner must beEqualTo(PlayerX)
  		}
  		"return PlayerO if it has a winning column" in {
  			val board = new Board
  			board.cells = buildCells(Array(Array('O',' ','X'),
  		                            	 	   Array('O','X',' '),
  		                            	 	   Array('O',' ','X')))
  			board.detectWinner must beEqualTo(PlayerO)
  		}
  		"return PlayerX if it has a winning descending diagonal" in {
  			val board = new Board
  			board.cells = buildCells(Array(Array('X',' ',' '),
  		                            		   Array('O','X','O'),
  		                            		   Array(' ',' ','X')))
  			board.detectWinner must beEqualTo(PlayerX)
  		}
  		"return PlayerO if it has a winning ascending diagonal" in {
  			val board = new Board
  			board.cells = buildCells(Array(Array('X',' ','O'),
	  		                            	   Array(' ','O','X'),
	  		                            	   Array('O',' ','X')))
  			board.detectWinner must beEqualTo(PlayerO)
  		}
  	}

	"detectTie" should {
  		"return true if the board is full and there is not a winner" in {
  			val board = new Board
  			board.cells = buildCells(Array(Array('X','O','X'),
  		                            		   Array('O','O','X'),
  		                            		   Array('X','X','O')))
  			board.detectTie must beEqualTo(true)
  		}
  	}

  	"currentPlayer" should {
    	"be initialized with PlayerX" in {
    		val game = new Game
    		game.currentPlayer must beEqualTo(PlayerX)	
    	}
  	}

  	"move" should {
  		"set current player letter to the move position" in {
  			val game = new Game
  			game.move(0,0)
  			game.board.cells(0)(0) must beEqualTo(PlayerX)
  		}
  		"turn currentPlayer from X to O" in {
  			val game = new Game
  			game.move(0,0)
  			game.currentPlayer must beEqualTo(PlayerO)
  		}
  		"turn currentPlayer from O to X" in {
  			val game = new Game
  			game.move(0,0)
  			game.move(0,1)
  			game.currentPlayer must beEqualTo(PlayerX)
  		}
  	}




}