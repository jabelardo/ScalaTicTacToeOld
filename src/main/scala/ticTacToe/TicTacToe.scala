package ticTacToe
/* 
 * Write a game that will take a tic-tac-toe board with X, O, and blank 
 * characters and detect the winner or whether there is a tie or no winner yet.
 * Use classes where appropriate.
 *
 * Bonus problem: Let two players play tic-tac-toe.
 */
 import scala.collection.mutable._

abstract class Player(value: Char) { def value(): Char = value }
case object PlayerX extends Player('X')
case object PlayerO extends Player('O')
case object Nobody extends Player('.')

class Board {
  val DIM = 3
  var cells = Array.fill[Player](DIM, DIM) { Nobody } 

  def detectWinner() = { 
  	var result = detectRowWinner
  	if (result == Nobody) result = detectColumnWinner
  	if (result == Nobody) result = detectDiagonalWinner  	
  	result
  }

  private def detectRowWinner(): Player = {
	for (row <- 0 until DIM) {
	  if (cells(row)(0) != Nobody
	  	  && cells(row)(0) == cells(row)(1) 
	  	  && cells(row)(0) == cells(row)(2)) {
	  	return cells(row)(0)
	  }
	}
	Nobody
  }

  private def detectColumnWinner(): Player = {
	for (column <- 0 until DIM) {
	  if (cells(0)(column) != Nobody
	  	  && cells(0)(column) == cells(1)(column)
	  	  && cells(0)(column) == cells(2)(column)) {
	  	return cells(0)(column)
	  }
	}
	Nobody
  }

  private def detectDiagonalWinner(): Player = {
	if (cells(0)(0) != Nobody
	  	&& cells(0)(0) == cells(1)(1)
	  	&& cells(0)(0) == cells(2)(2)) {
	  return cells(0)(0)
	}
	if (cells(0)(2) != Nobody
	  	&& cells(0)(2) == cells(1)(1)
	  	&& cells(0)(2) == cells(2)(0)) {
	  return cells(0)(2)
	}
	Nobody
  }

  def detectTie() =  detectWinner == Nobody && countEmptyCells == 0

  private def countEmptyCells() = {
  	var count = 0
  	for (row <- 0 until DIM) {
  		for (column <- 0 until DIM) {
  			if (cells(row)(column) == Nobody) count += 1
  		}
  	}
  	count
  }

  def possibleMoves() = {
  	var builder = new ArrayBuffer[(Int,Int)]()
	for (row <- 0 until DIM) {
  	  for (column <- 0 until DIM) {
	    if (cells(row)(column) == Nobody) {
	      builder += Pair(row, column)
	    }
	  }  		
  	}
  	builder.toArray
  }
}

class Game {
  var board = new Board
  var currentPlayer: Player = PlayerX
  var lastPlayer: Player = null

  def move(row: Int, column: Int) {
  	board.cells(row)(column) = currentPlayer
  	lastPlayer = currentPlayer
  	currentPlayer = if (currentPlayer == PlayerX) PlayerO else PlayerX
  }

  def possibleMoves() = board.possibleMoves

  def cell(row: Int, column: Int) = board.cells(row)(column)

  def detectWinner() = board.detectWinner

  def detectTie() = board.detectTie
}

object GameRunner {

	var game = new Game
	val DIM = 3
	
	def printBoard() {
		println("  0 1 2")
		for (row <- 0 until DIM) {
			print(row)
			for (column <- 0 until DIM) {
				print(" " + game.cell(row, column).value)
			}
			println()
		}
	}

	def printPossibleMoves() {
		val possibleMoves = game.possibleMoves
		print("Moves: ")
		for (i  <- 0 until possibleMoves.size) {
			print(i + ":" + possibleMoves(i) + " ")
		}
		println
	}

	def printInputPrompt() {
		printPossibleMoves
		print("Player " + game.currentPlayer.value +
			    ", enter your move number or 'q' to quit: ")
	}

	def safeStringToInt(str: String): Option[Int] = try {
   			Some(str.toInt)
		} catch {
   			case ex: NumberFormatException => None
	}

	def main(args: Array[String]) {
		printBoard
		printInputPrompt

 		for (ln <- io.Source.stdin.getLines) {
            if (ln equalsIgnoreCase "q") return
            val possibleMoves = game.possibleMoves
            val imput = safeStringToInt(ln)
            if (imput.isEmpty || imput.get < 0 || imput.get > possibleMoves.size - 1) {
            	print("Invalid imput: ")
        	} else {
        		val move = possibleMoves(imput.get)
        		game.move(move._1, move._2)
        		printBoard
        		if (game.detectWinner != Nobody) {
        			println("The winner is " + game.lastPlayer.value)
        			return
        		} else if (game.detectTie) {
        			println("The game ends in a tie")
        			return
    			} else {
    				printInputPrompt
    			}
        	}
 		}
 	}
}