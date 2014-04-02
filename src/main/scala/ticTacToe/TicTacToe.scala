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
case object Nobody extends Player(' ')

class Board {
  val DIM = 3
  var cels = Array.fill[Player](DIM, DIM) { Nobody } 
  var currentPlayer: Player = PlayerX

  def detectWinner() = { 
  	var result = detectRowWinner
  	if (result == Nobody) result = detectColumnWinner
  	if (result == Nobody) result = detectDiagonalWinner  	
  	result
  }

  private def detectRowWinner(): Player = {
	for (row <- 0 until DIM) {
	  if (cels(row)(0) != Nobody
	  	  && cels(row)(0) == cels(row)(1) 
	  	  && cels(row)(0) == cels(row)(2)) {
	  	return cels(row)(0)
	  }
	}
	Nobody
  }

  private def detectColumnWinner(): Player = {
	for (column <- 0 until DIM) {
	  if (cels(0)(column) != Nobody
	  	  && cels(0)(column) == cels(1)(column)
	  	  && cels(0)(column) == cels(2)(column)) {
	  	return cels(0)(column)
	  }
	}
	Nobody
  }

  private def detectDiagonalWinner(): Player = {
	if (cels(0)(0) != Nobody
	  	&& cels(0)(0) == cels(1)(1)
	  	&& cels(0)(0) == cels(2)(2)) {
	  return cels(0)(0)
	}
	if (cels(0)(2) != Nobody
	  	&& cels(0)(2) == cels(1)(1)
	  	&& cels(0)(2) == cels(2)(0)) {
	  return cels(0)(2)
	}
	Nobody
  }

  def detectTie() =  detectWinner == Nobody && countEmptyCells == 0

  private def countEmptyCells() = {
  	var count = 0
  	for (row <- 0 until DIM) {
  		for (column <- 0 until DIM) {
  			if (cels(row)(column) == Nobody) count += 1
  		}
  	}
  	count
  }

  def move(row: Int, column: Int) {
  	cels(row)(column) = currentPlayer
  	currentPlayer = if (currentPlayer == PlayerX) PlayerO else PlayerX
  }
}