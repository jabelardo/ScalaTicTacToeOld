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

class TicTacToe {
  val DIM = 3
  var board = Array.fill[Player](DIM, DIM) { Nobody } 

  def detectWinner() = { 
  	var result = detectRowWinner
  	if (result == Nobody) result = detectColumnWinner
  	if (result == Nobody) result = detectDiagonalWinner  	
  	result
  }

  private def detectRowWinner(): Player = {
	for (row <- 0 until DIM) {
	  if (board(row)(0) != Nobody
	  	  && board(row)(0) == board(row)(1) 
	  	  && board(row)(0) == board(row)(2)) {
	  	return board(row)(0)
	  }
	}
	Nobody
  }

  private def detectColumnWinner(): Player = {
	for (column <- 0 until DIM) {
	  if (board(0)(column) != Nobody
	  	  && board(0)(column) == board(1)(column)
	  	  && board(0)(column) == board(2)(column)) {
	  	return board(0)(column)
	  }
	}
	Nobody
  }

  private def detectDiagonalWinner(): Player = {
	if (board(0)(0) != Nobody
	  	&& board(0)(0) == board(1)(1)
	  	&& board(0)(0) == board(2)(2)) {
	  return board(0)(0)
	}
	if (board(0)(2) != Nobody
	  	&& board(0)(2) == board(1)(1)
	  	&& board(0)(2) == board(2)(0)) {
	  return board(0)(2)
	}
	Nobody
  }
}