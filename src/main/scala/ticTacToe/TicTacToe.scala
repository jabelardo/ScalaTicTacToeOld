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
  var cells = Array.fill[Player](DIM, DIM) { Nobody } 
  var currentPlayer: Player = PlayerX

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

  def move(row: Int, column: Int) {
  	cells(row)(column) = currentPlayer
  	currentPlayer = if (currentPlayer == PlayerX) PlayerO else PlayerX
  }
}