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
  var board = Array.fill[Player](DIM, DIM){ Nobody } 
}