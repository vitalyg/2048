import util.Random
import scala.collection.mutable.ArrayBuffer

object Row {
	def create: Row = Row(ArrayBuffer.fill[Int](4)(0))
}

case class Row(row: ArrayBuffer[Int]) {
	def apply(i: Int): Int = row(i)
	def update(i: Int, n: Int) = row(i) = n
	def reverse() = {
		def swap(i: Int, j: Int) = {
			val tmp = row(i)
			row(i) = row(j)
			row(j) = tmp
		}
		swap(0, 3)
		swap(1, 2)
	}
	def left() = {
			var last = 0
			var put = 0
			for (i <- 0 until row.size)
				if (row(i) != 0) {
					if (last == 0) {// spotted the first element
						last = row(i)
						row(put) = last
						put += 1
					}
					else if (last == row(i)) { // there is a pair
						row(put - 1) += last
						last = 0
					}
					else { // both elements do not match
						row(put) = row(i)
						put += 1
						last = row(i)	
					}
				}
			for (i <- put until row.size)
				row(i) = 0
		}
	def right() = {
		reverse()
		left()
		reverse()
	}
	override def clone = Row(row.clone())
	override def toString = row.map(c => "%4d".format(c)).mkString(" | ").replace('0', ' ')
}

object Board {
	type Matrix = ArrayBuffer[Row]

	val rand = new Random()

	def getElement: Int = if (Board.rand.nextDouble() < 0.9) 2 else 4	
	def create: Board = {		
		def getRandomCell = (Board.rand.nextInt(4), rand.nextInt(4))
		def getRandomDifferentCell(c: (Int, Int)) = {
			var newCell = getRandomCell
			while (newCell == c) newCell = getRandomCell
			newCell
		}
		val first = getRandomCell
		var second = getRandomDifferentCell(first)
		
		val board = ArrayBuffer.fill[Row](4)(Row.create)
		board(first._1)(first._2) = getElement
		board(second._1)(second._2) = getElement
		Board(board)
	}
	def move(board: Board, direction: Int) = {
		def left() = board.board.foreach(_.left())
		def right() = board.board.foreach(_.right())
		def up() = {
			board.transpose()
			left()
			board.transpose()
		}
		def down() = {
			board.transpose()
			right()
			board.transpose()
		}

		direction match {
			case 0 => left()
			case 1 => right()
			case 2 => up()
			case 3 => down()
		}
	}
}

case class Board(board: Board.Matrix) {
	val rows = 4
	val cols = 4

	def apply(t: (Int, Int)) = board(t._1)(t._2)
	def update(t: (Int, Int), n: Int) = board(t._1)(t._2) = n
	
	def transpose() = {
		def swap(x: (Int, Int), y: (Int, Int)) = {
			val tmp = this(x)
			this(x) = this(y)
			this(y) = tmp
		}
		for (x <- 0 until rows; y <- 0 until cols; if x < y)
			swap((x, y), (y, x))
	}

	def isLegalMove(direction: Int): Boolean = {
		val copy = this.clone
		val moved = Board.move(copy, direction)	
		this != copy
	}

	def getLegalMoves: Seq[Int] = (0 to 3).toSeq.filter(isLegalMove)
	def gameOver: Boolean = getLegalMoves.isEmpty
	def gameWon: Boolean = !board.map(_.row.find(_ == 2048)).isEmpty

	def move(direction: Int, debug: Boolean = false): Boolean = {		
		if (!this.isLegalMove(direction)) return false

		Board.move(this, direction)

		addNewTile

		if (debug) println(this)

		true
	}
	def getRandomCell = (Board.rand.nextInt(4), Board.rand.nextInt(4))
	def getRandomEmptyCell = {
		var newCell = getRandomCell
		while (this(newCell) != 0) newCell = getRandomCell
		newCell
	}
	def addNewTile() = this(getRandomEmptyCell) = Board.getElement

	def triangleScore: Int = {
		val cells = for (x <- 0 until rows; y <- 0 until cols) yield ((x + y + 2) * this((x, y)))
		cells.sum
	}

	override def clone() = Board(board.map(_.clone()))
	override def toString() = board.mkString("\n") + "\n"
}

object TwentyFortyEight extends App {
	val board = Board.create
	println(board)
	for (i <- 0 until 10) {		
		board.move(1, true)
		board.move(3, true)
	}
}