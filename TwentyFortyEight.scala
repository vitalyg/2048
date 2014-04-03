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
	def filter(p: Int => Boolean) = row.filter(p)
	def getEmptyIndices: ArrayBuffer[Int] = row.zipWithIndex.filter(_._1 == 0).map(_._2)
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

	def expectMinMax(board: Board, depth: Int, heuristic: Board => Int, isTurn: Boolean = true, direction: Int = -1): (Float, Int) = {
		println(s"Depth is $depth\tDirection is $direction")
		println(board)

		if (board.gameOver) return (Int.MinValue, direction)
		if (board.gameWon) return (Int.MaxValue, direction)
		if (depth == 0) return (heuristic(board), direction)

		if (isTurn) 
			board
				.getDirectionChildren
				.map{ case (c, d) => expectMinMax(c, depth - 1, heuristic, !isTurn, d) }
				.maxBy(_._1)
		else {
			val score = board
				.getAddElementChildren
				.map{ case (c, p) => p * expectMinMax(c, depth - 1, heuristic, !isTurn, direction)._1 }
				.sum
			(score, direction)
		}
	}
}

case class Board(board: Board.Matrix) {
	val rows = 4
	val cols = 4

	def apply(t: (Int, Int)): Int = board(t._1)(t._2)
	def apply(x: Int, y: Int): Int = apply((x, y))
	def update(t: (Int, Int), n: Int): Unit = board(t._1)(t._2) = n
	
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

	def getLegalDirections: Seq[Int] = (0 to 3).toSeq.filter(isLegalMove)
	def getDirectionChildren: Seq[(Board, Int)] = {
		def cloneAndMove(d: Int) = {
			val newBoard = this.clone
			newBoard.move(d)
			newBoard
		}
		
		getLegalDirections
			.map(d => (cloneAndMove(d), d))			
	}
	def getAddElementChildren: Seq[(Board, Float)] = {
		def cloneAndAddElements(cell: (Int, Int)) = {
			val board2 = this.clone
			board2(cell) = 2
			val board4 = this.clone
			board4(cell) = 4
			Seq((board2, 0.9f), (board4, 0.1f))
		}
		val cells = getAllEmptyCells.flatMap(cloneAndAddElements)
		val sumOfCells = cells.map(_._2).sum
		cells.map{ case (c, p) => (c, p / sumOfCells) }
	}
	def gameOver: Boolean = getLegalDirections.isEmpty
	def gameWon: Boolean = !board.flatMap(_.row.find(_ == 2048)).isEmpty

	def move(direction: Int) = Board.move(this, direction)
	def moveIfLegal(direction: Int, debug: Boolean = false): Boolean = {		
		if (!this.isLegalMove(direction)) return false

		move(direction)

		addNewTile

		if (debug) println(this)

		true
	}
	def getRandomCell: (Int, Int) = (Board.rand.nextInt(4), Board.rand.nextInt(4))
	def getRandomEmptyCell: (Int, Int) = {
		var newCell = getRandomCell
		while (this(newCell) != 0) newCell = getRandomCell
		newCell
	}
	def getAllEmptyCells: Seq[(Int, Int)] = {
		board
			.zipWithIndex
			.flatMap{ case (r, x) => r.getEmptyIndices.map(y => (x, y)) }
	}
	def addNewTile() = this(getRandomEmptyCell) = Board.getElement

	def triangleScore: Int = {
		val cells = for (x <- 0 until rows; y <- 0 until cols) yield ((x + y + 2) * this(x, y))
		cells.sum
	}

	override def clone = Board(board.map(_.clone()))
	override def toString = board.mkString("\n") + "\n"
}

object TwentyFortyEight extends App {
	val board = Board.create
	println(Board.expectMinMax(board, 2, _.triangleScore))
}