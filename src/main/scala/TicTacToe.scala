/**
 * Created by arunks on 15/02/15.
 */
object TicTacToe {

  val N = 5

  def factorial(n: Int) = {
    def factorialHelper(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else factorialHelper(n - 1, acc + n)
    }
    factorialHelper(n, 0)
  }

  type Board = List[Char]
  type PlayerSymbol = Char
  type Position = Int
  type Score = Int
  type Move = (Score, Position)

  val horizontal = {
    for (i <- 0 until N * N) yield i
  }.toList
  val vertical = {
    for (i <- 0 until N; j <- i until (N * N) by N) yield j
  }.toList
  val diagonal1 = {
    for (i <- 0 until (N * N) by (N + 1)) yield i
  }.toList
  val diagonal2 = {
    for (i <- (N - 1) until ((N * N) - 1) by (N - 1)) yield i
  }.toList

  def opponentPlayerSymbol(playerSymbol: PlayerSymbol): PlayerSymbol = {
    if (playerSymbol == 'X') 'O'
    else 'X'
  }

  def hasPlayerWon(board: Board, playerSymbol: PlayerSymbol) = {
    def hasWon(series: List[Int]): Boolean = {
      series.map(i => board(i) == playerSymbol).reduce((b1, b2) => b1 && b2)
    }

    def hasWonInThisSeries(series: List[Int]): Boolean = {
      if (series.isEmpty) false
      else if (hasWon(series.take(N))) true
      else hasWonInThisSeries(series.drop(N))
    }

    hasWonInThisSeries(horizontal) || hasWonInThisSeries(vertical) || hasWonInThisSeries(diagonal1) || hasWonInThisSeries(diagonal2)
  }

  def hasSomeOneWon(board: Board) = {
    hasPlayerWon(board, 'X') || hasPlayerWon(board, 'O')
  }

  def isGameDraw(board: Board) = {
    !board.contains('-')
  }

  def printBoard(board: Board) = {
    def printSet(set: Board) = {
      for (i <- 0 until N) print(set(i) + " ")
      println()
    }
    for (i <- 0 until N) printSet(board.slice(i * N, (i * N) + N))
  }


  def moveSelector(board: Board, maxPlayerSymbol: PlayerSymbol, round: Int): Int = {
    val minPlayerSymbol = opponentPlayerSymbol(maxPlayerSymbol)
    val isMaxPlayerSymbol: (PlayerSymbol => Boolean) = ((i: PlayerSymbol) => i == maxPlayerSymbol)
    val isMinPlayerSymbol: (PlayerSymbol => Boolean) = ((i: PlayerSymbol) => i == minPlayerSymbol)

    val depth = 6

    def possibleMoves(board: Board, _toPlay: PlayerSymbol): List[(Board, Position)] = {
      {
        for (i <- 0 until N * N; if board(i) == '-') yield (board.updated(i, _toPlay), i)
      }.toList
    }

    def possibleMovesForRoundOne(board: Board, _toPlay: PlayerSymbol): List[(Board, Position)] = {
      ({
        for (i <- 0 to (N - 1) / 2) yield i
      }.toList ::: {
        for (i <- ((N - 1) / 2) + N to ((N * N) - 1) / 2 by N) yield i
      }.toList).map((i) => (board.updated(i, _toPlay), i))
    }

    def possibleBoards(board: Board, _toPlay: PlayerSymbol): List[Board] = {
      possibleMoves(board, _toPlay).unzip._1
    }

    def score(board: Board): Int = {
      def scoreOfSeries(series: List[Int]): Int = {
        def scoreOfSet(series: List[Int]): Int = {
          val temp = series.map((i) => board(i))

//          if(temp.isEmpty)
//            println(temp.isEmpty)
                    if (temp.count(isMinPlayerSymbol) == N) -(N * N * 4)
                    else if (temp.contains(minPlayerSymbol)) 0
                    else if (temp.count(isMaxPlayerSymbol) == N) ((N + N) * N)
                    else 2 * factorial(temp.count(isMaxPlayerSymbol))
          //          (temp.filter((i)=>i==maxPlayerSymbol).size) - (temp.filter((i)=>i==minPlayerSymbol).size)
//          temp.map((i) => if (i == maxPlayerSymbol) 1 else if (i == minPlayerSymbol) -1 else 0).reduce((a, b) => a + b)
        }

        def helper(series: List[Int], acc: Score): Int = {
          if(series.isEmpty) acc
          else helper(series.drop(N), acc + scoreOfSet(series.take(N)))
        }

        //        (0 until N).map((i) => scoreOfSet(series.slice(i * N, (i * N) + N))).reduce((a, b) => a + b)
        helper(series, 0)
      }
      scoreOfSeries(horizontal) + scoreOfSeries(vertical) + scoreOfSeries(diagonal1) + scoreOfSeries(diagonal2)
    }

    def minMaxAB(board: Board, depth: Int, alpha: Int, beta: Int, isMaxPlayer: Boolean): Int = {
      if (depth == 0 || hasSomeOneWon(board) || isGameDraw(board)) score(board)
      else if (isMaxPlayer) {
        val bestScore = -9999
        val _possibleMoves = possibleBoards(board, if (isMaxPlayer) 'X' else 'O')

        def helper(bestScore: Score, boards: List[Board], alpha: Score): Score = {
          if (boards.isEmpty) bestScore
          else {
            val currentScore = minMaxAB(boards.head, depth - 1, alpha, beta, false)
            val newBestScore = Math.max(bestScore, currentScore)
            val newAlpha = Math.max(newBestScore, alpha)
            if (beta <= newAlpha) newAlpha
            else helper(newBestScore, boards.tail, newAlpha)
          }
        }
        helper(bestScore, _possibleMoves, alpha)
      } else {
        val bestScore = 9999
        val _possibleMoves = possibleBoards(board, if (isMaxPlayer) 'X' else 'O')

        def helper(bestScore: Score, boards: List[Board], beta: Score): Score = {
          if (boards.isEmpty) bestScore
          else {
            val currentScore = minMaxAB(boards.head, depth - 1, alpha, beta, true)
            val newBestScore = Math.min(bestScore, currentScore)
            val newBeta = Math.min(newBestScore, beta)
            if (newBeta <= alpha) newBeta
            else helper(newBestScore, boards.tail, newBeta)
          }
        }
        helper(bestScore, _possibleMoves, beta)
      }
    }

    def helper(moves: List[(Board, Position)], alpha: Score, acc: List[Move]): List[Move] = {
      if (moves.isEmpty) acc
      else {
        val currentScore = minMaxAB(moves.head._1, depth, alpha, 9999, false)
        helper(moves.tail, Math.max(currentScore, alpha), acc ::: List((currentScore, moves.head._2)))
      }
    }
    //    val rootNodes = {if(round==1) possibleMoves(board, maxPlayerSymbol) else possibleMoves(board, maxPlayerSymbol)}
    helper(possibleMoves(board, maxPlayerSymbol), -9999, List()).maxBy((move) => move._1)._2
  }

  def main(args: Array[String]) = {

    def play(board: Board, position: Int, player: PlayerSymbol): Board = {
      board.updated(position, player)
    }

    def game(board: Board, toPlay: PlayerSymbol, time: Long, round: Int): Unit = {
      val currentTime = System.currentTimeMillis()
      println("Time taken = " + ((currentTime - time)))
      printBoard(board)
      println()
      if (hasPlayerWon(board, opponentPlayerSymbol(toPlay))) println(opponentPlayerSymbol(toPlay) + " has Won!!")
      else if (isGameDraw(board)) println("Game Draw!!")
      else if (toPlay == 'X') game(play(board, moveSelector(board, toPlay, round), toPlay), opponentPlayerSymbol(toPlay), currentTime, round + 1)
      else game(play(board, Console.readInt() - 1, toPlay), opponentPlayerSymbol(toPlay), currentTime, round + 1)
    }

    val initialBoard = {
      for (i <- 0 until N * N) yield '-'
    }.toList
    game(initialBoard, 'X', System.currentTimeMillis(), 0)
  }

}
