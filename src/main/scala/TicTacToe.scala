object TicTacToe {

  val N = 5
  val winning = 4
  val depth = 4

  type Player = Char
  type PlayerSymbol = Char
  type Board = Vector[PlayerSymbol]
  type Position = Int
  type Score = Int
  type Move = (Score, Position)
  type Series = List[Int]

  val MaxScore = 999999

  val XPlayer = 'X'
  val OPlayer = 'O'
  val blank = '-'

  def getSeries(start: Int, length: Int, delta: Int): List[Int] = {
    {
      for (i <- 0 until length) yield start + (i * delta)
    }.toList
  }

  val horizontal = {
    for (i <- 0 until N) yield getSeries(i * N, N, 1)
  }.toList

  val vertical = {
    for (i <- 0 until N) yield getSeries(i, N, N)
  }.toList

  val diagonal1 = {
    val list = {
      for (i <- winning until N) yield (getSeries(i - 1, i, N - 1), getSeries((i - 1) + (((N * N) - 1) - ((N + 1) * (i - 1))), i, N - 1))
    }.toList.unzip
    list._1 ::: list._2 ::: List({
      for (i <- (N - 1) until ((N * N) - 1) by (N - 1)) yield i
    }.toList)
  }

  val diagonal2 = {
    val list = {
      for (i <- winning until N) yield (getSeries((N - i), i, N + 1), getSeries((N - i) + (((N - 1) * (N - 1)) - ((N - 1) * (i - 1))), i, N + 1))
    }.toList.unzip
    list._1 ::: list._2 ::: List({
      for (i <- 0 until (N * N) by (N + 1)) yield i
    }.toList)
  }

  val allPossibleSeries = horizontal ::: vertical ::: diagonal1 ::: diagonal2

  def opponentPlayerSymbol(playerSymbol: PlayerSymbol): PlayerSymbol = {
    if (playerSymbol == XPlayer) OPlayer
    else XPlayer
  }

  def hasWon(series: List[Boolean]): Boolean = {
    def helper(series: List[Boolean], acc: Int): Int = {
      if (series.isEmpty || acc >= winning) acc
      else helper(series.tail, {
        if (series.head == true) acc + 1 else 0
      })
    }
    helper(series, 0) >= winning
  }

  def hasPlayerWon(board: Board, playerSymbol: Player) = allPossibleSeries.exists(series => hasWon(series.map(i => board(i) == playerSymbol)))

  def hasSomeOneWon(board: Board) = hasPlayerWon(board, XPlayer) || hasPlayerWon(board, OPlayer)

  def isGameDraw(board: Board) = !board.contains(blank)

  def moveSelector(board: Board, maxPlayerSymbol: Player, round: Int): Int = {
    val minPlayerSymbol = opponentPlayerSymbol(maxPlayerSymbol)

    def possibleMoves(board: Board, _toPlay: Player): List[(Board, Position)] = {
      {
        for (i <- 0 until N * N; if board(i) == blank) yield (board.updated(i, _toPlay), i)
      }.toList
    }

    def possibleBoards(board: Board, _toPlay: Player): List[Board] = {
      possibleMoves(board, _toPlay).unzip._1
    }

    def score(board: Board): Int = {

      def scoreOfSeries(series: Series): Score = {

        def groupSeries(series: List[PlayerSymbol], acc: List[(PlayerSymbol, Int)], lastSymbol: (PlayerSymbol, Int)): List[(PlayerSymbol, Int)] = {
          if(series.isEmpty) lastSymbol :: acc
          else if(series.head == lastSymbol._1) groupSeries(series.tail, acc, (lastSymbol._1, lastSymbol._2 + 2))
          else groupSeries(series.tail, lastSymbol :: acc, (series.head, 1))
        }

        def getScore(series: List[(PlayerSymbol, Int)], _toPlay: PlayerSymbol): Score = {
          def helper(series: List[(PlayerSymbol, Int)], accScore: List[Score], currentScore: Score): Score = {
            if(series.isEmpty) (currentScore :: accScore).max
            else {
              if(series.head._1 == _toPlay) helper(series.tail, accScore, currentScore + (Math.pow(5, series.head._2).toInt))
              else if(series.head._1 == blank) helper(series.tail, accScore, currentScore + (Math.pow(2, series.head._2).toInt))
              else helper(series.tail, currentScore :: accScore, 0)
            }
          }
          helper(series, List(), 0)
        }

        val groupedSeries = groupSeries(series.map((i) => board(i)), List(), (' ', 0))
        val maxPlayerScore = getScore(groupedSeries, maxPlayerSymbol)
        val minPlayerScore = - getScore(groupedSeries, minPlayerSymbol)

        if(Math.abs(minPlayerScore)>maxPlayerScore) minPlayerScore
        else maxPlayerScore

      }

      def helper(series: List[Series], acc: Int): Int = {
        if(series.isEmpty) acc
        else {
          val score = scoreOfSeries(series.head)
          if(Math.abs(score) == MaxScore) score
          else helper(series.tail, acc + score)
        }
      }

      helper(allPossibleSeries, 0)
    }

    def minMaxAB(board: Board, depth: Int, alpha: Int, beta: Int, isMaxPlayer: Boolean): Int = {
      if (depth == 0 || isGameDraw(board) || hasSomeOneWon(board)) (depth + 1) * score(board)
      else if (isMaxPlayer) {
        val bestScore = -MaxScore
        val _possibleMoves = possibleBoards(board, XPlayer)

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
        val bestScore = MaxScore
        val _possibleMoves = possibleBoards(board, OPlayer)

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
        val currentScore = minMaxAB(moves.head._1, depth, alpha, MaxScore, false)
        helper(moves.tail, Math.max(currentScore, alpha), acc ::: List((currentScore, moves.head._2)))
      }
    }
    val moves = helper(possibleMoves(board, maxPlayerSymbol), -MaxScore, List())
    //    val maxScore = moves.maxBy(_._1)._1
    //    val filteredList = moves.filter(_._1 == maxScore)
    //    filteredList(new Random().nextInt(filteredList.size))._2
    moves.maxBy(_._1)._2
  }

  def printBoard(board: Board) = {
    def printSet(set: Board) = {
      for (i <- 0 until N) print(set(i) + " ")
      println()
    }
    for (i <- 0 until N) printSet(board.slice(i * N, (i * N) + N))
  }

  def main(args: Array[String]) = {

    def play(board: Board, position: Int, player: Player): Board = {
      board.updated(position, player)
    }

    def game(board: Board, toPlay: Player, time: Long, round: Int): Unit = {
      val currentTime = System.currentTimeMillis()
      println("Time taken = " + ((currentTime - time)))
      printBoard(board)
      println()
      if (hasPlayerWon(board, opponentPlayerSymbol(toPlay))) println(opponentPlayerSymbol(toPlay) + " has Won!!")
      else if (isGameDraw(board)) println("Game Draw!!")
      else if (toPlay == XPlayer) game(play(board, moveSelector(board, toPlay, round), toPlay), opponentPlayerSymbol(toPlay), currentTime, round + 1)
      else game(play(board, Console.readInt() - 1, toPlay), opponentPlayerSymbol(toPlay), currentTime, round + 1)
    }

    val initialBoard = {
      for (i <- 0 until N * N) yield blank
    }.toVector
    game(initialBoard, XPlayer, System.currentTimeMillis(), 0)
  }

}
