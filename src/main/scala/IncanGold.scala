package ren.kujoka.IncanGold
import ren.kujoka.common.Reader._
import scala.util.Random.shuffle
import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer

object IncanGold {
  private var players: ArrayBuffer[Player] = ArrayBuffer.empty
  private var way: ArrayBuffer[Int] = ArrayBuffer.empty
  private var explorationCards: ArrayBuffer[Int] = ArrayBuffer.empty
  // 0:zombie, 1:poison spider, 2:big snake, 3:flame, 4:rockfall
  private var traps: Array[Int] = Array.fill(5)(0)
  private var removedTraps: Array[Int] = Array.fill(5)(0)

  def main(args: Array[String]): Unit = {
    var yn = ""
    do {
      game()
      yn = yesOrNo("Play again?", "y", "n")
    } while (yn == "y")
  }

  def game(): Unit = {
    setPlayers()
    setExplorationCards(0)
    for (i <- 1 to 5) {
      addRelic()
      explorationCards = shuffle(explorationCards)
      if (i > 1) players(0).showScore()
      println("[ROUND" + i + "]")
      exploration()
    }
    println("[Result]")
    for (player <- players)
      player.showScore()
  }

  def exploration(): Unit = {
    var progress = 0
    players.foreach(n => n.isExploring = true)
    traps = Array.fill(5)(0)
    do {
      if (traps.max > 0) {
        var recent = Array.fill(players.length)("")
        for (player <- players if player.isExploring ) {
            if (player.playerNum == 0) {
            recent(0) = yesOrNo("Go or Back?", "g", "b")
          } else {
            recent(player.playerNum) = 
              player.think(waySum(1), relic(false))(traps)(removedTraps)
          }
        }
        var backRecent = 0
        for (r <- recent if r == "b") backRecent += 1
        for (i <- 0 until recent.length if recent(i) != "") {
          recent(i) match {
            case "g" => players(i).go()
            case "b" =>
              if (backRecent == 1)
                players(i).back(waySum(backRecent), relic(true))          
              else
                players(i).back(waySum(backRecent), 0)
            case _ => None
          }
        }
        if (backRecent > 0) wayRefresh(backRecent)
      }
      if (progress == 0) println("Go up to trap exists")
      if (inTheRuins > 0) {
        way += explorationCards.remove(0)
        showCards(way(progress))
        showWay()
        if (isGem(way(progress))) {
          for (player <- players if player.isExploring)
            player.temp += (way(progress) / inTheRuins)
          way(progress) %= inTheRuins
        }
        progress += 1
        Thread.sleep(1000)
      }
    } while (traps.max < 2 && inTheRuins > 0)
    val relicInTheRuin = relic(true)
    if (traps.max >= 2) {
      for (player <- players if player.isExploring)
        player.death()
      removedTraps(way.remove(way.length - 1) - TRAPS.min) += 1
    }
    setExplorationCards(relicInTheRuin)
    way.clear
    traps = Array.fill(5)(0)
  }

  def inTheRuins(): Int = {
    var n = players.length
    for (player <- players)
      if (!player.isExploring) n -= 1
    n
  }

  def showWay(): Unit = {
    for (i <- 0 until way.length) {
      if (i % 5 == 0 && i != 0) println("->")
      print("|" + "%1$3d".format(way(i)) + "|")
    }
    println
  }

  def showCards(card: Int): Unit = {
    val trap = card match {
      case ZOMBIE => "zombie"
      case POISON_SPIDER => "poison spider"
      case BIG_SNAKE => "big snake"
      case FLAME => "flame"
      case ROCKFAIL => "rockfall"
      case _ => ""
    }
    if (isTrap(card)) {
      traps(card - TRAPS.min) += 1
      println("!!!We were ataccked by a " + trap + "!!!")
    } else if (isRelic(card)) {
      println("***We found the relic***")
    } else {
      println("We have found " + card + " gems")
    }
  }

  def waySum(backRecent: Int): Int = {
    var sum = 0
    for (i <- 0 until way.length if isGem(way(i))) {
      sum += (way(i) / backRecent)
    }
    sum
  }

  def wayRefresh(backRecent: Int): Unit = {
    for (i <- 0 until way.length if isGem(way(i)))
      way(i) %= backRecent
  }

  def relic(back: Boolean): Int = {
    var sum = 0
    for (i <- 0 until way.length if isRelic(way(i))) {
      if (back) way(i) = 0
      sum += 1
    }
    sum
  }

  def setExplorationCards(relicInTheRuin: Int): Unit = {
    explorationCards = ArrayBuffer.empty[Int]
    // add gems
    explorationCards.appendAll(GEMS)

    // add traps
    for (i <- TRAPS)
      for (j <- 0 until 3 - removedTraps(i - TRAPS.min))
        explorationCards += i
    // add relics
    for (i <- 0 until relicInTheRuin)
      addRelic()
    explorationCards = shuffle(explorationCards)
  }

  def addRelic(): Unit = {
    explorationCards += RELIC
  }

  def setPlayers(): Unit = {
    players = ArrayBuffer.empty[Player]
    val numberOfPersons = readIntLoop("Please enter the number of persons(3~8) > ",
      "Please enter the correct value", 3, 8)
    players += new Player(0)
    println("Please enter the nature of the opponent")
    for (i <- 1 until numberOfPersons) {
      val nature = 
          readIntLoop("0:Chicken, 1:Normal, 2:Boldness > ",
          "Please enter the correct value", 0, 2)
      nature match {
        case 0 => players += new Chicken(i)
        case 1 => players += new Player(i)
        case 2 => players += new Boldness(i)
      }
    }
  }

  private val GEMS: Seq[Int] = 1 to 15
  private val RELIC: Int = 99
  private val ZOMBIE: Int = 100
  private val POISON_SPIDER: Int = 101
  private val BIG_SNAKE: Int = 102
  private val FLAME: Int = 103
  private val ROCKFAIL: Int = 104
  private val TRAPS = Seq(ZOMBIE, POISON_SPIDER, BIG_SNAKE, FLAME, ROCKFAIL)

  private def isGem(cardNum: Int): Boolean = GEMS.contains(cardNum) || cardNum == 0
  private def isRelic(cardNum: Int): Boolean = cardNum == RELIC
  private def isTrap(cardNum: Int): Boolean = TRAPS.contains(cardNum)
}
