package ch13io

object IoApp {
    def main(args: Array[String]): Unit = {
        val cnt = contest(Player("Matt", 3), Player("Rylan", 3))

        cnt.run
    }
    
    def contest(p1: Player, p2: Player): IO[Unit] = 
        PrintLine(winnerMsg(winner(p1, p2)))
    
    def PrintLine(msg: String): IO[Unit] = IO {
        def run: Unit = println(msg)
    }

    def winnerMsg(p: Option[Player]): String = 
        p.map(pl => s"${pl.name} is winner!")
            .getOrElse("It's draw!")

    def winner(p1: Player, p2: Player): Option[Player] = 
        if (p1.score > p2.score) Some(p1)
        else if (p1.score < p2.score) Some(p2)
        else None

    case class Player(name: String, score: Int)
}