package ch13io

object Converter extends Converter with App {
    converter.run
}

class Converter {
    def getStrLn: IO[String] = IO { readLine }
    def putStrLn(s: String): IO[Unit] = IO { println(s) }

    def converter: IO[Unit] = for {
        _ <- putStrLn("Enter a temperature in degrees Fahrenheit.")
        celcius <- getStrLn.map(f => fahrenheitToCelcius(f.toDouble))
        _ <- putStrLn(celcius.toString)
    } yield ()

    def fahrenheitToCelcius(f: Double): Double = (f - 32) * 5.0 / 9.0
}