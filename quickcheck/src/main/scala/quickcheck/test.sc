import org.scalacheck._
import Arbitrary._
import Gen._

lazy val genMap: Gen[Map[Int,Int]] = oneOf(
  const(Map.empty[Int,Int]),
  for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)
)

genMap.sample