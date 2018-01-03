import java.net.Socket

import scala.concurrent.Future

val socket = new Socket()

val packet = Future[Array[Byte]] = socket.readFr