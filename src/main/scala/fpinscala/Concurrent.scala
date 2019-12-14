package fpinscala

import java.util.concurrent.Executors

object Concurrent {

  def echoActor() = {
    val pool = Executors.newFixedThreadPool(4)
  }
}


