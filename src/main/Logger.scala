package main.logger

import org.apache.log4j.{BasicConfigurator, Logger}

/**
  * LogHelper is a trait you can mix in to provide easy log4j logging
  * for your scala classes.
  **/
trait Logger {
  BasicConfigurator.configure()
  val loggerName = this.getClass.getName
  lazy val logger = Logger.getLogger(loggerName)
}