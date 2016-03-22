package main.loader

import scala.io.{Source, BufferedSource}

object JSONpediaLoader {

  def GetDataForPlayer(playerName: String, filter:String):BufferedSource = {
    val encodedPlayerName = java.net.URLEncoder.encode(playerName, "utf-8")
    val encodedFilter = java.net.URLEncoder.encode(filter, "utf-8")
    Source.fromURL(s"http://jsonpedia.org/annotate/resource/json/it:$encodedPlayerName?filter=$encodedFilter&procs=-Extractors,Structure")
  }

  def GetDataForMatch(matchName: String, filter:String):BufferedSource = {
    val encodedPlayerName = java.net.URLEncoder.encode(matchName, "utf-8")
    val encodedFilter = java.net.URLEncoder.encode(filter, "utf-8")
    Source.fromURL(s"http://jsonpedia.org/annotate/resource/json/en:$encodedPlayerName?filter=$encodedFilter&procs=-Extractors,Structure")
  }
}
