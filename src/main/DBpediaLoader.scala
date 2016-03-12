package main.loader

import scala.io.{Source, BufferedSource}

object DBpediaLoader {

  def GetDBpediaSparqlSelect(query:String):BufferedSource = {
    val encodedQuery = java.net.URLEncoder.encode(query, "utf-8")
    return Source.fromURL(s"http://it.dbpedia.org/sparql?&query=$encodedQuery&format=json")
  }
}