package main

import scala.io.{BufferedSource, Source}

object WikiTemplateExtractor {
  def getDBpediaSparqlSelect(query:String):BufferedSource = {
    val encodedQuery= java.net.URLEncoder.encode(query, "utf-8")
    return Source.fromURL(s"http://it.dbpedia.org/sparql?&query=$encodedQuery&format=json")
  }

  def getPlayersCountForTemplate(templateURI:String):String = {
    val select ="select count(?s) as ?numero_calciatori " +
                "where{" +
                "?s a <http://dbpedia.org/ontology/SoccerPlayer>. " +
                s"?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI}"
    val html = getDBpediaSparqlSelect(select)
    return html.mkString
  }

  def main(args: Array[String]): Unit = {
    println(getPlayersCountForTemplate("<http://it.dbpedia.org/resource/Template:Sportivo>"))
  }
}
