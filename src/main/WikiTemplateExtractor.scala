package main

import scala.io.{BufferedSource, Source}
import org.json4s._
import org.json4s.native.JsonMethods._

object WikiTemplateExtractor {
  def GetDBpediaSparqlSelect(query:String):BufferedSource = {
    val encodedQuery= java.net.URLEncoder.encode(query, "utf-8")
    return Source.fromURL(s"http://it.dbpedia.org/sparql?&query=$encodedQuery&format=json")
  }

  def GetPlayersCountFromJSON(JSON:String):Int = {
    implicit val formats = DefaultFormats
    (parse(JSON) \ "results" \ "bindings" \ "numero_calciatori" \ "value").extract[String].toInt
  }

  def GetPlayersCountForTemplate(templateURI:String):String = {
    val select ="select count(?s) as ?numero_calciatori " +
                "where{" +
                "?s a <http://dbpedia.org/ontology/SoccerPlayer>. " +
                s"?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI}"
    val html = GetDBpediaSparqlSelect(select)
    return html.mkString
  }

  def main(args: Array[String]): Unit = {
    println(GetPlayersCountFromJSON(GetPlayersCountForTemplate("<http://it.dbpedia.org/resource/Template:Sportivo>")))
  }
}
