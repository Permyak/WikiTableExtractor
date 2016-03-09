package main

import org.json4s

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
    (parse(JSON) \\ "value").extract[String].toInt
  }

  def GetPlayersCountForTemplate(templateURI:String):String = {
    val select =s"""select count(?s) as ?numero_calciatori
                    where{
                    ?s a <http://dbpedia.org/ontology/SoccerPlayer>.
                    ?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI}"""
    val JSON = GetDBpediaSparqlSelect(select)
    return JSON.mkString
  }

  def ParseDataForPlayersWithIndexes(fromIndex:Int, limit:Int, templateURI:String) = {
    val select =s"""select ?s as ?calciatori
                    where{
                      ?s a <http://dbpedia.org/ontology/SoccerPlayer>.
                      ?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI
                    }
                    OFFSET $fromIndex
                    LIMIT  $limit"""
    val JSON = GetDBpediaSparqlSelect(select).mkString
    val playersURI = parse(JSON) \\ "value"
    playersURI.children.foreach(ParseDataForPlayer(_))
  }

  def ParseDataForPlayer(playerResourse:JValue) = {
    implicit val formats = DefaultFormats
    println(playerResourse.extract[String])
  }


  def main(args: Array[String]): Unit = {
    val template = "<http://it.dbpedia.org/resource/Template:Sportivo>"
    val playersForTemplateCount = GetPlayersCountFromJSON(GetPlayersCountForTemplate(template))

    val max = 10 - 1
    val limit = 5

    (0 to max by limit) foreach (ParseDataForPlayersWithIndexes(_, limit, template))

  }
}
