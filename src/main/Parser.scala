package main.parser

import main.loader.{JSONpediaLoader, DBpediaLoader}
import main.ontology.Ontology
import main.WikiTemplateExtractor.ontology

import org.json4s._
import org.json4s.native.JsonMethods._

object Parser {

  def GetPlayersCountFromJSON(JSON:String):Int = {
    implicit val formats = DefaultFormats
    (parse(JSON) \\ "value").extract[String].toInt
  }

  def GetPlayerNameFromResourseUrl(resourceUrl:String):String = {
    resourceUrl.replace("http://it.dbpedia.org/resource/", "")
  }

  def ParseDataForPlayer(playerResourse:JValue) = {
    implicit val formats = DefaultFormats
    val filter = "SquadreGiovanili>content"
    val playerCareer = JSONpediaLoader.GetDataForPlayer(GetPlayerNameFromResourseUrl(playerResourse.extract[String]), filter).mkString
    println(playerCareer)

    val jsonPlayerCareer = parse(playerCareer) \\ "result"
    println("ll "+jsonPlayerCareer)

    if (jsonPlayerCareer.children.size > 0) {
      jsonPlayerCareer(0).children.foreach(x => getTypeOfJsonElement(x))
    }
  }

  def ParseDataForPlayersWithIndexes(fromIndex:Int, limit:Int, templateURI:String) = {
    val select =s"""select ?s as ?calciatori
                    where{
                      ?s a <http://dbpedia.org/ontology/SoccerPlayer>.
                      ?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI
                    }
                    OFFSET $fromIndex
                    LIMIT  $limit"""
    val JSON = DBpediaLoader.GetDBpediaSparqlSelect(select).mkString
    val playersURI = parse(JSON) \\ "value"
    playersURI.children.foreach(ParseDataForPlayer(_))
  }

  def GetPlayersCountForTemplate(templateURI:String):String = {
    val select =s"""select count(?s) as ?numero_calciatori
                    where{
                    ?s a <http://dbpedia.org/ontology/SoccerPlayer>.
                    ?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI}"""
    val JSON = DBpediaLoader.GetDBpediaSparqlSelect(select)
    return JSON.mkString
  }

  def AddPlayerStationToGraph(player:String, careerStationIndex:Int) = {
    var prefix= "http://dbpedia.org/resource"
    val from = "<"+prefix+"/"+player+">"
    val conn = "http://dbpedia.org/ontology:careerStation"
    val to = "<"+prefix+"/"+player+"__"+ careerStationIndex + ">"
    println(from + " "+ conn + to)
    ontology.AddProperty(player)
  }

  def GetTypeOfString(string:String):Int = {
    val testRegex = """^\s*\d{4}(?:\s*-\s*\d{4})?\s*$""".r
    var resultType = 0
    string match {
      case testRegex() => AddPlayerStationToGraph(player = "myPl", 1)
      case _ => println("_" + string)
    }
    return resultType
  }

  def GetTypeOfList(list:List[(String, JValue)]):Int = {
    list.foreach(element => {
      println(element)
      element match {
        case ("name", x) => GetTypeOfString(x.toString)
        case _ =>
      }
    })

    return 0
  }

  def getTypeOfJsonElement(element: JValue) = {
    println("find type for " + element)
    var elementType = 0
    element.children match {
      case List(JString(x)) => elementType = GetTypeOfString(x)
      case List(JObject(x)) => elementType = GetTypeOfList(x)
      case _ =>
    }
    elementType == 1
  }
}
