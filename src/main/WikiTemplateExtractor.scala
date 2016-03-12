package main

import com.hp.hpl.jena.rdf.model.ModelFactory

import scala.io.{BufferedSource, Source}
import org.json4s._
import org.json4s.native.JsonMethods._

object WikiTemplateExtractor {

  def GetDBpediaSparqlSelect(query:String):BufferedSource = {
    val encodedQuery = java.net.URLEncoder.encode(query, "utf-8")
    return Source.fromURL(s"http://it.dbpedia.org/sparql?&query=$encodedQuery&format=json")
  }

  def GetDataForPlayer(playerName: String, filter:String):BufferedSource = {
    val encodedPlayerName = java.net.URLEncoder.encode(playerName, "utf-8")
    val encodedFilter = java.net.URLEncoder.encode(filter, "utf-8")
    return Source.fromURL(s"http://jsonpedia.org/annotate/resource/json/it:$encodedPlayerName?filter=$encodedFilter&procs=-Extractors,Structure")
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

  def GetPlayerNameFromResourseUrl(resourceUrl:String):String = {
    resourceUrl.replace("http://it.dbpedia.org/resource/", "")
  }

  def AddPlayerStationToGraph(player:String, careerStationIndex:Int) = {
    var prefix= "http://dbpedia.org/resource"
    val from = "<"+prefix+"/"+player+">"
    val conn = "http://dbpedia.org/ontology:careerStation"
    val to = "<"+prefix+"/"+player+"__"+ careerStationIndex + ">"
    println(from + " "+ conn + to)
    val model = ModelFactory.createDefaultModel();
    val playerRes = model.getResource("http://it.dbpedia.org/resource/Aad_Bak")
    val prop = model.createProperty("myProp")
    playerRes.addProperty(prop, "subj")
    //var newpl = model.createResource("http://it.dbpedia.org/resource/Aad_Bak")
    model.write(System.out, )
    var sdf=""
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

  def ParseDataForPlayer(playerResourse:JValue) = {
    implicit val formats = DefaultFormats
    val filter = "SquadreGiovanili>content"
    val playerCareer = GetDataForPlayer(GetPlayerNameFromResourseUrl(playerResourse.extract[String]), filter).mkString
    println(playerCareer)

    val jsonPlayerCareer = parse(playerCareer) \\ "result"
    println("ll "+jsonPlayerCareer)

    jsonPlayerCareer(0).children.foreach(x => getTypeOfJsonElement(x))
  }


  def main(args: Array[String]): Unit = {
    val template = "<http://it.dbpedia.org/resource/Template:Sportivo>"
    //val playersForTemplateCount = GetPlayersCountFromJSON(GetPlayersCountForTemplate(template))

    val max = 4 - 1
    val limit = 2

    (3 to max by limit) foreach (ParseDataForPlayersWithIndexes(_, limit, template))

  }
}
