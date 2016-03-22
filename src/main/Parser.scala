package main.parser

import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import main.loader.{JSONpediaLoader, DBpediaLoader}
import main.WikiTemplateExtractor.ontology
import main.logger.Logger
import org.json4s.JsonAST.JValue

import org.json4s._
import org.json4s.native.JsonMethods._

object Parser extends Logger{

  var CareerStationCountForCurrentPlayer = 0

  val AnyYearRegex = """^\s*\d{4}(?:\s*-\s*\d{4})?\s*$""".r
  val StartAndEndYearRegex = """^(\d{4})-(\d{4})$""".r
  val StartWithoutEndYearRegex = """^(\d{4})-\s*$""".r
  val OnlyStartYearRegex = """^(\d{4})$""".r

  def GetPlayersCountFromJSON(JSON:String):Int = {
    implicit val formats = DefaultFormats
    (parse(JSON) \\ "value").extract[String].toInt
  }

  def GetPlayerNameFromResourseUrl(resourceUrl:String):String = {
    resourceUrl.replace(ontology.ResoursePrefix, "")
  }

  def ParsePlayersCareerStationWithFilter(playerResource:JValue, filter:String): Unit ={
    implicit val formats = DefaultFormats
    val playerName = GetPlayerNameFromResourseUrl(playerResource.extract[String])
    val playerCareer = JSONpediaLoader.GetDataForPlayer(playerName, filter).mkString

    val jsonPlayerCareer = parse(playerCareer) \\ "result"

    if (jsonPlayerCareer.children.nonEmpty) {
      jsonPlayerCareer(0).children.foreach(x => getTypeOfJsonElement(x, playerName))
    }
  }

  def ParseDataForPlayer(playerResource:JValue) = {
    println("Player " + playerResource + " are parsing.")
    CareerStationCountForCurrentPlayer = 0
    ParsePlayersCareerStationWithFilter(playerResource, "SquadreGiovanili>content")
    ParsePlayersCareerStationWithFilter(playerResource, "Squadre>content")
    ParsePlayersCareerStationWithFilter(playerResource, "SquadreNazionali>content")
  }

  def ParseDataForPlayersWithIndexes(fromIndex:Int, limit:Int, max:Int, templateURI:String) = {
    var boundedLimit = limit
    if (fromIndex + limit > max){
      boundedLimit = max - fromIndex
    }
    val select =s"""select ?s as ?calciatori
                    where{
                      ?s a <http://dbpedia.org/ontology/SoccerPlayer>.
                      ?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI
                    }
                    OFFSET $fromIndex
                    LIMIT  $boundedLimit"""
    val JSON = DBpediaLoader.GetItDBpediaSparqlSelect(select).mkString
    val playersURI = parse(JSON) \\ "value"
    playersURI.children.foreach(ParseDataForPlayer(_))
  }

  def GetPlayersCountForTemplate(templateURI:String):String = {
    val select =s"""select count(?s) as ?numero_calciatori
                    where{
                    ?s a <http://dbpedia.org/ontology/SoccerPlayer>.
                    ?s <http://it.dbpedia.org/property/wikiPageUsesTemplate> $templateURI}"""
    val JSON = DBpediaLoader.GetItDBpediaSparqlSelect(select)
    JSON.mkString
  }

  def RemoveWhiteSpacesInString(string:String) = {
    string.replaceAll("""\s+$""", "")
  }

  def AddPlayerStationToGraph(player:String) = {
    val playerStation = player + "__" + CareerStationCountForCurrentPlayer
    ontology.AddProperty(player, "careerStation", playerStation)
    CareerStationCountForCurrentPlayer += 1
    playerStation
  }

  def AddPlayerStationInfoToGraph(currentPlayerStation:String, yearString:String) = {
    RemoveWhiteSpacesInString(yearString) match {
      case StartAndEndYearRegex(startYear, endYear) => {
        ontology.AddLiteral(currentPlayerStation, "startYear", startYear, XSDDatatype.XSDgYear)
        ontology.AddLiteral(currentPlayerStation, "endYear", endYear, XSDDatatype.XSDgYear)
      }
      case StartWithoutEndYearRegex(startYear) => {
        ontology.AddLiteral(currentPlayerStation, "startYear", startYear, XSDDatatype.XSDgYear)
      }
      case OnlyStartYearRegex(year) => {
        ontology.AddLiteral(currentPlayerStation, "startYear", year, XSDDatatype.XSDgYear)
        ontology.AddLiteral(currentPlayerStation, "endYear", year, XSDDatatype.XSDgYear)
      }
      case _ => logger.warn(s"Can't parse $yearString for $currentPlayerStation")
    }
  }

  def GetTypeOfString(string:String, playerName:String):Int = {
    var resultType = 0
    string match {
      case AnyYearRegex() => {
        val playerStation = AddPlayerStationToGraph(playerName)
        AddPlayerStationInfoToGraph(playerStation, string)
      }
      case _ =>
    }
    resultType
  }

  def GetTypeOfList(list:List[(String, JValue)], playerName:String):Int = {
    list.foreach {
      case ("name", x) => GetTypeOfString(x.toString, playerName)
      case _ =>
    }
    0
  }

  def getTypeOfJsonElement(element: JValue, playerName:String) = {
    var elementType = 0
    element.children match {
      case List(JString(x)) => elementType = GetTypeOfString(x, playerName)
      case List(JObject(x)) => elementType = GetTypeOfList(x, playerName)
      case _ =>
    }
    elementType == 1
  }

  def ParseDataForMatchesWithIndexes(fromIndex:Int, limit:Int, max:Int) = {
    var boundedLimit = limit
    if (fromIndex + limit > max){
      boundedLimit = max - fromIndex
    }
    val select =s"""select ?s as ?calciatori
                    where{
                      ?s a <http://dbpedia.org/ontology/FootballMatch> .
                    }
                    OFFSET $fromIndex
                    LIMIT  $boundedLimit"""
    val JSON = DBpediaLoader.GetDBpediaSparqlSelect(select).mkString
    val matchesURI = parse(JSON) \\ "value"
    matchesURI.children.foreach(ParseDataForMatches(_))
  }

  def ParseDataForMatches(matchResource:JValue) = {
    println("Match " + matchResource + " are parsing.")
    implicit val formats = DefaultFormats
    val matchName = GetMatchNameFromResourseUrl(matchResource.extract[String])
    val matchSummary = JSONpediaLoader.GetDataForMatch(matchName, "name:football box,@type:template").mkString

    val jsonMatchesSummary = parse(matchSummary) \\ "result"

    jsonMatchesSummary.children.foreach(f = matchSummary => {
      if (matchSummary.children(1).extract[String] == "football box") {
        println(matchSummary.children(2))
        (matchSummary.children(2).extract[Map[String, Any]] ).foreach(record => {
          record match {
            case ("date", _) => println("date parsing")
            case (key, _) => println(s"Error. Key $key not found")
          }
        })
        println(matchSummary.children(2).extract[Map[String, Any]])
      }
    })
  }

  def GetMatchNameFromResourseUrl(resourceUrl:String):String = {
    resourceUrl.replace("http://dbpedia.org/resource/", "")
  }
}
