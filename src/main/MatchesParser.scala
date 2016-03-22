package main.parser

import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import main.loader.{JSONpediaLoader, DBpediaLoader}
import main.WikiTemplateExtractor.ontology
import main.logger.Logger
import org.json4s.JsonAST.JValue

import org.json4s._
import org.json4s.native.JsonMethods._

object MatchesParser extends Logger{
  def GetFootballMatchesCount():Int ={
    implicit val formats = DefaultFormats
    val select = """select count(?s)
                    where{
                      ?s a <http://dbpedia.org/ontology/FootballMatch> .
                    }"""
    (parse(DBpediaLoader.GetDBpediaSparqlSelect(select).mkString) \\ "value").extract[String].toInt
  }

  def ParseDataForMatchesWithIndexes(fromIndex:Int, limit:Int, max:Int) = {
    var boundedLimit = limit
    if (fromIndex + limit > max){
      boundedLimit = max - fromIndex
    }
    val select =s"""select ?s
                    where{
                      ?s a <http://dbpedia.org/ontology/FootballMatch> .
                    }
                    OFFSET $fromIndex
                    LIMIT  $boundedLimit"""
    val JSON = DBpediaLoader.GetDBpediaSparqlSelect(select).mkString
    val matchesURI = parse(JSON) \\ "value"
    matchesURI.children.foreach(ParseDataForMatches(_))
  }

  def ParseDataForMatches(matchResource:JValue):Int = {
    println("Match " + matchResource + " are parsing.")
    implicit val formats = DefaultFormats
    val matchName = GetMatchNameFromResourseUrl(matchResource.extract[String])
    val matchSummary = JSONpediaLoader.GetDataForMatch(matchName, "name:football box,@type:template")

    if (matchSummary == null) {
      return -1
    }

    val jsonMatchesSummary = parse(matchSummary.mkString) \\ "result"

    jsonMatchesSummary.children.foreach(matchSummary => {
      if (matchSummary.children(1).extract[String] == "football box") {
        var currentMatchName = matchName;
        if (jsonMatchesSummary.children.length > 1){
          currentMatchName += "_" + jsonMatchesSummary.children.indexOf(matchSummary)
        }

        (matchSummary.children(2).extract[Map[String, JArray]] ).foreach(record => {
          try {

            record match {
              case ("date", value) =>
                // TODO: Add date parsing.
                ontology.AddLiteral(currentMatchName, "date", value.children(0).extract[String], XSDDatatype.XSDdate)
              case("time", value) =>
                var time = ""
                value.children.foreach(timePart => {
                  if (timePart.isInstanceOf[JString]) {
                    time += " " + timePart.extract[String]
                  }
                  else{
                    time += " " + timePart.children(1).extract[String]
                  }
                })
                ontology.AddLiteral(currentMatchName, "time", time, XSDDatatype.XSDstring)
              case("score", value) => ontology.AddLiteral(currentMatchName, "score", value(0).extract[String].replace("&ndash;", "-"), XSDDatatype.XSDstring)
              case("penaltyscore", value) => ontology.AddLiteral(currentMatchName, "penaltyscore", value(0).extract[String].replace("&ndash;", "-"), XSDDatatype.XSDstring)
              case("attendance", value) =>
                val attendance = attendanceToString(value)
                if (attendance != "") {
                  ontology.AddLiteral(currentMatchName, "attendance", attendance, XSDDatatype.XSDstring)
                }
              case("team1", value) =>
                val team = teamToString(value)
                if (team != "") {
                  ontology.AddProperty(currentMatchName, "teamHome", team)
                }
              case("team2", value) =>
                val team = teamToString(value)
                if (team != "") {
                  ontology.AddProperty(currentMatchName, "teamAway", team)
                }
              case("referee", value) => AddRefereeToOntology(value, currentMatchName)
              case("stadium", value) => AddStadiumToOntology(value, currentMatchName)
              case("report", value) => AddReportToOntology(value, currentMatchName)
              case("goals1", value) => AddTeamHomeGoalsToOntology(value, currentMatchName)
              case("goals2", value) => AddTeamAwayGoalsToOntology(value, currentMatchName)
              case("aet", value) => ontology.AddLiteral(currentMatchName, "aet", value(0).extract[String], XSDDatatype.XSDstring)
              case (key, _) => println(s"Error. Key $key not found")
            }
            // ...
          } catch {
            case ioe: NullPointerException => println(s"Null in $record")
            case e: Exception => println(s"Exception in $record")
          }
        })
      }
    })
    jsonMatchesSummary.children.length
  }

  def GetMatchNameFromResourseUrl(resourceUrl:String):String = {
    resourceUrl.replace("http://dbpedia.org/resource/", "")
  }

  def attendanceToString(value: JArray):String = {
    implicit val formats = DefaultFormats
    var attendance = ""
    if (value != null){
      attendance += value(0).asInstanceOf[JString].extract[String]
    }
    attendance
  }

  def teamToString(value: JArray):String = {
    implicit val formats = DefaultFormats
    var team = ""
    if (value(0).children.length > 0 && value(0).children(0).extract[String] == "reference") {
      team = value(0).children(1).extract[String]
    }
    else if (value.arr.length > 1 && value(1).children.length > 0 && value(1).children(0).extract[String] == "reference") {
      team = value(1).children(1).extract[String]
    }
    else if (value(0).children.length > 0 && value(0).children(0).extract[String] == "template"){
      team = value(0).children(2).children(0).children(0).extract[String]
    }
    team
  }

  def AddRefereeToOntology(value: JArray, matchName: String) = {
    implicit val formats = DefaultFormats
    var referee = ""
    if (value(0).children.length > 0 && value(0).children(0).extract[String] == "reference") {
      referee = value(0).children(1).extract[String]
      ontology.AddProperty(matchName, "referee", referee)

    }
    else{
      value.children.foreach(refPart => {
        if (refPart.isInstanceOf[JString]) {
          referee += " " + refPart.extract[String]
        }
        else{
          referee += " " + refPart.children(1).extract[String]
        }
      })
      ontology.AddLiteral(matchName, "referee", referee, XSDDatatype.XSDstring)
    }
  }

  def AddStadiumToOntology(value: JArray, matchName: String) = {
    implicit val formats = DefaultFormats
    if (value(0).children.length > 0 && value(0).children(0).extract[String] == "reference") {
      val stadium = value(0).children(1).extract[String]
      ontology.AddProperty(matchName, "stadium", stadium)
    }
  }

  def AddReportToOntology(value: JArray, matchName: String) = {
    implicit val formats = DefaultFormats
    if (value != null && value(0).children.length > 0 && value(0).children(0).extract[String] == "link") {
      val report = value(0).children(1).extract[String]
      ontology.AddLiteral(matchName, "report", report, XSDDatatype.XSDstring)
    }
  }

  def AddTeamHomeGoalsToOntology(value: JArray, matchName: String) = {
    implicit val formats = DefaultFormats
    if (value !=null) {
      value.children.foreach(goal => {
        if (goal.children(0).extract[String] == "reference") {
          val player = goal.children(1).extract[String]
          ontology.AddProperty(matchName, "teamHomeGoals",  player)
        }
      })
    }
  }

  def AddTeamAwayGoalsToOntology(value: JArray, matchName: String) = {
    implicit val formats = DefaultFormats
    if (value !=null) {
      value.children.foreach(goal => {
        if (goal.children(0).extract[String] == "reference") {
          val player = goal.children(1).extract[String]
          ontology.AddProperty(matchName, "teamAwayGoals",  player)
        }
      })
    }
  }
}
