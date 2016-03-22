package main

import main.logger.Logger
import main.ontology.Ontology
import main.parser._

object WikiTemplateExtractor extends Logger {
  val nameSpace = "http://dbpedia.org/ontology"
  val resoursePrefix = "http://dbpedia.org/resource/"
  val ontology = new Ontology(nameSpace, resoursePrefix)

  def extractSportivoTemplate() = {
    val template = "<http://it.dbpedia.org/resource/Template:Sportivo>"
    val playersForTemplateCount = PlayersParser.GetPlayersCountFromJSON(PlayersParser.GetPlayersCountForTemplate(template))

    val max =20
    val limit = 10

    (0 to max by limit) foreach (fromIndex => PlayersParser.ParseDataForPlayersWithIndexes(fromIndex, limit, max, template))

    ontology.WriteToFile("graph.ttl")
  }

  def extractFootballBoxTemplate() = {
    val max = MatchesParser.GetFootballMatchesCount()
    val limit = 10

    (0 to max by limit) foreach (fromIndex => MatchesParser.ParseDataForMatchesWithIndexes(fromIndex, limit, max))

    ontology.WriteToFile("graph.ttl")
  }

  def main(args: Array[String]): Unit = {
    extractFootballBoxTemplate();
  }
}