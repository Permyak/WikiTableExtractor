package main

import main.logger.Logger
import main.ontology.Ontology
import main.parser._

object WikiTemplateExtractor extends Logger {
  val nameSpace = "http://dbpedia.org/ontology"
  val resoursePrefix = "http://it.dbpedia.org/resource/"
  val ontology = new Ontology(nameSpace, resoursePrefix)

  def main(args: Array[String]): Unit = {
    val template = "<http://it.dbpedia.org/resource/Template:Sportivo>"
    val playersForTemplateCount = Parser.GetPlayersCountFromJSON(Parser.GetPlayersCountForTemplate(template))

    val max =20
    val limit = 10

    (0 to max by limit) foreach (fromIndex => Parser.ParseDataForPlayersWithIndexes(fromIndex, limit, max, template))

    ontology.WriteToFile("graph.ttl")
  }
}