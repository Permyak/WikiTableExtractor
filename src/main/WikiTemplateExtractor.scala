package main

import main.logger.Logger
import main.ontology.Ontology
import main.parser._

import scala.io.{BufferedSource, Source}

object WikiTemplateExtractor extends Logger {
  val nameSpace = "http://dbpedia.org/ontology"
  val resoursePrefix = "http://it.dbpedia.org/resource/"
  val ontology = new Ontology(nameSpace, resoursePrefix)

  def main(args: Array[String]): Unit = {
    val template = "<http://it.dbpedia.org/resource/Template:Sportivo>"
    val playersForTemplateCount = Parser.GetPlayersCountFromJSON(Parser.GetPlayersCountForTemplate(template))

    val max = 4 - 1
    val limit = 2

    (3 to max by limit) foreach (Parser.ParseDataForPlayersWithIndexes(_, limit, template))

    ontology.WriteToFile("graph.ttl")
  }
}