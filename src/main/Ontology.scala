package main.ontology

import java.io.{FileOutputStream, FileInputStream}

import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import com.hp.hpl.jena.rdf.model.ModelFactory
import org.apache.jena.riot.Lang
import org.apache.jena.riot.system.StreamRDFWriter

class Ontology(namespace:String, resoursePrefix:String) {

  def Namespace = namespace
  def ResoursePrefix = resoursePrefix
  var model = ModelFactory.createDefaultModel()

  def AddProperty(fromResourse:String, property:String, toResourse:String) = {
    val fromResName = resoursePrefix + fromResourse.replace(" ", "_")
    val toResName = resoursePrefix + toResourse.replace(" ", "_")
    val playerOntologyResourse = model.getResource(fromResName)
    val playerOntologyResourse2 = model.getResource(toResName)
    val prop = model.getProperty(s"$namespace/$property")

    playerOntologyResourse.addProperty(prop, playerOntologyResourse2)
  }

  def AddLiteral(fromResourse:String, property:String, literalString:String, literalDataType:XSDDatatype) = {
    val playerOntologyResourse = model.getResource(resoursePrefix + fromResourse)
    val prop = model.getProperty(s"$namespace/$property")
    var literal = model.createTypedLiteral(literalString, literalDataType)

    playerOntologyResourse.addLiteral(prop, literal)
  }

  def WriteToFile(fileName:String) ={
    StreamRDFWriter.write(new FileOutputStream(fileName), model.getGraph, Lang.TURTLE)
  }

  def LoadFile()={
    model.read(new FileInputStream("/Users/apechenezhskiy/Downloads/soccer_2015_11_22.ttl"),null, "TURTLE")
    StreamRDFWriter.write(new FileOutputStream("idealSample.ttl"), model.getGraph, Lang.TURTLE)
  }

}
