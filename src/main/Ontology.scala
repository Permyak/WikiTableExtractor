package main.ontology

import java.io.{FileOutputStream, BufferedReader, FileReader, FileInputStream}
import java.nio.file.{Files, Paths}

import com.hp.hpl.jena.rdf.model.{ResourceFactory, Property, ModelFactory}
import org.apache.jena.riot.lang.{PipedRDFIterator, PipedTriplesStream}
import org.apache.jena.riot.{RDFDataMgr, Lang}
import org.apache.jena.riot.system.StreamRDFWriter

class Ontology(namespace:String, resoursePrefix:String){

  def Namespace = namespace
  def ResoursePrefix = resoursePrefix
  // create an empty Model
  var model = ModelFactory.createDefaultModel();

  def AddProperty(fromResourse:String, property:String, toResourse:String) = {

    var fullName     = "John Smith";

    var playerOntologyResourse = model.getResource(resoursePrefix + fromResourse)
    var playerOntologyResourse2 = model.getResource(resoursePrefix + toResourse)

    var prop = model.getProperty(s"$namespace:$property")
    model.createStatement(playerOntologyResourse, prop, playerOntologyResourse2+"sad")
    playerOntologyResourse.addProperty(prop, fullName);
    playerOntologyResourse.addProperty(prop, playerOntologyResourse2)
  }

  def WriteToFile(fileName:String) ={
    StreamRDFWriter.write(System.out, model.getGraph, Lang.TURTLE)
  }

  def LoadFile()={
    model.read(new FileInputStream("/Users/apechenezhskiy/Downloads/soccer_2015_11_22.ttl"),null, "TURTLE")
    StreamRDFWriter.write(new FileOutputStream("graph.ttl"), model.getGraph, Lang.TURTLE)
  }

}
