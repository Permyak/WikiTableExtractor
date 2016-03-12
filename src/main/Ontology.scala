package main.ontology

import java.io.FileOutputStream

import com.hp.hpl.jena.rdf.model.{ResourceFactory, Property, ModelFactory}
import com.hp.hpl.jena.vocabulary.VCARD
import com.uncarved.helpers.LogHelper
import org.apache.jena.riot.Lang
import org.apache.jena.riot.system.StreamRDFWriter

class Ontology(namespace:String, resoursePrefix:String){

  // create an empty Model
  var model = ModelFactory.createDefaultModel();

  def AddProperty(playerName:String) = {
    val conn = "http://dbpedia.org/ontology:careerStation"
    var personURI    = "http://somewhere/JohnSmith";
    var fullName     = "John Smith";

    // create the resource
    var johnSmith = model.getResource(resoursePrefix + playerName)
    var prop = model.getProperty(conn)
    // add the property
    johnSmith.addProperty(prop, fullName);
  }

  def WriteToFile(fileName:String) ={
    StreamRDFWriter.write(System.out, model.getGraph, Lang.TURTLE)
  } ;
}
