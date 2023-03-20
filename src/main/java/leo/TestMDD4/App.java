package leo.TestMDD4;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
/**
 * Simple java application to make use 
 *
 */
public class App 
{
	
    public static void main( String[] args ) throws IOException, InterruptedException {
    	try {   
       		System.out.println("Insert name of the model from featureModels folder ('Model.xml' is added automatically): \n");
    		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
	    	String modelName =  reader.readLine();   
	   		File file = new File("featureModels/"+modelName+"Model.xml");  
	    	if(file.exists() && !file.isDirectory()) { 
		   		DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();  
		   		
		   		// Contains the parsed feature model xml
		   		Document document = documentBuilder.parse(file); 
		   		// Creates an instance for handling the parsing of the XML document and the class creating the MDD
		   		MDDInstance inst = new MDDInstance(document);
		   		// Creates variables, hierarchical structure and applies CTCs to create the complete MDD.
		   		inst.calculateMDD();

	       		System.out.println("Number of variables:\n " + inst.getMddVariableCount());
	       		System.out.println("Number of nodes:\n " + inst.getNodeCount());
	       		System.out.println("Number of Group Features:\n " + inst.getFMConstraints());
	       		System.out.println("Number of Leaf Features:\n " + inst.getFMFeatures());
	       		System.out.println("Number of CTCs:\n " + inst.getFMCTConstraints());
		   		// Calculate valid configurations and display it in output
	       		System.out.println("\nPaths # After CTCs:\n " + inst.getValidConfigs());
	    	} else {
	       		System.out.print("The file doesn't exist");
	    	}
   		}   
   		catch (Exception e) {   
   			System.out.println(e.getMessage());  
   		}  
    }
}
