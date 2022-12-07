package leo.TestMDD4;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicInteger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.PathSearcher;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import ctwedge.fmtester.Converter;
import ctwedge.ctWedge.CitModel;
import ctwedge.generator.medici.MediciCITGenerator;
import ctwedge.generator.util.Utility;

import pMedici.util.ModelToMDDConverter;
import pMedici.util.Operations;
import pMedici.util.TestModel;




/**
 * Hello world!
 *
 */
public class App 
{
	
    public static void main( String[] args ) throws IOException, InterruptedException {
    	try {   
	    	String modelName = "eshop";
	   		File file = new File("featureModels/"+modelName+"Model.xml");  
	   		DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();  
	   		
	   		// Contains the parsed feature model xml
	   		Document document = documentBuilder.parse(file);  
	   		System.out.println("Root element: "+ document.getDocumentElement().getNodeName());  
	   		// Contains a single item node list for the struct node
	   		NodeList nodeList = document.getElementsByTagName("struct");
	  			   		
	   		Vector<Node> root = new Vector<Node>();
	   		root.add(nodeList.item(0).getChildNodes().item(1));
	   		
	   		MDDConv conv = new MDDConv();
    		if (nodeList.item(0).hasChildNodes()) {  
    			// Generates the variables for the MDD
    			conv.variablesGenerator(root); 
    		}  
    		// Displays the variables currently generated
    		conv.displayVars();
    		
    		int baseMDD = conv.getNode(nodeList.item(0).getChildNodes().item(1), 1);
    		
    		MDDManager manager = conv.returnManager();
    		// System.out.println(manager.dumpMDD(baseMDD).toString());  

       		System.out.print("\nBefore new PathSearcher\n");
    		PathSearcher searcher = new PathSearcher(manager, 1);

       		System.out.print("\nBefore searcher setNode. \nBaseMDD: "+baseMDD);
    		searcher.setNode(baseMDD);
       		System.out.print("\nBefore countPaths\n");
    		int nPaths = searcher.countPaths();

       		System.out.print("Paths #:\n " + nPaths + "\n\n");
       		
       		
	   		// ******************************************************************************************
	   		

	   		
    		/*
    		mddTest();
    		
	   		MDDManager mgr = conv.returnManager();
    		System.out.println("Features # : " + countFeature + "\nAnd: " + countAnd + "\nAlt: " + countAlt + "\nOr: " + countOr);
    		System.in.read();
	   		FMtoCTWtoMediciMDDGen(modelName);

   			countFeature.set(0);
   			// Contains a single item node list for the constraints node  
	   		NodeList nodeList2 = document.getElementsByTagName("constraints");
	   		// First item of the list is the constraints node
	   		Node constr = nodeList2.item(0);
	   		
	   		if (constr.hasChildNodes()) {  
    			printNodeList(constr.getChildNodes(),countFeature,countAnd,countAlt,countOr);  
    		}  
    		System.out.println("Rules # : " + countFeature);
    		
    		*/
   		}   
   		catch (Exception e) {  
   			System.out.println(e.getMessage());  
   		}  
    }
    
   // Prints out all the nodes and their children in the NodeList given, counting features, and, or and alts
	private static void printNodeList(NodeList nodeList) { 
		for (int count = 0; count < nodeList.getLength(); count++) {  
    		Node elemNode = nodeList.item(count);  
    		if (elemNode.getNodeType() == Node.ELEMENT_NODE) {  
	    		// get node name and value  
	    		System.out.println("\nNode Name =" + elemNode.getNodeName()+ " [OPEN]");  
	    		//System.out.println("Node Content =" + elemNode.getTextContent());  
	    		if (elemNode.hasAttributes()) {  
		    		NamedNodeMap nodeMap = elemNode.getAttributes();  
		    		for (int i = 0; i < nodeMap.getLength(); i++) {  
			    		Node node = nodeMap.item(i);  
			    		if ( node.getNodeName() == "name" ) {
			    			System.out.println(node.getNodeName() + "=" + node.getNodeValue()); 
			    		}
		    		}  
	    		}  
	    		if (elemNode.hasChildNodes()) {  
		    		//recursive call if the node has child nodes  
		    		System.out.println("Children (#="+elemNode.getChildNodes().getLength()+"):\n");  
		    		printNodeList(elemNode.getChildNodes());  
		    		System.out.println("Stop Children:\n");  
	    		}  
	    		System.out.println("Node Name =" + elemNode.getNodeName()+ " [CLOSE]");  
	    	}  
    	}

	}
    
    
    public static void mddTest() {
        MDDVariableFactory varios = new MDDVariableFactory();
        
        varios.add("E",(byte)4);
        varios.add("A",(byte)8);
        varios.add("S",(byte)4);
        
        System.out.print("Values: " + varios.getNbValue("x") + "\n");
        
        System.out.print(varios.size() + "\n");
        
        MDDManager tree = MDDManagerFactory.getManager(varios, 2);
        
        /*for (int x : tree.getChildren(0)) {
        	System.out.print(x);
        }*/
        //MDDVariable[] x = tree.getAllVariables();
        System.out.print("s node ID: " + tree.getVariableForKey("S").getNode(new int[]{0,0,0,1}) + "\n");
        System.out.print("a node ID: " + tree.getVariableForKey("A").getNode(new int[]{0,2,1,2,1,2,1,2}) + "\n");
        System.out.print("a node ID: " + tree.getVariableForKey("A").getNode(new int[]{0,2,0,0,1,2,0,0}) + "\n");
        System.out.print("e node ID: " + tree.getVariableForKey("E").getNode(new int[]{0,22,12,0}) + "\n");
        
        //System.out.print("z node ID: " + tree.getVariableForKey("z").getNode(new int[]{1,1,1,1}) + "\n");
        //System.out.print("y index: " + tree.getVariableIndex(tree.getVariableForKey("y"))+ "\n");
        System.out.print("Leaves: " + tree.getLeafCount() + "\n" );
        for (MDDVariable x : tree.getAllVariables()) {
        	System.out.print(x.toString());
        }
        System.out.print(tree.getNodeCount() + "\n\n");
        System.out.print(tree.reach(22,new byte[]{0,2}) + "\n\n");
        

   		System.out.print("\nBefore new PathSearcher\n");
		PathSearcher searcher = new PathSearcher(tree, 1);

   		System.out.print("\nBefore searcher setNode\n");
		searcher.setNode(32);
   		System.out.print("\nBefore countPaths\n");
		int nPaths = searcher.countPaths();

   		System.out.print("Paths #:\n " + nPaths + "\n\n");
        
        //System.out.print(tree.dumpMDD(32));
    }
    
    
    public static void FMtoCTWtoMediciMDDGen (String modelName) throws IOException, InterruptedException{
   		//File file = new File("./featureModels/gplModel.xml");  
    	//String modelName = "waterloo";
   		String xmlPath = "featureModels/"+modelName+"Model.xml";
   		String ctwPath = "featureModels/"+modelName+"CTWedge.ctw";
   		String mediciPath = "featureModels/"+modelName+"CITMod.txt";

   		ClassLoader loader = ClassLoader.getSystemClassLoader();
   	    loader.setDefaultAssertionStatus(true);
   	    
   	   
   		System.out.print(xmlPath);
   		// Converts FM to CTW, prints the result in ctwPath
   		try {
   			Converter.fromFMtoCTWedge_ENUM(xmlPath, ctwPath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
   		
   		// Converts CTW to medici model, prints the result in mediciPath
		CitModel model = null;
		model = Utility.loadModelFromPath(ctwPath);
		MediciCITGenerator gen = new MediciCITGenerator();
		MediciCITGenerator.OUTPUT_ON_STD_OUT_DURING_TRANSLATION = false;
		String mediciModel = gen.translateModel(model, false);
		File modelFile = new File(mediciPath);
		FileWriter wf;
		wf = new FileWriter(modelFile);
		wf.write(mediciModel);
		wf.close();
   		
		// Loads the (just converted) medici model in the TestModel class
		TestModel m = Operations.readFile(mediciPath); // TestModel = model with constraints

		// Converts the model in an MDD, the class returns the MDD itself and the starting node
   		System.out.print("\nBefore ModelToMDDConverter\n");
   		ModelToMDDConverter mc = new ModelToMDDConverter(m);
   		System.out.print("\nBefore getMDD\n");
		MDDManager manager = mc.getMDD();
   		System.out.print("\nBefore getStartingNode\n");
		int baseMDD = mc.getStartingNode();
		
		// Applies constraint to the "floating" MDD
   		System.out.print("\nBefore updateMDDWithConstraints\n");
		baseMDD = Operations.updateMDDWithConstraints(manager, m, baseMDD);

   		System.out.print("\nBefore new PathSearcher\n");
		PathSearcher searcher = new PathSearcher(manager, 1);

   		System.out.print("\nBefore searcher setNode\n");
		searcher.setNode(baseMDD);
   		System.out.print("\nBefore countPaths\n");
		int nPaths = searcher.countPaths();

   		System.out.print("Paths #:\n " + nPaths + "\n\n");
		/*
   		System.out.print("BaseMDD Dump:\n\n " + manager.dumpMDD(baseMDD) + "\n\n\n");*/
    	
 
    }
}
