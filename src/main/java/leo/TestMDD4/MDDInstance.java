package leo.TestMDD4;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicInteger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

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
import org.xml.sax.SAXException;

import leo.TestMDD4.CTCManager;
import leo.TestMDD4.MDDConv;
import leo.TestMDD4.NodeManager;

/*
 * Class to manage an instance of the MDD.
 * @TODO: name could do with a change to avoid confusion
 */
public class MDDInstance 
{
	private Node rootNode;
	private Node constraintsNode;
	private MDDConv conv;
	private int baseMDD;

	
	public MDDInstance(Document document) {
   		// Contains a single item node list for the struct node
   		NodeList nodeList = document.getElementsByTagName("struct");
   		rootNode = nodeList.item(0).getChildNodes().item(1);

   		constraintsNode = document.getElementsByTagName("constraints").item(0);
	}
	
	public int calculateMDD() {
   		Vector<Node> root = new Vector<Node>();
   		root.add(rootNode);

   		conv = new MDDConv();
		// Generates the variables for the MDD and initializes the manager
		conv.variablesGenerator(root); 
		// Displays the variables currently generated
		conv.getManager();
		// Generates the MDD with the inline constraints
		try {
			baseMDD = conv.getNode(root.get(0), 1);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
		baseMDD = conv.applyCTConstraints(baseMDD, rootNode, constraintsNode);
		
		return baseMDD;
	}
	
	public int getValidConfigs() {
		MDDManager manager = conv.returnManager();
		PathSearcher searcher = new PathSearcher(manager, 1);
		searcher.setNode(baseMDD);
		searcher.getPath();
		return searcher.countPaths();
	}
	
	public int getNodeCount() {
		MDDManager manager = conv.returnManager();
		return manager.getNodeCount();
	}

	public int getMddVariableCount() {
		MDDManager manager = conv.returnManager();
		MDDVariable[] vars = manager.getAllVariables();
		return vars.length;
	}
	
	public int getFMConstraints() {
		return countFMElements(rootNode.getChildNodes(),1)+1;
	}
	
	public int getFMFeatures() {
		return countFMElements(rootNode.getChildNodes(),2);
	}

	public int getFMCTConstraints() {
		return countFMElements(constraintsNode.getChildNodes(),3);
	}
	
	/*
	 * Funzione per il calcolo di informazioni riguardanti il feature model
	 * 
	 * @param nodeList: Lista di nodi contenente la struct o i constraints del feature model
	 * @param flag: Può assumere 3 valori, in base all'informazione ricercata:
	 *   1 per constraint inline
	 *   2 per feature
	 *   3 per constraint cross-tree
	 */
	private static int countFMElements(NodeList nodeList, int flag) { 
		int ret = 0;
		for (int count = 0; count < nodeList.getLength(); count++) {  
    		Node elemNode = nodeList.item(count);  
    		if (elemNode.getNodeType() == Node.ELEMENT_NODE) {  
	    		// get node name and value  	    		
	    		switch (elemNode.getNodeName()) {
	    		case("and"):
	    		case("alt"):
	    		case("or"):
	    			if (flag==1) 
	    				ret++;
	    			break;
	    		case("feature"):
	    			if (flag==2)
	    				ret++;
	    			break;
	    		case("rule"):
	    			if (flag==3)
	    				ret++;
	    			break;
	    		}
	    		
	    		
	    		if (elemNode.hasChildNodes()) {  
		    		//recursive call if the node has child nodes  
		    		ret += countFMElements(elemNode.getChildNodes(),flag);  
	    		}  
	    	}  
    	}
		return ret;
	}
	
	

	public void addConstraint(Node constrNode) {

		baseMDD = conv.applyCTConstraints(baseMDD, rootNode, constrNode);
	}
    
    
}
