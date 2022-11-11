package leo.TestMDD4;

import java.util.Vector;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import pMedici.safeelements.ExtendedSemaphore;


public class MDDConv {

	MDDVariableFactory mvf;
	MDDManager manager;
	int varCount;

	// Transforms a signed byte value (-128 to 127) into an unsigned one (0 to 255)
	private static int sigToUnsig (int n) {
		if ( n<0 )
			n = (n*(-1))+127;
		return n;
	}

	// Transforms an unsigned byte value (0 to 255) into a signed one (-128 to 127)
	private static int unsigToSig (int n) {
		if ( n > 127 )
			n = ( n - 127 ) * (-1);
		return n;
	}

	public MDDConv () {
		this.manager = null;
		this.mvf = new MDDVariableFactory();
		varCount = 0;
	}
	
	public MDDManager returnManager() {
		return manager;
	}

    public void variablesGenerator (NodeList nodeList) {
		
    	for (int count = 0; count < nodeList.getLength(); count++) {  
    		Node elemNode = nodeList.item(count);  
    		if (elemNode.getNodeType() == Node.ELEMENT_NODE) {  
	    		// Prints out the name of the node (and, or, alt or feature)  
	    		System.out.println("\nNode Name =" + elemNode.getNodeName()+ " [OPEN]");  
	    		// Prints out the name of the element
    			System.out.println("Name =" +  elemNode.getAttributes().getNamedItem("name").getNodeValue()); 
	    		

	    		NodeList children = elemNode.getChildNodes();
    	    	Boolean leafNode=true;
    			int mandatoryChildren = 0;
    	    	// For each child, check whether any child is not a feature
		    	for(int i = 0; i < children.getLength(); i++) {
		    		// Only consider mandatory children for AND constraints
	    			if (children.item(i).getAttributes() != null)
						if (children.item(i).getAttributes().getNamedItem("mandatory") != null && elemNode.getNodeName() == "and")
							mandatoryChildren++;
					
			    	switch (children.item(i).getNodeName()) {
				   		// Add as variable
				   		case ("and"):
				   		case ("or"):
				   		case ("alt"):
				   				leafNode=false;
				   		break;
			    	}
		    	}

				// subtracting (mandatoryChildren-1) simplifies the structure of the MDD by grouping all mandatory features into
		    	// a single sub boolean value, thanks to the fact there's a single path leading to T from all mandatory features.
		    	
		    		
	    		int childNodesNum = ( (elemNode.getChildNodes().getLength() - 1 ) /2 );
	    		if (mandatoryChildren > 0) 
	    			childNodesNum = childNodesNum - (mandatoryChildren - 1);
	    		
    			int dups = (int) Math.ceil(childNodesNum/6.0);

    			System.out.println("ChildNodesNum#: "+childNodesNum);
    			System.out.println("Dups#: "+dups);
    			// Only check children if we know there's more constraints downstream
	    		if (elemNode.hasChildNodes() && !leafNode) {  
		    		// Recursive call if the node has child nodes  
		    		variablesGenerator(elemNode.getChildNodes());  
	    		}  

    			String varName = elemNode.getAttributes().getNamedItem("name").getNodeValue() + "var";
	    		switch (elemNode.getNodeName()) {
		    		// Ignore for now
		    		case ("feature"):
		    			System.out.println("FEAT Mngmt\n");
		    		break;
		    		// Add as variable
		    		case ("and"):
		    		case ("or"):
		    			System.out.println("B4 AND or OR Mngmt\n");
		    			for (int i=0; i<dups; i++) {
		    				
		    				if (dups-i == 1) {
		    					// cNN - (6*(dups-1)) corresponds to the number of children mod 6, where 6 stays 6 rather than 0.
		    					mvf.add(varName + varCount + i, (byte)Math.pow( 2,childNodesNum-(6*(dups-1)) ) );
		    					
		    				} else {
		    					mvf.add(varName + varCount + i, (byte)64 );
		    				}
		    	    		this.varCount++;
		    			}
		    		break;
		    		// Add as variable
		    		case ("alt"):
		    			System.out.println("B4 ALT Mngmt\n");
		    			// NEED TO CHECK MATHS FOR DUPS
		    			int altDups = (int) Math.ceil(childNodesNum/64.0);
		    			if (childNodesNum>1) {
			    			for (int i=0; i<altDups; i++) {
			    				if (altDups-i == 1) {
			    					mvf.add(varName + varCount + i, (byte) (childNodesNum-(63*(altDups-1))) );
			    				} else {
			    					mvf.add(varName + varCount + i, (byte)64 );
			    				}
			    				this.varCount++;
			    			}
		    			} else {
	    					mvf.add(varName + varCount + 0, (byte)2 );
		    				this.varCount++;
		    			}
		    				
		    		break;
	    		}
	    	}  
    	}
    }
    
    public void displayVars() {

		System.out.println("B4 factory\n");
		// Returns the MDD manager with the needed variable factory and with 2 leaves (T -> 1, F -> 0)
		this.manager = MDDManagerFactory.getManager(mvf, 2); 
		
		
		// Build the MDD structure
		MDDVariable[] vars = manager.getAllVariables();
		System.out.println("B4 vars\n");
		System.out.println("Var Count: "+ this.varCount);
		System.out.println("Var Count: "+ vars.length);
		// for (int i = 0; i< vars.length; i++) {
		for (int i = 0; i< varCount; i++) {
			System.out.println("Var Name: " + vars[i].key.toString()+" \t|| Var Bounds: "+ vars[i].nbval); 
		}
    }
    
    public void MDDBuilderStarter(NodeList nodeList) {
		MDDVariable[] vars = manager.getAllVariables();
		int nvars = vars.length;
		if (varCount != nvars) {
			System.out.println("Variables count not matching");
		}
    	varCount=0;
    	MDDBuilder(nodeList);
    	
    }
    
    public void MDDBuilder(NodeList nodeList, Vector<Integer>... childNodes) {
		
    	for (int count = 0; count < nodeList.getLength(); count++) {  
    		Node elemNode = nodeList.item(count);  
    		if (elemNode.getNodeType() == Node.ELEMENT_NODE) {  
    			/*
	    		// Prints out the name of the node (and, or, alt or feature)  
	    		System.out.println("\nNode Name =" + elemNode.getNodeName()+ " [OPEN]");  
	    		// Prints out the name of the element
    			System.out.println("Name =" +  elemNode.getAttributes().getNamedItem("name").getNodeValue()); 
    			*/
	    		
	    		// Begins here
	    		NodeList children = elemNode.getChildNodes();
    	    	Boolean leafNode=true;
    			int mandatoryChildren = 0;
    	    	// For each child, check whether any child is not a feature
		    	for(int i = 0; i < children.getLength(); i++) {
		    		// Only consider mandatory children for AND constraints
	    			if (children.item(i).getAttributes() != null)
						if (children.item(i).getAttributes().getNamedItem("mandatory") != null && elemNode.getNodeName() == "and")
							mandatoryChildren++;
	    			
			    	switch (children.item(i).getNodeName()) {
				   		// Add as variable
				   		case ("and"):
				   		case ("or"):
				   		case ("alt"):
				   				leafNode=false;
				   		break;
			    	}
		    	}
		    	
				// subtracting (mandatoryChildren-1) simplifies the structure of the MDD by grouping all mandatory features into
		    	// a single sub boolean value, thanks to the fact there's a single path leading to T from all mandatory features.
	    		int childNodesNum = ( (elemNode.getChildNodes().getLength() - 1 ) /2 );
	    		if (mandatoryChildren > 0) 
	    			childNodesNum = childNodesNum - (mandatoryChildren - 1);
    			int dups = (int) Math.ceil(childNodesNum/6.0);

    			// Only check children if we know there's more constraints downstream
	    		if (elemNode.hasChildNodes() && !leafNode) {  
		    		// Recursive call if the node has child nodes  
	    			MDDBuilder(elemNode.getChildNodes());  
	    		}  

	    		String varName = elemNode.getAttributes().getNamedItem("name").getNodeValue() + "var";
		    	switch (elemNode.getNodeName()) {
			    	// Depending on how many mandatory features
			    	case ("and"):
			    	case ("or"):
			    	case ("alt"):
			    		if (dups == 1) {
			    			MDDVariable var = manager.getVariableForKey(varName+varCount+0);
			    			System.out.println("var: "+varName+varCount+0);
			    			int[] paths = new int[var.nbval];
			    			// Cycles through all the possible values obtainable by the variable.
			    			paths[0]=0;
			    			for (int j=1; j<var.nbval;j++) 
			    				paths[j]=1;
			    			
			    			manager.getVariableForKey(varName+varCount+0).getNode(paths);
				   	    	this.varCount++;
			    		} else {
			    			// When the variable has been duplicated.
				   			for (int i=0; i<dups; i++) {
					    	    	this.varCount++;
			    			}
		    			}
		    		break;
	    		}
    			
	    	}  
    	}
    	
    }

	public int getStartingNode() throws InterruptedException {
		int newNode = 1;
		this.manager = MDDManagerFactory.getManager(mvf, 2); 

		if (manager == null)
			throw new RuntimeException(
					"You need to create an MDDManager with getMDD() before obtaining the starting node");

		// Build the MDD structure
		MDDVariable[] vars = manager.getAllVariables();
		for (int i = this.varCount - 1; i>= 0; i--) {
			ExtendedSemaphore.OPERATION_SEMAPHORE.acquire();
			newNode = vars[i].getNode(getChildrenList(vars[i].nbval, newNode));
			ExtendedSemaphore.OPERATION_SEMAPHORE.release();
		}
		
		return newNode;
	}

	private int[] getChildrenList(int dim, int nextNode) {
		int[] childrenList = new int[dim];
		
		for (int i=0; i<dim; i++) {
			childrenList[i] = nextNode;
		}
		
		return childrenList;
	}
	
}
