package leo.TestMDD4;

import java.util.Vector;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


public class MDDConv {

	MDDVariableFactory mvf;
	MDDManager manager;
	int varCount;

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
	    	
    			NodeManager ocl = new NodeManager(elemNode);
		    

    			System.out.println("ChildNodesNum#: "+ ocl.getTotalChildrenNumber());
    			System.out.println("Dups#: "+ ocl.getDups());
    			// Only check children if we know there's more constraints downstream
	    		if (elemNode.hasChildNodes() && !ocl.isMDDLeaf()) {  
		    		// Recursive call if the node has child nodes  
		    		variablesGenerator(elemNode.getChildNodes());  
	    		}  

	    		
    			String varName = elemNode.getAttributes().getNamedItem("name").getNodeValue() + "var";
	    		switch (elemNode.getNodeName()) {
		    		// Add as variable
		    		case ("and"):
		    		case ("or"):
		    			System.out.println("B4 AND or OR Mngmt\n");
		    		
		    			for (int i=0; i<ocl.getDups(); i++) {
		    				
		    				if (ocl.getDups()-i == 1) {
		    					mvf.add(varName  + i, ocl.getBounds());
		    					
		    				} else {
		    					mvf.add(varName + i, (byte)64 );
		    				}
		    	    		this.varCount++;
		    			}
		    		break;
		    		// Add as variable
		    		case ("alt"):
		    			System.out.println("B4 ALT Mngmt\n");
		    		
		    			if (ocl.getTotalChildrenNumber()>1) {
			    			for (int i=0; i<ocl.getDups(); i++) {
			    				if (ocl.getDups()-i == 1) {
			    					mvf.add(varName + i, ocl.getBounds() );
			    				} else {
			    					mvf.add(varName + i, (byte)64 );
			    				}
			    				this.varCount++;
			    			}
		    			} else {
	    					mvf.add(varName + 0, (byte)2 );
		    				this.varCount++;
		    			}
		    				
		    		break;
	    		}
	    	}  
    	}
    }
    
    public void displayVars() {

		System.out.println("B4 factory\n");
		
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
			    			MDDVariable var = manager.getVariableForKey(varName+0);
			    			System.out.println("var: "+varName+0);
			    			int[] paths = new int[var.nbval];
			    			// Cycles through all the possible values obtainable by the variable.
			    			paths[0]=0;
			    			for (int j=1; j<var.nbval;j++) 
			    				paths[j]=1;
			    			
			    			manager.getVariableForKey(varName+0).getNode(paths);
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
    
    /* Old getStartingNode
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
	*/
    
    
    
	public int getStartingNode(Node currentNode, int destination) throws InterruptedException {
		
		this.manager = MDDManagerFactory.getManager(mvf, 2); 
		if (manager == null)
			throw new RuntimeException(
					"You need to create an MDDManager with getMDD() before obtaining the starting node");

		String varName = currentNode.getAttributes().getNamedItem("name").getNodeValue() + "var";
		NodeManager ocl = new NodeManager(currentNode);
		Vector<Node> OrderedChildrenList = ocl.getOrderedConstraintChildrenList();
		
		MDDVariable var = manager.getVariableForKey(varName+0);
		
		int totConstraintCh =  ocl.getMandatoryConstraintChildrenNumber() + ocl.getConstraintChildrenNumber();

		// TODO: Manage duplications, check that bounds calculations correspond on both sides (variable creation and node creation)
		
		int[] mddNodePath = new int[var.nbval];
		mddNodePath[0] = 0;
		int[] tempNodePath;
		
		switch (currentNode.getNodeName()) {
			case ("and"):
				int nextNode = destination;
				
				if (ocl.getMandatoryChildrenNumber() > 0)
					tempNodePath = new int[(int) Math.pow(2, (ocl.getConstraintChildrenNumber() + 1) )];
				else 
					tempNodePath = new int[(int) Math.pow(2, ocl.getConstraintChildrenNumber() )];
				
				
				if (ocl.getMandatoryConstraintChildrenNumber() > 0 || ocl.getMandatoryFeatureChildrenNumber() > 0) {
					
					// For every mandatory constraint child, build the "forced" part of the tree.
					// With MCCN == 0, nextNode is destination, thus making the following OR section equivalent.
					for (int i = 0; i < ocl.getMandatoryConstraintChildrenNumber();i++) 
						if (i == 0)
							nextNode = getStartingNode(OrderedChildrenList.get(i), destination);
						else
							nextNode = getStartingNode(OrderedChildrenList.get(i),tempNodePath[ nextNode ]);
					
					// In the specific situation where there are no mandatory constraints and at least one mandatory feature,
					// the path that would skip the optional constraints goes directly to 1 (no further constraints downstream)
					if (ocl.getMandatoryConstraintChildrenNumber() == 0 && ocl.getMandatoryFeatureChildrenNumber() > 0) 
						tempNodePath[0] = nextNode;
					else
						tempNodePath[0] = 1;

					// Set the first half of paths to zero, as they're the equivalent of saying one of the mandatory nodes is not selected.
					// TODO: Could be eliminated, granting one more optional variable to be used by a single non-duplicated AND node.
					for (int i=0; i<mddNodePath.length/2;i++)
						mddNodePath[i]=0;
						
					// For every optional constraint child, builds the recursions necessary to create the OR structure
					for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
						for (int j=0; j < Math.pow(2, i); j++) 
							if (j==0)
								tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),nextNode);
							else
								tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),tempNodePath[(int) ((Math.pow(2, i)+j) - Math.pow(2, i))]);
					
					// For every optional feature child, duplicates the paths leading to each node and builds the actual path
					if ((ocl.getFeatureChildrenNumber()) > 0)
						for (int i=0; i < tempNodePath.length; i++) 
							for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
									mddNodePath [ (int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j) + mddNodePath.length/2] = tempNodePath[i];
					
				} else {
					// Functionally equivalent to an OR
					tempNodePath[0] = 0;
					// For every constraint child, builds the recursions necessary to create the or structure
					for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
						for (int j=0; j < Math.pow(2, i); j++) 
							if (j==0)
								tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i),destination);
							else
								tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i),tempNodePath[(int) ((Math.pow(2, i)+j) - Math.pow(2, i))]);
					
					// For every feature child, duplicates the paths leading to each node
					if ((ocl.getFeatureChildrenNumber()) > 0)
						for (int i=0; i < tempNodePath.length; i++) 
							for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
								if (i==0 && j!=0) 
									mddNodePath [(int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j)] = 1;
								else 
									mddNodePath [ (int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j)] = tempNodePath[i];
					
				}
				
			break;
			case ("or"):
				tempNodePath = new int[(int) Math.pow(2,ocl.getConstraintChildrenNumber())];
				tempNodePath[0] = 0;
				// For every constraint child, builds the recursions necessary to create the or structure
				for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
					for (int j=0; j < Math.pow(2, i); j++) 
						if (j==0)
							tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i),destination);
						else
							tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i),tempNodePath[(int) ((Math.pow(2, i)+j) - Math.pow(2, i))]);
				
				// For every feature child, duplicates the paths leading to each node
				if ((ocl.getFeatureChildrenNumber()) > 0)
					for (int i=0; i < tempNodePath.length; i++) 
						for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
							if (i==0 && j!=0) 
								mddNodePath [(int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j)] = 1;
							else 
								mddNodePath [ (int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j)] = tempNodePath[i];
							
			break;
			case ("alt"):
				// For every constraint, one path that leads to that constraint
				for (int i=0; i < ocl.getConstraintChildrenNumber(); i++ )
					mddNodePath[i+1] = getStartingNode(OrderedChildrenList.get(i), destination);
			
				// For every feature, one path that leads to T
				for (int i=0; i < ocl.getFeatureChildrenNumber(); i++ )
					mddNodePath[i + ocl.getConstraintChildrenNumber() + 1] = getStartingNode(OrderedChildrenList.get(i), destination);
							
			break;
		}
    	

		System.out.println("ChildNodesNum#: "+ ocl.getTotalChildrenNumber());
		System.out.println("Dups#: "+ ocl.getDups());
		
		int newNode = 1;
		
		newNode = var.getNode(mddNodePath);
		
		return newNode;
	}

	/*
	private int[] getChildrenList(int dim, int nextNode) {
		int[] childrenList = new int[dim];
		
		for (int i=0; i<dim; i++) {
			childrenList[i] = nextNode;
		}
		
		return childrenList;
	}*/
}
