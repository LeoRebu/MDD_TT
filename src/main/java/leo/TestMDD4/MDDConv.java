package leo.TestMDD4;

import java.util.ArrayList;
import java.util.Date;
import java.util.Stack;
import java.util.Vector;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import pMedici.safeelements.ExtendedSemaphore;


public class MDDConv {

	MDDVariableFactory mvf;
	MDDManager manager;

	public MDDConv () {
		this.manager = null;
		this.mvf = new MDDVariableFactory();
		// varCount = 0;
	}
	
	public MDDManager returnManager() {
		return manager;
	}

	/**
	 * Recursive procedure to generate all the variables for the MDD
	 * 
	 * @param nodes: Vector of the nodes to be examined. Starts with the root
	 */
    public void variablesGenerator (Vector<Node> nodes) {
		
    	for (int count = nodes.size()-1; count >= 0; count--) {  
   			NodeManager ocl = new NodeManager(nodes.get(count));
   			// Base of the variable's name
    		String varName = ocl.getName() + "Var";
    			
    		
    		// If a variable isn't duplicated, a 0 is added to its name.
    		// If duplicated, the duplications have 1...n added to their name depending on their position in the duplication's hierarchy.
    		switch (ocl.getType()) {
		    	// Add as variable
		    	case ("and"):
		    	case ("or"):
			    	for (int i=0; i<ocl.getDups(); i++) {
			    		
			    		if (i == 0) {
			    			mvf.add(varName + i, (byte) ocl.getBounds());
			    		}
		   				else
			    			// 64 for variables, 65th for a zero to prevent flattening of node
		   					mvf.add(varName + i, (byte)65 );
			    	}
		    	break;
		    	// Add as variable
		    	case ("alt"):
		    	
		    		if (ocl.getTotalChildrenNumber()>0) {
			   			for (int i=0; i<ocl.getDups(); i++) {
			   				if (i == 0)
			   					mvf.add(varName + i, ocl.getBounds() );
			   				else 
				    			// 63 for variables, 64th for a zero to prevent flattening of node
			   					mvf.add(varName + i, (byte)64 );
			   			}
		    		}
		    			
		    	break;
    		}

    		// Only check children if the current node has constraint children
	    	if (!ocl.isMDDLeaf()) {  
		    	// Recursive call if the node has child nodes  
		    	variablesGenerator(ocl.getOrderedConstraintChildrenList());  
	    	}  
	    	
    	}
    }
    
	/**
	 * Debug function to make sure the variables are generated correctly. Prints out the variables and their bounds
	 * 
	 */
    public void displayVars() {
		this.manager = MDDManagerFactory.getManager(mvf, 2); 
		
		// Fetches the variables
		MDDVariable[] vars = manager.getAllVariables();
		System.out.println("Variables Count: "+ vars.length);
		for (int i = 0; i< vars.length; i++) {
			System.out.println("Var " + vars[i].key.toString()+" -> B="+ vars[i].nbval); 
		}
    }
    
    private ArrayList<String> readConstraint (Node n, ArrayList<String> ret) {
    	NodeList constraintList = n.getChildNodes();
    	int count = 0;
    	
    	for (int i = 0; i < constraintList.getLength(); i++) {  
    		Node elemNode = constraintList.item(i);  
    		if (elemNode.getNodeType() == Node.ELEMENT_NODE) {  
	    		// get node name and value  
	    		//System.out.println("Node Content =" + elemNode.getTextContent()); 
	    		switch (elemNode.getNodeName()) {
	    		case ("var"): 
	    			ret.add(elemNode.getTextContent());
	    			break;
	    		case ("imp"): 
	    			readConstraint(elemNode,ret);
	    			ret.add("+");
	    			break;
	    		case ("eq"): 
                	System.out.println("eq node not yet implemented");
	    			readConstraint(elemNode,ret);
	    			break;
	    		case ("conj"): 
	    			readConstraint(elemNode,ret);
    				ret.add("*");
	    			break;
	    		case ("disj"): 
	    			readConstraint(elemNode,ret);
    				ret.add("+");
	    			break;
	    		case ("not"): 
	    			readConstraint(elemNode,ret);
    				ret.add("-");
	    			break;
	    		}
        		if (n.getNodeName() == "imp") {
        			if (count++ == 0) {
        				ret.add("-");
        			}
        		}
	    	}  
    	} 
    	
    	return ret;
    }
    
    
    public int applyCTConstraints(int baseMDD, Node root, Node constraints) {
    	  		
    	NodeList constraintList = constraints.getChildNodes();
    	ArrayList<ArrayList<String>> cList = new ArrayList<ArrayList<String>>();
    	CTCManager ctcm = new CTCManager(manager,baseMDD, root);
    	
    	for (int count = 0; count < constraintList.getLength(); count++) {  
    		Node elemNode = constraintList.item(count);  
    		if (elemNode.getNodeType() == Node.ELEMENT_NODE) {  
    			// New constraint
	    		if (elemNode.getNodeName() == "rule") {
	    	    	ArrayList<String> ret = new ArrayList<String>();
	    			cList.add(readConstraint(elemNode,ret));
	    			for (String s : ret)
	    				System.out.print(s + " ");
	    			System.out.println();
	    		}
	    	}  
    	} 

    	
    	// Fetch all the constraints
		for (ArrayList<String> a : cList) {
			// Fetch all the elements inside the constraint
			Stack<Integer> tPList = new Stack<Integer>();
			for (String s : a) {
				
				if (s.equals("+") || s.equals("-") || s.equals("*")) {
					int newNode;
					int n1 = -1;
					int n2 = -1;
					switch (s) {
					case "+":
						// OR Operation
						assert (tPList.size() >= 2);
						n1 = tPList.pop();
						n2 = tPList.pop();
						newNode = MDDBaseOperators.OR.combine(manager, n1, n2);
						tPList.push(newNode);
						break;
					case "*":
						// AND Operation
						assert (tPList.size() >= 2);
						n1 = tPList.pop();
						n2 = tPList.pop();
						newNode = MDDBaseOperators.AND.combine(manager, n1, n2);
						tPList.push(newNode);
						break;
					case "-":
						// NOT Operation
						assert (tPList.size() >= 1);
						n1 = tPList.pop();
						newNode = manager.not(n1);
						tPList.push(newNode);
						break;
					}
				} else {
					// Convert the value in a MDD and store it into a list
					int newNode = ctcm.getMDDFromVar(s);
					tPList.push(newNode);
				}
			}

			// At the end of the single constraint management, each constraint must
			// correspond to a single node
			if (tPList.size() != 1) {
				System.out.println(tPList.size() + " - ERROR IN CONSTRAINTS DEFINITION \n");
				return -1;
			}

			// Now the top of the stack must contain the complete constraint representation
			// and we can update the base node

			baseMDD = MDDBaseOperators.AND.combine(manager, baseMDD, tPList.pop());
		}
		return baseMDD;
    }
    
    
    
	/**
	 * Recursive function that creates the MDD with the inline constraints.
	 *  
	 * @param currentNode: node on which to create a corresponding MDD node
	 * @param destination: the node at which this node will point to when a path is valid
	 * @return integer value of the node created. 
	 */
	public int getNode(Node currentNode, int destination) throws InterruptedException {
		
		if (manager == null)
			throw new RuntimeException(
					"You need to create an MDDManager with getMDD() before obtaining the starting node");

		NodeManager ocl = new NodeManager(currentNode);
		String varName = ocl.getName()+"Var";
		// System.out.println("var: "+varName+0);
		Vector<Node> OrderedChildrenList = ocl.getOrderedConstraintChildrenList();
		
		MDDVariable var = manager.getVariableForKey(varName+0);
		MDDVariable[] duplicatedVar = new MDDVariable[ocl.getDups()-1];
		
		for (int i=0; i< (ocl.getDups()-1);i++) {
			duplicatedVar[i] = manager.getVariableForKey(varName+(i+1));
		}
			
		
		
		// Creates and initialises the matrix for duplications
		Vector<Vector<Integer>> mddNodePathMatrix = new Vector<Vector<Integer>>(duplicatedVar.length);
		for (int i=0;i<duplicatedVar.length;i++)
			mddNodePathMatrix.add(i,new Vector<Integer>());
		
		
		Vector<Integer> mddNodePath = new Vector<Integer>();
		Vector<Integer> tempNodePath = new Vector<Integer>();

		// System.out.println("mddNodePathMatrix length: "+ mddNodePathMatrix.size());
		// System.out.println("duplicatedVar length: "+ duplicatedVar.length);
		// System.out.println("Dups: "+ ocl.getDups());
		// System.out.println("No Dups " + ocl.getBoundsNoDups());
		
		int newNode = 1;

		if (ocl.isMDDLeaf()) {  
			// Leaf Children

    		if (ocl.getDups() == 1) {
    			// No duplications, same for all three types
    			mddNodePath.add(0);
	    			
    			// System.out.println("var: "+varName+0);
    			// System.out.println("Bounds no dups: "+ ocl.getBoundsNoDups());
    			// Cycles through all the possible values obtainable by the variable.
    			for (int j=1; j < ocl.getBoundsNoDups();j++) 
    				mddNodePath.add(destination);

    			if (ocl.getType() == "and") {
    				if (ocl.getMandatoryChildrenNumber() == ocl.getTotalChildrenNumber())
        				mddNodePath.add(0);
    				else
    					mddNodePath.add(destination);
    			}
					
    		} else {
    			// Leaf with duplications, distinguishes between ALT and OR/AND nodes.
    			if (ocl.getType() == "alt") {
	    			// First value is zero
	    			tempNodePath.add(0);

	    			// Cycles through all the possible values obtainable by the variable.
	    			for (int j=1; j < ocl.getBoundsNoDups();j++) {
	    				
	    				tempNodePath.add(destination);
	    				
	   					if (tempNodePath.size() == 64) {
	   						// Empty mddNodePath into finalPath
	   						
	   						int[] helper = new int[64];
	   						// Turns the Vector<Integer> that has been filled into an int[]
	   						for (int k=0; k<64; k++)
	   							helper[k] = tempNodePath.get(k);
	   						
	   						mddNodePath.add(duplicatedVar[0].getNode(helper));
	   						tempNodePath.clear();
	   						tempNodePath.add(0);
	   					}
	    			}
	    			
	    			// Leftovers in the temporary path are included in the last bottom level node, filled with zeroes otherwise
	    			if (tempNodePath.size() > 0) {
	    				// Fill the node with zeroes to avoid mismatch
		    			for (int j=tempNodePath.size(); j<65; j++)
	   						tempNodePath.add(0);
		    			int[] finalPath = new int[64];
   						for (int k=0; k<64; k++)
   							finalPath[k] = tempNodePath.get(k);
   						mddNodePath.add(duplicatedVar[0].getNode(finalPath));
	    			}
	    			
    			} else {
    				// AND or OR leaf nodes
					int[] dupsBasePath = new int[65];

		   			for (int i=0; i < ocl.getBoundsNoDups() / 64 ; i++) {
		    			// The cycle repeats for as many times the bottom level is duplicated

		   				// Creation of bottom level nodes paths
		   				for (int j = 0; j < 64; j++) {
		   					if (i==0 && j==0) {
		   						if (ocl.getMandatoryChildrenNumber() == 0)
		   							dupsBasePath[j]=0;
		   						else
		   							dupsBasePath[j]=destination;
		   					} else
		   						dupsBasePath[j]=destination;
		   				}
		   				// 65th path to zero (to avoid flattening)
   						dupsBasePath[64]=0;

   						// After reaching 65 paths, creates a bottom level node (for MDD ordering reasons)
	   					mddNodePathMatrix.get(0).add(duplicatedVar[duplicatedVar.length-1].getNode(dupsBasePath));
   						
		   				// Creation of higher level nodes paths (whenever a level reaches bounds, we pack it in the higher level
		   				for (int j = 1; j<ocl.getDups()-1; j++) {
		   					if (mddNodePathMatrix.get(j-1).size() == 64) {
		   						mddNodePathMatrix.get(j-1).add(0);
		   						int[] helper = new int[65];
		   						// Turns the Vector<Integer> that has been filled into an int[]
		   						for (int k=0; k<64; k++)
		   							helper[k] = mddNodePathMatrix.get(j-1).get(k);
		   						
		   						// The new node is added to the path of the level above (j+1)
		   						// and the node is created from the same level of variable:
		   						// Level 1 -> Second last duplicatedVar (length - 1)
		   						// Level ocl.getDups()-2 -> duplicatedVar[0] 
		   						mddNodePathMatrix.get(j).add(duplicatedVar[ (duplicatedVar.length-1) - j ].getNode(helper));
			   					mddNodePathMatrix.get(j-1).clear();
		   					}
		   				}
		   			}

		   			mddNodePath = mddNodePathMatrix.get(ocl.getDups()-2);
		   			mddNodePath.add(0);
    			}
    		}
			
		}  else {
			// Non Leaf Nodes
			
			// ALT node
			// TODO: implement duplications
			if (ocl.getType() == "alt") {
				mddNodePath.add(0);
				if (ocl.getTotalChildrenNumber() > 0) {
					// For every constraint, one path that leads to that constraint
					for (int i=0; i < ocl.getConstraintChildrenNumber(); i++ ) {
						mddNodePath.add( getNode(OrderedChildrenList.get(i), destination));
					}
				
					// For every feature, one path that leads to T
					for (int i=0; i < ocl.getFeatureChildrenNumber(); i++ ) {
						mddNodePath.add(destination);
					}
				}
				// System.out.println("Current Node is ALT, named: "+ ocl.getName());
			}
			
			// AND node with mandatory part
			if ( ocl.getType() == "and" && (ocl.getMandatoryConstraintChildrenNumber() > 0 || ocl.getMandatoryFeatureChildrenNumber() > 0)) {
				int nextNode = destination;
				
				// PART 1.1: Mandatory constraints in series
				// For every mandatory constraint child, build the "forced" part of the tree.
				// With MCCN == 0, nextNode is destination, thus making the following OR section correct.
				for (int i = 0; i < ocl.getMandatoryConstraintChildrenNumber(); i++) {
						nextNode = getNode(OrderedChildrenList.get(i), nextNode );
				}
				
				// PART 1.2: First path leads to the mandatory part (in case it doesn't exist, to destination)
				tempNodePath.add(nextNode);
				
				
				// PART 2: Mandatory not chosen paths lead to zero
				// Set the first half of paths to zero, as they're the equivalent of saying one of the mandatory nodes is not selected.
				// Removed, as AND constraints with lots of children would've meant million of paths leading to zero
				// for (int i=0; i < ocl.getBoundsNoDups()/2 ;i++)
					// mddNodePath.add(i, 0);
				
				// PART 3: Optional constraints go in an OR structure
				// For every optional constraint child, builds the recursions necessary to create the OR structure
				// The optional constraint children being after the mandatory ones, they are indexed using the MCCN
				for (int i = ocl.getConstraintChildrenNumber() - 1; i >= 0; i--) 
					for (int j=0; j < Math.pow(2, i); j++) 
						if (j==0)
							tempNodePath.add(getNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),nextNode));
						else
							tempNodePath.add(getNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),tempNodePath.get(j) ));
						
				if (ocl.getConstraintChildrenNumber() == 0 && ocl.getFeatureChildrenNumber() == 0)
					tempNodePath.add(0,0);
				
				// PART 4: Building final paths array, duplicating based on optional features 
				// For every optional feature child, duplicates the paths leading to each node and builds the actual path
				for (int i=0; i < tempNodePath.size(); i++) 
					for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) {
						mddNodePath.add( tempNodePath.get(i));
						
						if (mddNodePath.size() == 64 && ocl.getDups() > 1) {
							mddNodePath.add(0);
							int[] helper = new int[65];
	   						// Turns the Vector<Integer> that has been filled into an int[]
	   						for (int k=0; k<65; k++)
	   							helper[k] = mddNodePath.get(k);
							// After reaching 65 paths, creates a bottom level node (for MDD ordering reasons)
		   					mddNodePathMatrix.get(0).add(duplicatedVar[duplicatedVar.length-1].getNode(helper));
		   					mddNodePath.clear();
						}

		   				// Creation of higher level nodes paths (whenever a level reaches bounds, we pack it in the higher level
		   				for (int k = 1; k<ocl.getDups()-1; k++) {
		   					if (mddNodePathMatrix.get(k-1).size() == 64) {
		   						mddNodePathMatrix.get(k-1).add(0);
		   						int[] helper = new int[65];
		   						// Turns the Vector<Integer> that has been filled into an int[]
		   						for (int l=0; l<64; l++)
		   							helper[l] = mddNodePathMatrix.get(k-1).get(l);
		   						
		   						mddNodePathMatrix.get(k).add(duplicatedVar[ (duplicatedVar.length-1) - k ].getNode(helper));
			   					mddNodePathMatrix.get(k-1).clear();
		   					}
		   				}
					}
			
				if (ocl.getDups() > 1) {
   					mddNodePath.clear();
		   			mddNodePath = mddNodePathMatrix.get(ocl.getDups()-2);
		   			mddNodePath.add(0);
				}
			} 
			
			// OR node or equivalent AND node without mandatory children
			if ( ocl.getType() == "or" || ( ocl.getType() == "and" && ocl.getMandatoryConstraintChildrenNumber() == 0 && ocl.getMandatoryFeatureChildrenNumber() == 0)) {
				tempNodePath.add(0,0);
				// For every constraint child, builds the recursions necessary to create the or structure
				for (int i = ocl.getConstraintChildrenNumber() - 1; i >= 0; i--) 
					for (int j=0; j < Math.pow(2, i); j++) 
						if (j==0) {
							tempNodePath.add(getNode(OrderedChildrenList.get(i),destination));
						} else {
							tempNodePath.add(getNode(OrderedChildrenList.get(i),tempNodePath.get(j) ));
						}

				// For every feature child, duplicates the paths leading to each node
				for (int i=0; i < tempNodePath.size(); i++) 
					for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) {
						if (i==0 && j!=0) {
							mddNodePath.add(destination);
						} else {
							mddNodePath.add(tempNodePath.get(i) );
						}
						
						// Code that splits the path in the duplicated variables
						if (mddNodePath.size() == 64 && ocl.getDups() > 1) {
							mddNodePath.add(0);
							int[] helper = new int[65];
	   						// Turns the Vector<Integer> that has been filled into an int[]
	   						for (int k=0; k<65; k++)
	   							helper[k] = mddNodePath.get(k);
							// After reaching 65 paths, creates a bottom level node (for MDD ordering reasons)
		   					mddNodePathMatrix.get(0).add(duplicatedVar[duplicatedVar.length-1].getNode(helper));
		   					mddNodePath.clear();
						}

		   				// Creation of higher level nodes paths (whenever a level reaches bounds, we pack it in the higher level
		   				for (int k = 1; k<ocl.getDups()-1; k++) {
		   					if (mddNodePathMatrix.get(k-1).size() == 64) {
		   						mddNodePathMatrix.get(k-1).add(0);
		   						int[] helper = new int[64];
		   						// Turns the Vector<Integer> that has been filled into an int[]
		   						for (int l=0; l<64; l++)
		   							helper[l] = mddNodePathMatrix.get(k-1).get(l);
		   						
		   						mddNodePathMatrix.get(k).add(duplicatedVar[ (duplicatedVar.length-1) - k ].getNode(helper));
			   					mddNodePathMatrix.get(k-1).clear();
		   					}
		   				}
					}

				if (ocl.getDups() > 1) {
   					mddNodePath.clear();
		   			mddNodePath = mddNodePathMatrix.get(ocl.getDups()-2);
		   			mddNodePath.add(0);
				}
			}
		}
		
		
		// System.out.println("Mdd size: "+mddNodePath.size());
		int[] finalPath = new int[mddNodePath.size()];
		for (int i=0;i<mddNodePath.size();i++) {
			finalPath[i] = mddNodePath.get(i);
			 System.out.print(mddNodePath.get(i) + " ");
		}
		
		// System.out.println("Var Name: "+ var.key);
		// System.out.println("Var Bounds: "+ var.nbval);
		// for (int i=0;i<finalPath.length;i++) {
		//  	System.out.println("i: "+ i + ", Dest: " + finalPath[i]);
		// }
		
		// Only instance of the creation of the newNode. Each section outputs a mddNodePath list with the list for the top level variable.
		newNode = var.getNode(finalPath);

		System.out.println("\nNew Node: "+ newNode + " on Var:  "+ var.toString() );
		return newNode;
	}
    
}
