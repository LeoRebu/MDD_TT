package leo.TestMDD4;

import java.util.Vector;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import pMedici.safeelements.ExtendedSemaphore;


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


    // TODO: Completely implement NodeManager into variable generation, including children ordering
    public void variablesGenerator (Vector<Node> nodes) {
		
    	for (int count = 0; count < nodes.size(); count++) {  
   			NodeManager ocl = new NodeManager(nodes.get(count));
    		// Prints out the name of the node (and, or, alt or feature)  
    		System.out.println("\nNode Name =" + ocl.getType() + " [OPEN]");  
    		// Prints out the name of the element
   			System.out.println("Name =" +  ocl.getName()); 
    		System.out.println("ChildNodesNum#: "+ ocl.getTotalChildrenNumber());
    		System.out.println("Dups#: "+ ocl.getDups());
    		
    		String varName = ocl.getName() + "Var";
    			
    		switch (ocl.getType()) {
		    	// Add as variable
		    	case ("and"):
		    	case ("or"):
		    		System.out.println("B4 AND or OR Mngmt\n");
			    	for (int i=0; i<ocl.getDups(); i++) {
			    		
			    		if (i == 0)
			    			mvf.add(varName + i, ocl.getBounds());
			    		else 
			    			// 64 for variables, 65th for a zero to prevent flattening of node
			    			mvf.add(varName + i, (byte)65 );
			        	this.varCount++;
			    	}
		    	break;
		    	// Add as variable
		    	case ("alt"):
		    		System.out.println("B4 ALT Mngmt\n");
		    	
		    		if (ocl.getTotalChildrenNumber()>1) {
			   			for (int i=0; i<ocl.getDups(); i++) {
			   				if (i == 0)
			   					mvf.add(varName + i, ocl.getBounds() );
			   				else
				    			// 64 for variables, 65th for a zero to prevent flattening of node
			   					mvf.add(varName + i, (byte)65 );
			   				this.varCount++;
			   			}
		    		} else {
	    				mvf.add(varName + 0, (byte)2 );
		    			this.varCount++;
		    		}
		    			
		    	break;
    		}

    		// Only check children if we know there's more constraints downstream
	    	if (!ocl.isMDDLeaf()) {  
		    	// Recursive call if the node has child nodes  
		    	variablesGenerator(ocl.getOrderedConstraintChildrenList());  
	    	}  
	    	
    	}
    }
    
    public void displayVars() {

		System.out.println("B4 factory\n");
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
    

	public int getStartingNode(Node currentNode, int destination) throws InterruptedException {
		
		if (manager == null)
			throw new RuntimeException(
					"You need to create an MDDManager with getMDD() before obtaining the starting node");

		NodeManager ocl = new NodeManager(currentNode);
		String varName = ocl.getName()+"Var";
		System.out.println("var: "+varName+0);
		Vector<Node> OrderedChildrenList = ocl.getOrderedConstraintChildrenList();
		
		MDDVariable var = manager.getVariableForKey(varName+0);
		MDDVariable[] duplicatedVar = new MDDVariable[ocl.getDups()-1];
		
		for (int i=0; i< (ocl.getDups()-1);i++) {
			duplicatedVar[i] = manager.getVariableForKey(varName+(i+1));
		}
			
		
		// TODO: Manage duplications, check that bounds calculations correspond on both sides (variable creation and node creation)
		Vector<Vector<Integer>> mddNodePathMatrix = new Vector<Vector<Integer>>();
		
		Vector<Integer> mddNodePath = new Vector<Integer>();
		Vector<Integer> tempNodePath = new Vector<Integer>();

		// Only check children if we know there's more constraints downstream
		if (ocl.isMDDLeaf()) {  
			mddNodePath.add(0);
	    	switch (ocl.getType()) {
		    	// Depending on how many mandatory features
		    	case ("and"):
		    	case ("or"):
		    	case ("alt"):
		    		if (ocl.getDups() == 1) {
		    			System.out.println("var: "+varName+0);
		    			System.out.println("Bounds no dups: "+ ocl.getBoundsNoDups());
		    			// Cycles through all the possible values obtainable by the variable.
		    			for (int j=1; j < ocl.getBoundsNoDups();j++) 
		    				mddNodePath.add(destination);
		    			
		    		} else {/*
		    			// When the variable has been duplicated.
			   			for (int i=0; i<dups; i++) {
				    	    	this.varCount++;
		    			}*/
	    			}
	    		break;
			}
			
		}  else {

			switch ( ocl.getType() ) {
				case ("and"):

					int nextNode = destination;
					

					if (ocl.getMandatoryConstraintChildrenNumber() > 0 || ocl.getMandatoryFeatureChildrenNumber() > 0) {
						// AND component followed by the OR component
						
						// PART 1.1: Mandatory constraints in series
						// For every mandatory constraint child, build the "forced" part of the tree.
						// With MCCN == 0, nextNode is destination, thus making the following OR section correct.
						for (int i = 0; i < ocl.getMandatoryConstraintChildrenNumber(); i++) {
							if (i == 0)
								nextNode = getStartingNode(OrderedChildrenList.get(i), destination);
							else
								nextNode = getStartingNode(OrderedChildrenList.get(i), nextNode );
						}
						
						// PART 1.2: Mandatory paths lead to one
						// In the specific situation where there are no mandatory constraints and at least one mandatory feature,
						// the path that would skip the optional constraints goes directly to 1 (no further constraints downstream)
						if (ocl.getMandatoryConstraintChildrenNumber() == 0 && ocl.getMandatoryFeatureChildrenNumber() > 0) 
							tempNodePath.add(0,1); // First element
						else
							tempNodePath.add(0,nextNode);
						
						
						// PART 2: Mandatory not chosen paths lead to zero
						// Set the first half of paths to zero, as they're the equivalent of saying one of the mandatory nodes is not selected.
						// Removed, as AND constraints with lots of children would've meant million of paths leading to zero
						// for (int i=0; i < ocl.getBoundsNoDups()/2 ;i++)
							// mddNodePath.add(i, 0);
						
						// PART 3: Optional constraints go in an OR structure
						// For every optional constraint child, builds the recursions necessary to create the OR structure
						// The optional constraint children being after the mandatory ones, they are indexed using the MCCN
						for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
							for (int j=0; j < Math.pow(2, i); j++) 
								if (j==0)
									tempNodePath.add((int) Math.pow(2, i)+j, getStartingNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),nextNode));
								else
									tempNodePath.add((int) Math.pow(2, i)+j, getStartingNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),tempNodePath.get((int) ((Math.pow(2, i)+j) - Math.pow(2, i))) ));
						
						// PART 4: Building final paths array, duplicating based on optional features 
						// For every optional feature child, duplicates the paths leading to each node and builds the actual path
						if ((ocl.getFeatureChildrenNumber()) > 0)
							for (int i=0; i < tempNodePath.size(); i++) 
								for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
										mddNodePath.add((int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j) + ocl.getBoundsNoDups()/2, tempNodePath.get(i));
						else
							for (int i=0; i<ocl.getBoundsNoDups();i++)
								mddNodePath.add(i, tempNodePath.get(i));


					} else {
						
						// Functionally equivalent to an OR
						tempNodePath.add(0, 0);
						// For every constraint child, builds the recursions necessary to create the or structure
						for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
							for (int j=0; j < Math.pow(2, i); j++) {
								System.out.println("Test5");
								if (j==0)
									tempNodePath.add((int) Math.pow(2, i)+j, getStartingNode(OrderedChildrenList.get(i),destination));
								else
									tempNodePath.add((int) Math.pow(2, i)+j, getStartingNode(OrderedChildrenList.get(i),tempNodePath.get((int) ((Math.pow(2, i)+j) - Math.pow(2, i)))));
							}
						// For every feature child, duplicates the paths leading to each node
						if ((ocl.getFeatureChildrenNumber()) > 0)
							for (int i=0; i < tempNodePath.size(); i++) 
								for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
									if (i==0 && j!=0) 
										mddNodePath.add((int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j), 1);
									else 
										mddNodePath.add((int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j), tempNodePath.get(i));
						
					}
					System.out.println("Current Node is AND, named: "+ ocl.getName());
					
				break;
				case ("or"):
					tempNodePath = new Vector<Integer>();
					tempNodePath.add(0,0);
					// For every constraint child, builds the recursions necessary to create the or structure
					for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
						for (int j=0; j < Math.pow(2, i); j++) 
							if (j==0) {
								tempNodePath.add((int) Math.pow(2, i)+j, getStartingNode(OrderedChildrenList.get(i),destination));
							} else {
								tempNodePath.add((int) Math.pow(2, i)+j, getStartingNode(OrderedChildrenList.get(i),tempNodePath.get((int) ((Math.pow(2, i)+j) - Math.pow(2, i)))));
							}
								
					// For every feature child, duplicates the paths leading to each node
					if ((ocl.getFeatureChildrenNumber()) > 0)
						for (int i=0; i < tempNodePath.size(); i++) 
							for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
								if (i==0 && j!=0) 
									mddNodePath.add((int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j), 1);
								else 
									mddNodePath.add((int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j), tempNodePath.get(i));

					System.out.println("Current Node is OR, named: "+ ocl.getName());
				break;
				case ("alt"):
					mddNodePath.add(0);
					if (ocl.getTotalChildrenNumber() > 1) {
						// For every constraint, one path that leads to that constraint
						for (int i=1; i < ocl.getConstraintChildrenNumber(); i++ ) {
							System.out.println("New Node Going in");
							mddNodePath.add(i, getStartingNode(OrderedChildrenList.get(i), destination));
						}
					
						// For every feature, one path that leads to T
						for (int i=1; i < ocl.getFeatureChildrenNumber(); i++ ) {
							System.out.println("New Node Going in, array is: I=" + i + " CCN="+ocl.getConstraintChildrenNumber() +" and array size is " + mddNodePath.size());
							System.out.println("OrderedChildrenList Length: "+OrderedChildrenList.size());
							mddNodePath.add(i + ocl.getConstraintChildrenNumber(),1);
						}
					} else {
						// Weird edge case where the constraint has just one child
						if (ocl.getConstraintChildrenNumber() > 0) {
							//If the child is a constraint, it goes to it
							mddNodePath.add(1,getStartingNode(OrderedChildrenList.get(0), destination));
						} else {
							// If the child is a feature, it goes to true
							mddNodePath.add(1,1);
						}
					}
				
					System.out.println("Current Node is ALT, named: "+ ocl.getName());
				break;
			}
			System.out.println("Switch is OVER");
		}
		
    	

		System.out.println("ChildNodesNum#: "+ ocl.getTotalChildrenNumber());
		System.out.println("Dups#: "+ ocl.getDups());
		
		int newNode = 1;

		System.out.println("Node Name: "+ ocl.getName());
		System.out.println("MDD Node Path#: ");
		
		
		int[] finalPath = new int[mddNodePath.size()];
		for (int i=0;i<mddNodePath.size();i++) {
			finalPath[i] = mddNodePath.get(i);
			System.out.println("i: "+ i + ", Dest: " +mddNodePath.get(i));
		}
		
		System.out.println("Var Name: "+ var.key);
		System.out.println("Var Bounds: "+ var.nbval);
		for (int i=0;i<finalPath.length;i++) {
			System.out.println("i: "+ i + ", Dest: " + finalPath[i]);
		}
		
		ExtendedSemaphore.OPERATION_SEMAPHORE.acquire();
		newNode = var.getNode(finalPath);
		ExtendedSemaphore.OPERATION_SEMAPHORE.release();

		System.out.println("New Node: "+ newNode);
		return newNode;
	}
    
    
    /*
    // Old getStartingNode
	public int getStartingNode(Node currentNode, int destination) throws InterruptedException {
		
		if (manager == null)
			throw new RuntimeException(
					"You need to create an MDDManager with getMDD() before obtaining the starting node");

		NodeManager ocl = new NodeManager(currentNode);
		String varName = ocl.getName()+"Var";
		System.out.println("var: "+varName+0);
		Vector<Node> OrderedChildrenList = ocl.getOrderedConstraintChildrenList();
		
		MDDVariable var = manager.getVariableForKey(varName+0);
		
		// TODO: Manage duplications, check that bounds calculations correspond on both sides (variable creation and node creation)
		Vector<Integer> mddNodePath2 = new Vector<Integer>();
		
		int[] mddNodePath = new int[var.nbval];
		mddNodePath[0] = 0;
		int[] tempNodePath;

		// Only check children if we know there's more constraints downstream
		if (ocl.isMDDLeaf()) {  
	    	switch (ocl.getType()) {
		    	// Depending on how many mandatory features
		    	case ("and"):
		    	case ("or"):
		    	case ("alt"):
		    		if (ocl.getDups() == 1) {
		    			System.out.println("var: "+varName+0);
		    			// Cycles through all the possible values obtainable by the variable.
		    			mddNodePath[0]=0;
		    			for (int j=1; j < var.nbval;j++) 
		    				mddNodePath[j]=destination;
		    			
		    		} else {
		    			// When the variable has been duplicated.
			   			//for (int i=0; i<dups; i++) {
				    	//    	this.varCount++;
		    			//}
	    			}
	    		break;
			}
			
		}  else {

			switch ( ocl.getType() ) {
				case ("and"):

					int nextNode = destination;
					
					if (ocl.getMandatoryChildrenNumber() > 0)
						tempNodePath = new int[(int) Math.pow(2, (ocl.getConstraintChildrenNumber() + 1) )];
					else 
						tempNodePath = new int[(int) Math.pow(2, ocl.getConstraintChildrenNumber() )];
					
					
					if (ocl.getMandatoryConstraintChildrenNumber() > 0 || ocl.getMandatoryFeatureChildrenNumber() > 0) {
						// AND component followed by the OR component
						
						// PART 1.1: Mandatory constraints in series
						// For every mandatory constraint child, build the "forced" part of the tree.
						// With MCCN == 0, nextNode is destination, thus making the following OR section correct.
						for (int i = 0; i < ocl.getMandatoryConstraintChildrenNumber(); i++) {
							if (i == 0)
								nextNode = getStartingNode(OrderedChildrenList.get(i), destination);
							else
								nextNode = getStartingNode(OrderedChildrenList.get(i), nextNode );
						}
						
						// PART 1.2: Mandatory paths lead to one
						// In the specific situation where there are no mandatory constraints and at least one mandatory feature,
						// the path that would skip the optional constraints goes directly to 1 (no further constraints downstream)
						if (ocl.getMandatoryConstraintChildrenNumber() == 0 && ocl.getMandatoryFeatureChildrenNumber() > 0) 
							tempNodePath[0] = 1;
						else
							tempNodePath[0] = nextNode;
						
						// PART 2: Mandatory not chosen paths lead to zero
						// Set the first half of paths to zero, as they're the equivalent of saying one of the mandatory nodes is not selected.
						// TODO: Could be eliminated, granting one more optional variable to be used by a single non-duplicated AND node.
						for (int i=0; i<mddNodePath.length/2;i++)
							mddNodePath[i]=0;
						
						// PART 3: Optional constraints go in an OR structure
						// For every optional constraint child, builds the recursions necessary to create the OR structure
						// The optional constraint children being after the mandatory ones, they are indexed using the MCCN
						for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
							for (int j=0; j < Math.pow(2, i); j++) 
								if (j==0)
									tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),nextNode);
								else
									tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i + ocl.getMandatoryConstraintChildrenNumber()),tempNodePath[(int) ((Math.pow(2, i)+j) - Math.pow(2, i))]);
						
						// PART 4: Building final paths array, duplicating based on optional features 
						// For every optional feature child, duplicates the paths leading to each node and builds the actual path
						if ((ocl.getFeatureChildrenNumber()) > 0)
							for (int i=0; i < tempNodePath.length; i++) 
								for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
										mddNodePath [ (int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j) + mddNodePath.length/2] = tempNodePath[i];
						else
							for (int i=mddNodePath.length/2; i<mddNodePath.length;i++)
								mddNodePath[i] = tempNodePath[i-mddNodePath.length/2];
								

					} else {
						
						// Functionally equivalent to an OR
						tempNodePath[0] = 0;
						// For every constraint child, builds the recursions necessary to create the or structure
						for (int i=0; i < (ocl.getConstraintChildrenNumber()); i++) 
							for (int j=0; j < Math.pow(2, i); j++) {
								System.out.println("Test5");
								if (j==0)
									tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i),destination);
								else
									tempNodePath[(int) Math.pow(2, i)+j] = getStartingNode(OrderedChildrenList.get(i),tempNodePath[(int) ((Math.pow(2, i)+j) - Math.pow(2, i))]);
							}
						// For every feature child, duplicates the paths leading to each node
						if ((ocl.getFeatureChildrenNumber()) > 0)
							for (int i=0; i < tempNodePath.length; i++) 
								for (int j=0; j < Math.pow(2,ocl.getFeatureChildrenNumber()); j++) 
									if (i==0 && j!=0) 
										mddNodePath [(int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j)] = 1;
									else 
										mddNodePath [ (int) (i * Math.pow(2,ocl.getFeatureChildrenNumber()) + j)] = tempNodePath[i];
						
					}
					System.out.println("Current Node is AND, named: "+ ocl.getName());
					
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

					System.out.println("Current Node is OR, named: "+ ocl.getName());
				break;
				case ("alt"):
					if (ocl.getTotalChildrenNumber() > 1) {
						// For every constraint, one path that leads to that constraint
						for (int i=1; i < ocl.getConstraintChildrenNumber(); i++ ) {
							System.out.println("New Node Going in");
							mddNodePath[i] = getStartingNode(OrderedChildrenList.get(i), destination);
						}
					
						// For every feature, one path that leads to T
						for (int i=1; i < ocl.getFeatureChildrenNumber(); i++ ) {
							System.out.println("New Node Going in, array is: I=" + i + " CCN="+ocl.getConstraintChildrenNumber() +" and array size is " + mddNodePath.length);
							System.out.println("OrderedChildrenList Length: "+OrderedChildrenList.size());
							mddNodePath[i + ocl.getConstraintChildrenNumber()] = 1;
						}
					} else {
						// Weird edge case where the constraint has just one child
						if (ocl.getConstraintChildrenNumber() > 0) {
							//If the child is a constraint, it goes to it
							mddNodePath[1] = getStartingNode(OrderedChildrenList.get(0), destination);
						} else {
							// If the child is a feature, it goes to true
							mddNodePath[1] = 1;
						}
					}
				
					System.out.println("Current Node is ALT, named: "+ ocl.getName());
				break;
			}
			System.out.println("Switch is OVER");
		}
		
    	

		System.out.println("ChildNodesNum#: "+ ocl.getTotalChildrenNumber());
		System.out.println("Dups#: "+ ocl.getDups());
		
		int newNode = 1;

		System.out.println("Node Name: "+ ocl.getName());
		System.out.println("MDD Node Path#: ");
		for (int i=0;i<mddNodePath.length;i++)
			System.out.println("i: "+ i + ", Dest: " +mddNodePath[i]);

		System.out.println("Var Name: "+ var.key);
		ExtendedSemaphore.OPERATION_SEMAPHORE.acquire();
		newNode = var.getNode(mddNodePath);
		ExtendedSemaphore.OPERATION_SEMAPHORE.release();

		System.out.println("New Node: "+ newNode);
		return newNode;
	}  */
	/*
	private int[] getChildrenList(int dim, int nextNode) {
		int[] childrenList = new int[dim];
		
		for (int i=0; i<dim; i++) {
			childrenList[i] = nextNode;
		}
		
		return childrenList;
	}*/
}
