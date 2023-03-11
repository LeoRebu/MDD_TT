package leo.TestMDD4;

import java.util.ArrayList;
import java.util.Vector;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.operators.MDDBaseOperators;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class CTCManager {
	MDDManager manager;
	int baseMDD;
	Node structRoot;
	
	
	public CTCManager () {
		manager = null;
	}
	
	public CTCManager (MDDManager mgr, int base, Node r) {
		manager = mgr;
		structRoot = r;
		baseMDD = base;
    	
	}

    static ArrayList<String> readConstraint (Node n, ArrayList<String> ret) {
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
	

	public int getMDDFromVar(String name) {
		
			
		int newNode = -1;
		NodeManager parentNode, childNode = null;
		MDDVariable var;
		int position;
		int dim;
		Integer varPosition = -1;
		parentNode = NodeManager.findNodeParent(structRoot, name);
   		position = getChildrenPosition(parentNode,name);
   		if (position > 0)
   			varPosition =  parentNode.getDups() - (int) Math.ceil( position / 6.0 );
   		else if (position == -2)
   			varPosition = 0;
   		else 
   			System.out.println("Var position not found");
   		
   		/*
		for (Node n : parentNode.getOrderedChildrenList()) {
			if (n.getAttributes().getNamedItem("name").getNodeValue().equals(name)) {
				childNode = new NodeManager(n);
			}
		}*/

   		var = manager.getVariableForKey(parentNode.getName()+"Var" + varPosition.toString());
   		
   		if (varPosition > 0) 
   			dim = 65;
   		else
   			dim = parentNode.getBounds();

		// Reflects its position in the (possibly duplicated) variable. Has to be between 1 and 6 included.
		int realPosition;
		if (position>0 && parentNode.getType() != "alt")
			realPosition = (position - 6 * ((int) Math.ceil( position / 6.0 ) - 1));
		else
			realPosition = position;

		System.out.println("Name = " + name);
		System.out.println("Node Type = " + parentNode.getType());
		System.out.println("Position = " + position);
		System.out.println("Var Position = " + varPosition);
		System.out.println("Var Name = " + var.toString());
		System.out.println("Dim = " + dim);
		System.out.println("Real Position = " + realPosition);
		System.out.println();
		System.out.println("Name = " + name + "  - Father Name = " + parentNode.getName());

    	newNode = var.getNode( getChildrenList(parentNode.getType(), dim, realPosition ) );
    	
    	// The path needs to be forced starting from the first node
    	if (!structRoot.getAttributes().getNamedItem("name").getNodeValue().equals(parentNode.getName())) {
    		int fatherNode = getMDDFromVar(parentNode.getName());
    		newNode = MDDBaseOperators.AND.combine(manager, newNode, fatherNode);
    	}
		return newNode;

	}
	
	// Returns the position of the child. -1 if not found. -2 if mandatory. 
	// For an ALT node, returns its exact position in the path. For OR and AND nodes returns the corresponding binary digit position.
	static public int getChildrenPosition(NodeManager node, String name) {
		Vector<Node> children = node.getOrderedChildrenList();
		int pos = -1;
		
		for (Node n : children) {
			if (n.getAttributes().getNamedItem("name").getNodeValue().equals(name)) {
				
				switch (node.getType()) {
				case "alt":
					// Position in the path is equal to its position in the OCL (indexed starting from 1)
					pos = children.indexOf(n) + 1;
					break;
				case "and":
				case "or":
					if (n.getAttributes().getNamedItem("mandatory") != null && node.getType().equals("and") )
						// The child is mandatory
						pos = -2;
					else
						//pos = children.indexOf(n) - node.getMandatoryChildrenNumber() + 1;
						if (n.getNodeName().equals("alt") || n.getNodeName().equals("and") || n.getNodeName().equals("or"))
							// Position of the corresponding binary digit of the path is the inverse of the list.
							//pos = children.indexOf(n) + node.getFeatureChildrenNumber() - node.getMandatoryChildrenNumber() + 1;
							pos = children.indexOf(n) - node.getMandatoryChildrenNumber() + node.getFeatureChildrenNumber() + 1;
						else
							//pos = children.size() - children.indexOf(n);
							pos = children.indexOf(n) - node.getMandatoryChildrenNumber() - node.getConstraintChildrenNumber() + 1;
					break;
				}
			}
		}
		
    	return pos;
	}
	
	
	/**
	 * Returns the list of children for a given node
	 * 
	 * @param type: the type of the node
	 * @param dim: the size of the variable
	 * @param pos: the element to be set as accepted
	 * @return the list of children for a given node
	 */
	public int[] getChildrenList(String type, int dim, int pos) {
		int[] childrenList = new int[dim];
		Boolean flag = false;
		switch (type) {
		case "alt":
			for (int i=0; i < dim; i++) {
				if (i == pos)
					childrenList[i] = 1;
				else
					childrenList[i] = 0;
			}
			break;
		case "or":
			for (int i=0; i<dim; i++) {
				if ((i>0 ) && i % Math.pow(2, pos-1) == 0)
					flag = !flag;
				
				if (flag)
					childrenList[i] = 1;
				else 
					childrenList[i] = 0;

			}
		break;
		case "and":
			if (pos == -2) {
				for (int i=0; i<dim; i++) {
					childrenList[i] = 1;
				}
				childrenList[dim-1] = 0;
			} else {
				for (int i=0; i<dim-1; i++) {
					if ((i>0 ) && i % Math.pow(2, pos-1) == 0)
						flag = !flag;
					
					if (flag)
						childrenList[i] = 1;
					else 
						childrenList[i] = 0;

				}
				childrenList[dim-1] = 0;
			}
		break;
		}

		System.out.print("Pos: "+ pos + ", type: " + type + " - ");
		for (int i=0;i<dim;i++)
			System.out.print(childrenList[i] + " ");
		System.out.println();
		System.out.println();
		
		return childrenList;
	}
	
}
