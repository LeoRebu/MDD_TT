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


    static ArrayList<String> readConstraint (Node n) {
    	NodeList constraintList = n.getChildNodes();
    	int count = 0;
    	ArrayList<String> ret = new ArrayList<String>();

    	
    	for (int i = 0; i < constraintList.getLength(); i++) {  
    		Node elemNode = constraintList.item(i);  
    		// We only care about element nodes
    		if (elemNode.getNodeType() == Node.ELEMENT_NODE) {  
	    		switch (elemNode.getNodeName()) {
	    		case ("var"): 
	    			// Add the name of the var to the list
	    			ret.add(elemNode.getTextContent());
	    			break;
	    		case ("not"): 
	    			// Recursively explore the children of the node and then negate them 
	    			ret.addAll(readConstraint(elemNode));
    				ret.add("-");
	    			break;
	    		case ("conj"): 
	    			// Recursively explore the children of the node and then link them with an and in postfix notation
	    			ret.addAll(readConstraint(elemNode));
    				ret.add("*");
	    			break;
	    		case ("disj"): 
	    			// Recursively explore the children of the node and then link them with an or in postfix notation
	    			ret.addAll(readConstraint(elemNode));
    				ret.add("+");
	    			break;
	    		case ("imp"): 
	    			// Recursively explore the children of the node (noting the first child needs negating) and then link them with an or in postfix notation
	    			ret.addAll(readConstraint(elemNode));
	    			ret.add("+");
	    			break;
	    		case ("eq"): 
	    			// Recursively explore the children of the node, linking them with an and in postfix notation
	    			ret.addAll(readConstraint(elemNode));
	    			ret.add("*");
	    			// Recursively explore the children of the node, linking them with an or in postfix notation and negating it
	    			ret.addAll(readConstraint(elemNode));
	    			ret.add("+");
	    			ret.add("-");
	    			// Complete the XNOR by linking the previous two statements with an or in postfix notation
	    			ret.add("+");
	    			break;
	    		}
        		if (n.getNodeName() == "imp") {
        			// Only negate the first item
        			if (count++ == 0) 
        				ret.add("-");
        		}
	    	}  
    	} 
    	
    	return ret;
    }
	

	public int getMDDFromVar(String name) {
		
		int newNode = -1;
		NodeManager parentNode;
		MDDVariable var;
		int position;
		int dim;
		Integer varPosition = -1;
		parentNode = NodeManager.findNodeParent(structRoot, name);
		
   		position = parentNode.getChildPosition(name);
   		
   		if (position > 0)
   			varPosition =  parentNode.getDups() - (int) Math.ceil( position / 6.0 );
   		else if (position == -2)
   			varPosition = 0;
   		else 
   			System.out.println("Var position not found");
   		

   		var = manager.getVariableForKey(parentNode.getName()+"Var" + varPosition.toString());
   		dim = var.nbval;

		// Reflects its position in the (possibly duplicated) variable. Has to be between 1 and 6 included.
		int realPosition;
		if (position>0 && parentNode.getType() != "alt")
			realPosition = (position - 6 * ((int) Math.ceil( position / 6.0 ) - 1));
			// realPosition = position + 6 - (int) Math.ceil( position / 6.0 ) * 6;
		else
			realPosition = position;

    	newNode = var.getNode( getChildrenList(parentNode.getType(), dim, realPosition ) );
    	
    	// The path needs to be forced starting from the first node
    	if (!structRoot.getAttributes().getNamedItem("name").getNodeValue().equals(parentNode.getName())) {
    		int fatherNode = getMDDFromVar(parentNode.getName());
    		newNode = MDDBaseOperators.AND.combine(manager, newNode, fatherNode);
    	}
    	
		return newNode;

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
/*
		System.out.print("Pos: "+ pos + ", type: " + type + " - ");
		for (int i=0;i<dim;i++)
			System.out.print(childrenList[i] + " ");
		System.out.println();
		System.out.println();*/
		
		return childrenList;
	}
	
}
