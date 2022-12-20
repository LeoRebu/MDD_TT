package leo.TestMDD4;

import java.util.Vector;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
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

	public int getMDDFromVar(String name) {
		int newNode = -1;
		NodeManager node;
		MDDVariable var;
		int position;
		int dim;
		Integer varPosition = -1;
   		node = NodeManager.findNodeParent(structRoot, name);
   		position = getChildrenPosition(node,name);
   		if (position > 0)
   			varPosition =  node.getDups() - (int) Math.ceil( position / 6.0 );
   		else if (position == -2)
   			varPosition = 0;
   		else 
   			System.out.println("Var position not found");
   		

   		var = manager.getVariableForKey(node.getName()+"Var" + varPosition.toString());
   		
   		if (varPosition > 0) 
   			dim = 65;
   		else
   			dim = node.getBounds();
   		
/*
		System.out.println("Position = " + position);
		System.out.println("Var Position = " + varPosition);
		System.out.println("Var Name = " + var.toString());
		System.out.println("Dim = " + dim);
		System.out.println("Real Position = " + (position - 6 * (int) Math.floor( position / 6.0 )));*/
   		
		// Reflects its position in the (possibly duplicated) variable. Has to be between 1 and 6 included.
		int realPosition;
		if (position>0)
			realPosition = (position - 6 * (int) Math.floor( position / 6.0 ));
		else
			realPosition = position;
		
    	newNode = var.getNode( getChildrenList(node.getType(), dim, realPosition ) );
   
		return newNode;
	}
	
	
	
	// Returns the position of the child. -1 if not found. -2 if mandatory. 
	// For an ALT node, returns its exact position in the path. For OR and AND nodes returns the corresponding binary digit position.
	static public int getChildrenPosition(NodeManager node, String name) {
		Vector<Node> children = node.getOrderedChildrenList();
		int pos = -1;
		
		for (Node n : children) {
			if (n.getAttributes().getNamedItem("name").getNodeValue().equals(name)) {
				
				if (node.getType() == "alt") {
					// Position in the path is equal to its position in the OCL (indexed starting from 1)
					pos = children.indexOf(n) + 1;
				}
				
				if (node.getType() == "or" || node.getType() == "and") {

					if (n.getAttributes().getNamedItem("mandatory") != null && node.getType() == "and")
						// The child is mandatory
						pos = -2;
					else
						// Position of the corresponding binary digit of the path is the inverse of the list.
						pos = children.size() - children.indexOf(n);
						
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
		if (type == "alt") {
			for (int i=0; i < dim; i++) {
				if (i == pos)
					childrenList[i] = 1;
				else
					childrenList[i] = 0;
			}
		}
		if (type == "or" || type == "and") {
			if (pos == -2) {
				for (int i=0; i<dim; i++) {
					childrenList[i] = 1;
				}
			} else {
				Boolean flag = false;
				for (int i=0; i<dim; i++) {
					if (flag)
						childrenList[i] = 1;
					else 
						childrenList[i] = 0;

					if (i % Math.pow(2, pos-1) == 0)
						flag = !flag;
				}
			}
		} 
		return childrenList;
	}
	
}
