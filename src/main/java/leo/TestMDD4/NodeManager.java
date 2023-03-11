package leo.TestMDD4;

import java.util.Vector;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class NodeManager {
	
	private Node currentNode;
	private Vector<Node> featCh;
	private Vector<Node> orCh;
	private Vector<Node> andCh;
	private Vector<Node> altCh;
	private Vector<Node> featMandatoryCh;
	private Vector<Node> orMandatoryCh;
	private Vector<Node> andMandatoryCh;
	private Vector<Node> altMandatoryCh;
	
	
	public NodeManager (Node n) {
		currentNode = n;
		featCh = new Vector<Node>();
		orCh = new Vector<Node>();
		andCh = new Vector<Node>();
		altCh = new Vector<Node>();
		featMandatoryCh = new Vector<Node>();
		orMandatoryCh = new Vector<Node>();
		andMandatoryCh = new Vector<Node>();
		altMandatoryCh = new Vector<Node>();
		
		
		NodeList children = currentNode.getChildNodes();
    	// Collect information on each child
    	for(int i = 0; i < children.getLength(); i++) {
    		Boolean currentMandatory = false;
    		// Only consider mandatory children for AND constraints
			if (children.item(i).getAttributes() != null)
				if (children.item(i).getAttributes().getNamedItem("mandatory") != null && currentNode.getNodeName() == "and")
					currentMandatory = true;
			
			// Grabbing information from the node's children
	    	switch (children.item(i).getNodeName()) {
		   		case ("feature"):
		   			if (currentMandatory)
		   				featMandatoryCh.add(children.item(i));
		   			else
		   				featCh.add(children.item(i));
			   	break;
		   		// Add as variable
		   		case ("and"):
		   			if (currentMandatory )
		   				andMandatoryCh.add(children.item(i));
		   			else
		   				andCh.add(children.item(i));
			   	break;
		   		case ("or"):
		   			if (currentMandatory) 
		   				orMandatoryCh.add(children.item(i));
		   			else 
		   				orCh.add(children.item(i));
			   	break;
		   		case ("alt"):
		   			if (currentMandatory) 
		   				altMandatoryCh.add(children.item(i));
		   			else 
		   				altCh.add(children.item(i));
		   		break;
	    	}
    	}
    	
	}
	
	public Boolean isMandatory() {
		if (currentNode.getAttributes() != null)
			if (currentNode.getAttributes().getNamedItem("mandatory") != null )
				return true;
		return false;
	}
	
	
	public Boolean isMDDLeaf() {
		if(orCh.isEmpty() && andCh.isEmpty() && altCh.isEmpty() && orMandatoryCh.isEmpty() && 
				andMandatoryCh.isEmpty() && altMandatoryCh.isEmpty() )
			return true;
		else
			return false;
	}
	
	public String getName() {
		return currentNode.getAttributes().getNamedItem("name").getNodeValue();
	}

	public String getType() {
		return currentNode.getNodeName();
	}
	
	public int getConstraintChildrenNumber() {
		return orCh.size() + andCh.size() + altCh.size();
	}
	
	public int getMandatoryConstraintChildrenNumber() {
		return orMandatoryCh.size() + andMandatoryCh.size() + altMandatoryCh.size();
	}
	
	public int getFeatureChildrenNumber() {
		return featCh.size();
	}
	
	public int getMandatoryFeatureChildrenNumber() {
		return featMandatoryCh.size();
	}
	
	public int getTotalChildrenNumber() {
		return featCh.size() + orCh.size() + andCh.size() + altCh.size() +
				featMandatoryCh.size() + orMandatoryCh.size() + andMandatoryCh.size() + altMandatoryCh.size();
	}

	public int getMandatoryChildrenNumber() {
		return featMandatoryCh.size() + orMandatoryCh.size() + andMandatoryCh.size() + altMandatoryCh.size();
	}
	
	public int getDups() {
		if (currentNode.getNodeName() == "alt")
			return (int) Math.ceil( this.getTotalChildrenNumber() / 64.0);
		else if (this.getTotalChildrenNumber() == this.getMandatoryChildrenNumber())
			return (int) 1;
		else			
			// Subtracting mandatoryChildren simplifies the structure of the MDD by removing all mandatory features from the equation,
			// due to the fact there's a single path leading to T from all mandatory features. Doesn't impact OR (no mandatory children)
			return (int) Math.ceil( (this.getTotalChildrenNumber() - this.getMandatoryChildrenNumber() ) / 6.0);
	}
	
	public byte getBounds() {
		// Boundary corresponds to the # of child nodes mod 64. 
		// 63 -> 64 | 64 -> 2 | 65 -> 3
		if (currentNode.getNodeName() == "alt") {
			if (this.getTotalChildrenNumber() < 65)
				return (byte) ( this.getTotalChildrenNumber() + 1 ) ;
			else
				return (byte) ( (byte) Math.ceil(this.getBoundsNoDups()/64.0)) ;
		} 
		else if (this.getTotalChildrenNumber() == this.getMandatoryChildrenNumber()) {
				// Very specific case of a leaf node 
				return (byte) 3;
		}
		else {
			// cNN - (6*(dups-1)) corresponds to the number of children mod 6, where 6 stays 6 rather than 0.
			// 6 children = 2^6 | 7 children = 2^1 
			if ( (currentNode.getNodeName() == "and") || this.getDups() > 1) 
				// AND nodes and duplicated nodes require one more to avoid flattening of node
				return (byte) (Math.pow( 2, (this.getTotalChildrenNumber() - this.getMandatoryChildrenNumber() ) - (6*(this.getDups()-1)) ) + 1);
			else
				return (byte) Math.pow( 2, (this.getTotalChildrenNumber() - this.getMandatoryChildrenNumber() ) - (6*(this.getDups()-1)) );
		}
	}
	

	/**
	 * Recursive function that returns the NodeManager of the node with the given named node among his children
	 * 
	 * @param root: Node to examine his children. Starts with the root
	 * @param name: the name of the node we're looking the parent of
	 * @return NodeManager of the node with the given named node among his children, null if not found
	 */
    static public NodeManager findNodeParent (Node root, String name) {

		NodeManager rootNM = new NodeManager(root);
		if ( CTCManager.getChildrenPosition(rootNM, name) != -1 )
			return rootNM;

    	if (!rootNM.isMDDLeaf()) {  
	    	for (Node n : rootNM.getOrderedConstraintChildrenList()) {  
		    	// Recursive call if the node has child nodes  
		    	NodeManager node = findNodeParent(n, name);   
		    	if (node != null)
		    		return node;
		    }  
	    }
    	return null;
    }

	public double getBoundsNoDups() {
		// Used for calculation in duplicated variables
		if (currentNode.getNodeName() == "alt")
			return (double) ( this.getTotalChildrenNumber() + 1 );
		else if (this.getTotalChildrenNumber() == this.getMandatoryChildrenNumber())
			return (double) 2;
		else 
			return (double) Math.pow( 2, (this.getTotalChildrenNumber() - this.getMandatoryChildrenNumber() ) );
	}

	public Vector<Node> getOrderedConstraintChildrenList() {
		Vector<Node> OrderedChildrenList = new Vector<Node>();
		
		switch (currentNode.getNodeName()) {
			case ("or"):
			case ("alt"):
				OrderedChildrenList.addAll(altCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(orCh);
			break;
			case ("and"):
				OrderedChildrenList.addAll(altMandatoryCh);
				OrderedChildrenList.addAll(andMandatoryCh);
				OrderedChildrenList.addAll(orMandatoryCh);
				OrderedChildrenList.addAll(altCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(orCh);
			break;
		}
		
		return OrderedChildrenList;
	}
	
	public Vector<Node> getOrderedChildrenList() {
		Vector<Node> OrderedChildrenList = new Vector<Node>();
		
		switch (currentNode.getNodeName()) {
			case ("or"):
			case ("alt"):
				OrderedChildrenList.addAll(altCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(orCh);
				OrderedChildrenList.addAll(featCh);
			break;
			case ("and"):
				OrderedChildrenList.addAll(altMandatoryCh);
				OrderedChildrenList.addAll(andMandatoryCh);
				OrderedChildrenList.addAll(orMandatoryCh);
				OrderedChildrenList.addAll(featMandatoryCh);
				OrderedChildrenList.addAll(altCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(orCh);
				OrderedChildrenList.addAll(featCh);
			break;
		}
		
		return OrderedChildrenList;
	}
	
	
	
	
	
	
}
