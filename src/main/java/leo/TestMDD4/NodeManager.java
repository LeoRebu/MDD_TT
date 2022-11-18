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
		//if (currentNode.getNodeName() == "and") {
			featMandatoryCh = new Vector<Node>();
			orMandatoryCh = new Vector<Node>();
			andMandatoryCh = new Vector<Node>();
			altMandatoryCh = new Vector<Node>();
		//}
		
		
		NodeList children = currentNode.getChildNodes();
    	// For each child, check whether any child is not a feature
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
			// NEED TO CHECK MATHS FOR DUPS
			return (int) Math.ceil(this.getTotalChildrenNumber()/64.0);
		else if (currentNode.getNodeName() == "and" && this.getMandatoryChildrenNumber()>0)				
			// subtracting (mandatoryChildren-1) simplifies the structure of the MDD by grouping all mandatory features into
	    	// a single sub boolean value, due to the fact there's a single path leading to T from all mandatory features.
			return (int) Math.ceil( (this.getTotalChildrenNumber() - (this.getMandatoryChildrenNumber() - 1)) / 6.0);
		else
			return (int) Math.ceil( this.getTotalChildrenNumber() / 6.0);
	}
	
	public byte getBounds() {
		// Boundary corresponds to the # of child nodes mod 64. 
		// 64 -> 64 | 65 -> 2
		if (currentNode.getNodeName() == "alt")
			return (byte) (this.getTotalChildrenNumber() - ( 63*(this.getDups()-1) ));
		// cNN - (6*(dups-1)) corresponds to the number of children mod 6, where 6 stays 6 rather than 0.
		// 6 children = 2^6 | 7 children = 2^1 
		else if (currentNode.getNodeName() == "and" && this.getMandatoryChildrenNumber()>0)
			return (byte) Math.pow( 2, (this.getTotalChildrenNumber() - (this.getMandatoryChildrenNumber() - 1)) - (6*(this.getDups()-1)) );
		else
			return (byte) Math.pow( 2,this.getTotalChildrenNumber()-(6*(this.getDups()-1)) );
	}
	

	public Vector<Node> getOrderedConstraintChildrenList() {
		Vector<Node> OrderedChildrenList = new Vector<Node>();
		
		switch (currentNode.getNodeName()) {
			case ("or"):
				OrderedChildrenList.addAll(orCh);
				// OrderedChildrenList.addAll(orMandatoryCh); // Always empty
				OrderedChildrenList.addAll(andCh);
				// OrderedChildrenList.addAll(andMandatoryCh); // Always empty
				OrderedChildrenList.addAll(altCh);
				// OrderedChildrenList.addAll(altMandatoryCh); // Always empty
			break;
			case ("and"):
				OrderedChildrenList.addAll(orMandatoryCh);
				OrderedChildrenList.addAll(andMandatoryCh);
				OrderedChildrenList.addAll(altMandatoryCh);
				OrderedChildrenList.addAll(orCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(altCh);
			break;
			case ("alt"):
				OrderedChildrenList.addAll(orCh);
				// OrderedChildrenList.addAll(orMandatoryCh); // Always empty
				OrderedChildrenList.addAll(andCh);
				// OrderedChildrenList.addAll(andMandatoryCh); // Always empty
				OrderedChildrenList.addAll(altCh);
				// OrderedChildrenList.addAll(altMandatoryCh); // Always empty
			break;
		}
		
		return OrderedChildrenList;
	}
	
	public Vector<Node> getOrderedChildrenList() {
		Vector<Node> OrderedChildrenList = new Vector<Node>();
		
		switch (currentNode.getNodeName()) {
			case ("or"):
				OrderedChildrenList.addAll(orCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(altCh);
				OrderedChildrenList.addAll(featCh);
			break;
			case ("and"):
				OrderedChildrenList.addAll(orCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(altCh);
				OrderedChildrenList.addAll(featCh);
				OrderedChildrenList.addAll(orMandatoryCh);
				OrderedChildrenList.addAll(andMandatoryCh);
				OrderedChildrenList.addAll(altMandatoryCh);
				OrderedChildrenList.addAll(featMandatoryCh);
			break;
			case ("alt"):
				OrderedChildrenList.addAll(altCh);
				OrderedChildrenList.addAll(andCh);
				OrderedChildrenList.addAll(orCh);
				OrderedChildrenList.addAll(featCh);
			break;
		}
		
		return OrderedChildrenList;
	}
	
	
	
	
	
	
}
