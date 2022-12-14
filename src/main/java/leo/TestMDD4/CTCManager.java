package leo.TestMDD4;

import java.util.Vector;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class CTCManager {
	private Node currentNode;

	public CTCManager (Node n) {
		currentNode = n;
		
		NodeList children = currentNode.getChildNodes();
    	// For each child, check whether any child is not a feature
    	for(int i = 0; i < children.getLength(); i++) {
    		// Only consider mandatory children for AND constraints
			if (children.item(i).getAttributes() != null)
				if (children.item(i).getAttributes().getNamedItem("mandatory") != null && currentNode.getNodeName() == "and")
					// Do Nothing
			
			// Grabbing information from the node's children
	    	switch (children.item(i).getNodeName()) {
		   		case ("rule"):
		   			
		   		break;
	    	}
    	}
    	
	}
}
