package leo.TestMDD4;

import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;


public class NodeManagerTest {
	NodeManager nodeManager;
	Vector<Node> orList;
	Vector<Node> andList;
	Vector<Node> altList;

	public NodeManagerTest() throws SAXException, IOException, ParserConfigurationException {

		DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();  
		Document document = documentBuilder.parse("featureModels/testModel.xml");  
		NodeList nodeList;
		
   		orList = new Vector<Node>();
   		andList = new Vector<Node>();
   		altList = new Vector<Node>();

   		nodeList = document.getElementsByTagName("or");
   		for (int i=0; i<nodeList.getLength();i++) 
   			orList.add(nodeList.item(i));
   		
   		nodeList = document.getElementsByTagName("and");
   		for (int i=0; i<nodeList.getLength();i++) 
   			andList.add(nodeList.item(i));
   		
   		nodeList = document.getElementsByTagName("alt");
   		for (int i=0; i<nodeList.getLength();i++) 
   			altList.add(nodeList.item(i));
   		
	}
	
	@Test
	public void orTest () {
		// System.out.println(nodeManager.getBounds());
		
		// 1: 7 children, 4 of which are features, not leaf
		nodeManager = new NodeManager( orList.get(0) );
		assertTrue(nodeManager.getName().equals("1"));
		assertTrue(nodeManager.getType().equals("or"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 7);
		assertTrue(nodeManager.getBounds() == 2);
		assertTrue(nodeManager.getFeatureChildrenNumber() == 4);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 2);
		assertTrue(!nodeManager.isMDDLeaf());
		assertTrue(nodeManager.getOrderedConstraintChildrenList().size() == 3 );
		assertTrue(
				nodeManager.getOrderedConstraintChildrenList().get(0).getNodeName() == "or" &&
				nodeManager.getOrderedConstraintChildrenList().get(1).getNodeName() == "and" &&
				nodeManager.getOrderedConstraintChildrenList().get(2).getNodeName() == "alt"
				);
		

		// 2: 3 children, 0 of which are features, not leaf
		nodeManager = new NodeManager( orList.get(1) );
		assertTrue(nodeManager.getName().equals("2"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 3);
		assertTrue(nodeManager.getBounds() == 8);
		assertTrue(nodeManager.getFeatureChildrenNumber() == 0);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(!nodeManager.isMDDLeaf());
		assertTrue(nodeManager.getOrderedConstraintChildrenList().size() == 3 );
		assertTrue(
				nodeManager.getOrderedConstraintChildrenList().get(0).getNodeName() == "and" &&
				nodeManager.getOrderedConstraintChildrenList().get(1).getNodeName() == "and" &&
				nodeManager.getOrderedConstraintChildrenList().get(2).getNodeName() == "alt"
				);
	}

	@Test
	public void altTest () {
		// System.out.println(nodeManager.getBounds());
		
		// TEST (root): 1 child
		nodeManager = new NodeManager( altList.get(0) );
		assertTrue(nodeManager.getName().equals("TEST"));
		assertTrue(nodeManager.getType().equals("alt"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 1);
		assertTrue(nodeManager.getBounds() == 2);
		assertTrue(nodeManager.getFeatureChildrenNumber() == 0);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(!nodeManager.isMDDLeaf());

		// 217: 2 children, 2 of which are features, leaf
		nodeManager = new NodeManager( altList.get(1) );
		assertTrue(nodeManager.getName().equals("217"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 2);
		assertTrue(nodeManager.getBounds() == 3);
		assertTrue(nodeManager.getFeatureChildrenNumber() == 2);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(nodeManager.isMDDLeaf());

		// 223: 2 children, 2 of which are features, leaf
		nodeManager = new NodeManager( altList.get(2) );
		assertTrue(nodeManager.getName().equals("223"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 3);
		assertTrue(nodeManager.getBounds() == 4);
		assertTrue(nodeManager.getFeatureChildrenNumber() == 3);
		assertTrue(nodeManager.getConstraintChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(nodeManager.isMDDLeaf());
		assertTrue(nodeManager.getOrderedConstraintChildrenList().size() == 0 );

		// 23: 4 children, 3 of which are features, not leaf
		nodeManager = new NodeManager( altList.get(3) );
		assertTrue(nodeManager.getName().equals("23"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 4);
		assertTrue(nodeManager.getBounds() == 5);
		assertTrue(nodeManager.getConstraintChildrenNumber() == 1);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(!nodeManager.isMDDLeaf());
		assertTrue(nodeManager.getOrderedConstraintChildrenList().size() == 1 );
		assertTrue(
				nodeManager.getOrderedConstraintChildrenList().get(0).getNodeName() == "and" 
				);
	}

	@Test
	public void andTest () {
		// System.out.println(nodeManager.getName());

		// 21: 7 children, 2 of which mandatory, not leaf
		nodeManager = new NodeManager( andList.get(0) );
		assertTrue(nodeManager.getName().equals("21"));
		assertTrue(nodeManager.getType().equals("and"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 7);
		assertTrue(nodeManager.getBounds() == 32);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 2);
		assertTrue(nodeManager.getMandatoryFeatureChildrenNumber() == 2);
		assertTrue(nodeManager.getConstraintChildrenNumber() == 3);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(!nodeManager.isMDDLeaf());
		assertTrue(nodeManager.getOrderedConstraintChildrenList().size() == 3 );
		assertTrue(
				nodeManager.getOrderedConstraintChildrenList().get(0).getNodeName() == "and" &&
				nodeManager.getOrderedConstraintChildrenList().get(1).getNodeName() == "and" &&
				nodeManager.getOrderedConstraintChildrenList().get(2).getNodeName() == "alt"
				);
		assertTrue(nodeManager.getOrderedChildrenList().size() == 7 );
		
		// 215: 14 children, 12 of which mandatory, leaf
		nodeManager = new NodeManager( andList.get(1) );
		assertTrue(nodeManager.getTotalChildrenNumber() == 14);
		assertTrue(nodeManager.getBounds() == 4);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 12);
		assertTrue(nodeManager.getName().equals("215"));
		assertTrue(nodeManager.getConstraintChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(nodeManager.isMDDLeaf());
		assertTrue(nodeManager.getOrderedConstraintChildrenList().size() == 0 );
		
		// 216: 8 children, 0 of which mandatory, leaf
		nodeManager = new NodeManager( andList.get(2) );
		assertTrue(nodeManager.getName().equals("216"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 8);
		assertTrue(nodeManager.getBounds() == 4);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 0);
		assertTrue(nodeManager.getConstraintChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 2);
		assertTrue(nodeManager.isMDDLeaf());

		// 22: 6 children, 1 of which mandatory, not leaf
		nodeManager = new NodeManager( andList.get(3) );
		assertTrue(nodeManager.getName().equals("22"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 6);
		assertTrue(nodeManager.getBounds() == 32);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 1);
		assertTrue(nodeManager.getConstraintChildrenNumber() == 1);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(!nodeManager.isMDDLeaf());

		// 234: 2 children, 1 of which mandatory, leaf
		nodeManager = new NodeManager( andList.get(4) );
		assertTrue(nodeManager.getName().equals("234"));
		assertTrue(nodeManager.getTotalChildrenNumber() == 2);
		assertTrue(nodeManager.getBounds() == 2);
		assertTrue(nodeManager.getMandatoryChildrenNumber() == 1);
		assertTrue(nodeManager.getConstraintChildrenNumber() == 0);
		assertTrue(nodeManager.getDups() == 1);
		assertTrue(nodeManager.isMDDLeaf());
	}
	
	
}
