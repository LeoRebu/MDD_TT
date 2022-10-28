package leo.TestMDD4;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.PathSearcher;
import org.colomoto.mddlib.operators.MDDBaseOperators;

import ctwedge.fmtester.Converter;
import ctwedge.ctWedge.CitModel;
import ctwedge.generator.medici.MediciCITGenerator;
import ctwedge.generator.util.Utility;

import pMedici.util.ModelToMDDConverter;
import pMedici.util.Operations;
import pMedici.util.TestModel;




/**
 * Hello world!
 *
 */
public class App 
{
	/*
	private static int sigToUnsig (int n) {
		if ( n<0 )
			n = (n*(-1))+127;
		return n;
	}

	private static int unsigToSig (int n) {
		if ( n > 127 )
			n = ( n - 127 ) * (-1);
		return n;
	}*/
	
    public static void main( String[] args ) throws IOException, InterruptedException {
   		//File file = new File("./featureModels/gplModel.xml");  
    	String modelName = "waterloo";
   		String xmlPath = "D:\\Leo\\Tesi MDD\\eclipse-workspace\\TestMDD4\\featureModels\\"+modelName+"Model.xml";
   		String ctwPath = "D:\\Leo\\Tesi MDD\\eclipse-workspace\\TestMDD4\\featureModels\\"+modelName+"CTWedge.ctw";
   		String mediciPath = "D:\\Leo\\Tesi MDD\\eclipse-workspace\\TestMDD4\\featureModels\\"+modelName+"CITMod.txt";

   		System.out.print(xmlPath);
   		// Converts FM to CTW, both as a file in ctwPath and as a string in ctwStr
   		try {
   			String ctwStr = "";
   			ctwStr = Converter.fromFMtoCTWedge_ENUM(xmlPath, ctwPath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
   		
   		
		CitModel model = null;
		model = Utility.loadModelFromPath(ctwPath);
		MediciCITGenerator gen = new MediciCITGenerator();
		MediciCITGenerator.OUTPUT_ON_STD_OUT_DURING_TRANSLATION = false;
		String mediciModel = gen.translateModel(model, false);
		File modelFile = new File(mediciPath);
		FileWriter wf;
		wf = new FileWriter(modelFile);
		wf.write(mediciModel);
		wf.close();
   		

		TestModel m = Operations.readFile(mediciPath); // TestModel = model with constraints

   		System.out.print("\nBefore ModelToMDDConverter\n");
   		ModelToMDDConverter mc = new ModelToMDDConverter(m);
   		System.out.print("\nBefore getMDD\n");
		MDDManager manager = mc.getMDD();
   		System.out.print("\nBefore getStartingNode\n");
		int baseMDD = mc.getStartingNode();

   		System.out.print("\nBefore updateMDDWithConstraints\n");
		baseMDD = Operations.updateMDDWithConstraints(manager, m, baseMDD);

   		System.out.print("\nBefore new PathSearcher\n");
		PathSearcher searcher = new PathSearcher(manager, 1);

   		System.out.print("\nBefore searcher setNode\n");
		searcher.setNode(baseMDD);
   		System.out.print("\nBefore countPaths\n");
		int nPaths = searcher.countPaths();

   		System.out.print("Paths #:\n " + nPaths + "\n\n");
		
   		System.out.print("BaseMDD Dump:\n\n " + manager.dumpMDD(baseMDD) + "\n\n\n");
    }
    
    
    
    
    
    
    
    public static void mddTest() {
        MDDVariableFactory varios = new MDDVariableFactory();
        
        varios.add("E",(byte)4);
        varios.add("A",(byte)8);
        varios.add("S",(byte)4);
        
        System.out.print("Values: " + varios.getNbValue("x") + "\n");
        
        System.out.print(varios.size() + "\n");
        
        MDDManager tree = MDDManagerFactory.getManager(varios, 2);
        
        /*for (int x : tree.getChildren(0)) {
        	System.out.print(x);
        }*/
        //MDDVariable[] x = tree.getAllVariables();
        System.out.print("s node ID: " + tree.getVariableForKey("S").getNode(new int[]{0,0,0,1}) + "\n");
        System.out.print("a node ID: " + tree.getVariableForKey("A").getNode(new int[]{0,2,1,2,1,2,1,2}) + "\n");
        System.out.print("a node ID: " + tree.getVariableForKey("A").getNode(new int[]{0,2,0,0,1,2,0,0}) + "\n");
        System.out.print("e node ID: " + tree.getVariableForKey("E").getNode(new int[]{0,22,12,0}) + "\n");
        //System.out.print("z node ID: " + tree.getVariableForKey("z").getNode(new int[]{1,1,1,1}) + "\n");
        //System.out.print("y index: " + tree.getVariableIndex(tree.getVariableForKey("y"))+ "\n");
        System.out.print("Leaves: " + tree.getLeafCount() + "\n" );
        for (MDDVariable x : tree.getAllVariables()) {
        	System.out.print(x.toString());
        }
        System.out.print(tree.getNodeCount() + "\n\n");
        System.out.print(tree.reach(22,new byte[]{0,2}) + "\n\n");

        System.out.print(tree.dumpMDD(32));
    }
}
