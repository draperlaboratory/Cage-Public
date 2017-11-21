import java.util.Map;

import soot.Body;
import soot.Pack;
import soot.BodyTransformer;
import soot.PackManager;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.Transform;
import soot.Main;
import soot.toolkits.graph.BlockGraph;
import soot.toolkits.graph.DirectedGraph;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.toolkits.graph.UnitGraph;
import soot.toolkits.graph.ZonedBlockGraph;
import soot.util.cfgcmd.CFGToDotGraph;
import soot.util.dot.DotGraph;


public class DrawCFG extends BodyTransformer{

    private static final String UNIT = "UNITGRAPH";
    private static final String ZONED = "ZONEDBLOCKGRAPH";
    private static DrawCFG instance;

    private static SootClass getClass(String pathToClass){
        SootClass ret = Scene.v().loadClassAndSupport(pathToClass);
        return ret;
    }

    private static DirectedGraph getGraph(String type, Body b){
        DirectedGraph g = null;
        switch(type) {
        case UNIT:
            g = new ExceptionalUnitGraph(b);
            break;
        case ZONED:
            g = new ZonedBlockGraph(b);
            break;
        default:
            System.out.println("Unrecognized graph type: " + type);
            System.exit(1);
            break;
        }
        return g;
    }

    public static DrawCFG getInstance(){
        if (DrawCFG.instance == null){
            DrawCFG.instance = new DrawCFG();
        }
        return DrawCFG.instance;
    }

    private static void render(String outDir, DirectedGraph g, Body b){
        String methodName = b.getMethod().getName();
        CFGToDotGraph drawer = new CFGToDotGraph();
        DotGraph render = drawer.drawCFG(g,b);
        System.out.println("Considering: " + methodName);
        render.plot(outDir + methodName + ".dot");
    }

    public static void main(String[] args) {
        DrawCFG inst = DrawCFG.getInstance();
        PackManager.v().getPack("jtp").add(new Transform("jtp.viscfg", inst));
        System.out.println("added new phase thingy.");
        Main.main(args);
        System.out.println("exiting");
    }

    @Override
    protected void internalTransform(Body b, String phaseName,
                                     Map<String, String> options) {
        // TODO Auto-generated method stub
        System.out.println("running analysis.");
        DirectedGraph g = DrawCFG.getGraph(DrawCFG.UNIT, b);
        DrawCFG.render("/var/tmp/", g, b);
    }
}
