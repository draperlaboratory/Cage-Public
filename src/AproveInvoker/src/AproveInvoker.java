import soot.G;
import soot.Main;
import soot.PackManager;
import soot.Transform;
import soot.options.Options;


public class AproveInvoker
{
  public static void main(String[] args) {
    G.v().out.println("Aprove Invoker.");

    //---- Create and add the aprove invocation transform to the wjtp pack ------
    RunAproveTransformer RAT = new RunAproveTransformer();
    Transform transformer = new Transform("wjtp.aprove_invoker", RAT);

    // setup args
    String optsString = "enabled summaries-file entry-method";
    for (String flag : RATOpts.flags){
      optsString += " " + flag;
    }

    // make sure the processor knows what the arguments are
    transformer.setDeclaredOptions(optsString);

    //Transform iflowT = new Transform("wjtp.iflow", new IflowTransformer());
    PackManager.v().getPack("wjtp").add(transformer);
    //----- Set various other options for the Iflow Analysis -----------------
    Options.v().set_whole_program(true);
    // For now, we don't emit any code
    Options.v().set_output_format(Options.output_format_none);
    //Call Soot's main function
    Main.main(args);
    // after everything is done, dump the table
    RAT.getResults().display();
  }
}
