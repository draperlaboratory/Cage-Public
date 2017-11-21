public class TestAnnotations{

    public static void innerLoop(int outerIteration, int innerIterations){
        for(int i = 0; i < innerIterations; i++){
            //System.out.println(outerIteration + " " + i);
        }
    }

    public static void outerLoop(int outerIterations, int innerIterations){
        for(int i = 0; i < outerIterations; i++){
            innerLoop(i, innerIterations);
        }
    }

    public static void main(String[] argv){
        outerLoop(argv.length, argv[0].length());
    }

}
