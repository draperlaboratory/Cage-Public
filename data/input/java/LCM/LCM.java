public class LCM{
    public static int compute(int a, int b){
        int div = StaticGCD.positive(a,b);
        int num = a * b;
        if (num < 0) num *= -1;
        return num / div;
    }

    public static void main(String[] argv){
        int a;
        int b;
        if(argv.length < 2){
            //System.out.println("please input exactly two numbers as args");
        }else{
            a = argv[0].length();
            b = argv[1].length();
            //System.out.println("a: " + a + "\tb:" + b);
            int result1 = LCM.compute(a,b);
            int result2 = LCM.compute(b,a);
            //System.out.println(result1 + " == " + result2);
            if (result1 == result2){
                //System.out.println("ok");
                System.exit(0);
            } else {
                //System.out.println("bug");
                System.exit(1);
            }
        }
    }
}
