public class StaticGCD {

    public static int mod(int a, int b){
        int div = a / b;
        return a - b * div;
    }

    public static int general(int a, int b){
        if(b == 0){
            return a;
        }else{
            return StaticGCD.general(b, StaticGCD.mod(a, b));
        }
    }

    public static int positive(int a, int b){
        if(a < 0 || b < 0){
            return StaticGCD.general(a,b);
        }else{
            if(a == b){
                return a;
            }else if (b == 0){
                return a;
            }else if (a > b){
                return StaticGCD.positive(a - b, b);
            }else { //(b > a)
                return StaticGCD.positive(a, b - a);
            }
        }
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
            int result1 = StaticGCD.general(a,b);
            int result2 = StaticGCD.positive(a,b);
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
