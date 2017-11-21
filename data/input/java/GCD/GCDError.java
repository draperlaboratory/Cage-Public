public class GCDError {

    public int mod(int a, int b){
        int div = a / b;
        return a - b * div;
    }

    public int general(int a, int b){
        if(b == 0){
            return a;
        }else{
            return this.general(b, this.mod(a, b));
        }
    }

    public int positive(int a, int b) throws NegativeInts{
        if(a < 0 || b < 0){
            throw new NegativeInts();
        }else{
            if(a == b){
                return a;
            }else if (b == 0){
                return a;
            }else if (a > b){
                return this.positive(a - b, b);
            }else { //(b > a)
                return this.positive(a, b - a);
            }
        }
    }

    public static void main(String[] argv) throws NegativeInts{
        int a;
        int b;
        GCDError me = new GCDError();
        if(argv.length < 2){
            //System.out.println("please input exactly two numbers as args");
        }else{
            a = argv[0].length();
            b = argv[1].length();
            //System.out.println("a: " + a + "\tb:" + b);
            int result1 = me.general(a,b);
            int result2 = me.positive(a,b);
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
