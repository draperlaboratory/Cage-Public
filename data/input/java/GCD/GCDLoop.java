public class GCDLoop {

    public int mod(int a, int b){
        int div = a / b;
        return a - b * div;
    }

    public int general(int a, int b){
        int swap;
        while(b != 0){
            swap = a;
            a = b;
            b = this.mod(swap, b);
        }
        return a;
    }

    public int positive(int a, int b){
        if(a < 0 || b < 0){
            return this.general(a,b);
        }else{
            while(true){
                if(a == b){
                    return a;
                }else if (b == 0){
                    return a;
                }else if (a > b){
                    a = a - b;
                }else { //(b > a)
                    b = b - a;
                }
            }
        }
    }

    public static void main(String[] argv){
        int a;
        int b;
        GCDLoop me = new GCDLoop();
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
