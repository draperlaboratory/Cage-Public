public class Exp{

    private static void mainLoop(int base,int exp){
        int result = 1;
        int dummy = 0;
        while(exp > 0){
            if (exp %2 == 1) {
                result *= base;
            } // else {
            //     dummy *= base;
            // }
            base = base * base;
            exp /= 2;
        }
        System.out.println(result + dummy);
    }
    
    
    public static void main(String[] argv){

        int base = Integer.parseInt(argv[0]);
        int exp  = Integer.parseInt(argv[1]);

        if (exp < 0){
            System.out.println("Exponent must be non-negative");
        } else {
            mainLoop(base, exp);
        }
    }
}
                    
