import java.lang.Math;

public class Eratosthenes{

    public static boolean eratosthenes(int n){
        if(n == 0 || n == 1) return true;
        boolean[] prime = new boolean[n];
        // set everything to be prime
        for(int i = 0; i < prime.length; i++){
            prime[i] = true;
        }
        // now determine what actually is.
        int integer = 0;
        for(int index = 0; index < prime.length; index++){
            integer = index + 2;
            if(prime[index]){
                //System.out.println(integer + " is prime.");
                for(int update = integer * integer;
                    (update - 2) < n;
                    update += integer){
                    prime[update-2] = false;
                }
            }
        }
        // and then is the thing we care about prime
        return prime[n-2];
    }

    public static void main(String[] argv){
        boolean ans = Eratosthenes.eratosthenes(argv.length);
        //System.out.println("Primality of " + argv.length);
        //System.out.println(ans);
    }
}
