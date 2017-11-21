import java.math.BigInteger;

public class FastExpBigInt {
    private static BigInteger secret = new BigInteger("123456789101112131415161718192021222324252627282930313233343536373839404142");

    public static void main(String[] args){
        BigInteger b = new BigInteger(args[0]);
        BigInteger m = new BigInteger(args[1]);
        System.out.println(secret.modPow(b, m));
    }
}
