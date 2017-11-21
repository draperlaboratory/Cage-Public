import java.lang.Math;


public class PrimeLeak{
    private static int[] knownPrimes = new int[]{2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997};

    private static final int maxFixed = 997;
    private static final int useTrial = 100000;

    public int findFixed(int i){
        for(int j = 0; j < knownPrimes.length; j++){
            if(knownPrimes[j] > i){
                return knownPrimes[j];
            }
        }
        return -1;
    }

    public boolean isPrimeTrial(int i){
        double max = Math.sqrt(i);

        for(int j = 0; j < knownPrimes.length; j++){
            if(knownPrimes[j] <= max && i % knownPrimes[j] == 0){
                return false;
            }
        }
        for(int j = maxFixed + 1; j < max; j++){
            if(i % j == 0)
                return false;
        }
        return true;
    }

    public int findPrimesTrial(int i){
        i++;
        while(!isPrimeTrial(i)){
            i++;
        }
        return i;
    }

    public int sieve(int n){
        if(n == 0 || n == 1) return n;
        int growth = 2;
        boolean[] prime = new boolean[n * growth];
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
        for(int i = n - 1; i < prime.length; i++){
            if(prime[i])
                return i + 2;
        }
        return sieve(n * growth);
    }

    public int findPrime(int i){
        if(i < maxFixed){
            return this.findFixed(i);
        }else{
            return this.findPrimesTrial(i);
        }
    }

    public static void main(String[] argv){
        int findLarger = Integer.parseInt(argv[0]);
        PrimeLeak me = new PrimeLeak();
        int ans = me.findPrime(findLarger);
        System.out.println("next prime after " + findLarger + " is " + ans);
    }
}
