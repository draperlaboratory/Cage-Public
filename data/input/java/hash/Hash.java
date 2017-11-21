

public class Hash {

    public static void main(String[] args) {

	HashTbl H = new HashTbl();

        if(args.length == 0){
            System.out.println("O(n^2)");
        }else{
            int l = args[0].length();

            // for(int i = 0; i < (1 << l); i++)
            //O(n^2)
            for(int i = 0; i < l; i++)
                //each of these is O(n)
                H.insert(i);
        }
    }
}
