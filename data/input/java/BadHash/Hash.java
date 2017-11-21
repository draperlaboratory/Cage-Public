public class Hash{

    public static int hash(String s){
        if(null == s){
            return -1;
        }else{
            char c = s.toCharArray()[0];
            int i = (int) c;
            if(i < 0)
                i *= -1;
            return i;
        }
    }
}
