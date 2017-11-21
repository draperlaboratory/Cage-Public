class Find {


    public static void main(String[] args){
        Finder f = new Finder("hello");
        System.out.println(f.find(args));
    }

}


        
class Finder {

    String x;

    public Finder(String key){
        x = key;
    }

    public boolean find(String[] xs){
        for (int i=0; i < xs.length; i++){
            if (x.equals(xs[i]))
                return true;
        }
        return false;
    }
}
