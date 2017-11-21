class Find {


    public static void main(String[] args){
        List<String, Boolean> my_list = toList(args);

        Finder f = new Finder("hello");
        
        System.out.println(f.visit(my_list));
    }

    public static List<String, Boolean> toList(String[] ss){
        List<String, Boolean> accum = new Nil<String, Boolean>();
        for (int i = ss.length - 1; i >= 0; i--)
            accum = new Cons<String, Boolean>(ss[i], accum);
        return accum;
    }


}
