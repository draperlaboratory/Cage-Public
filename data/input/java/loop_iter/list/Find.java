class Find {


    public static void main(String[] args){
        List<String> my_list = toList(args);
        
        System.out.println(my_list.find("hello"));
    }

    public static List<String> toList(String[] ss){
        List<String> accum = new Nil<String>();

        for (int i = ss.length - 1; i >= 0; i--)
            accum = new Cons<String>(ss[i], accum);

        return accum;
    }


}
