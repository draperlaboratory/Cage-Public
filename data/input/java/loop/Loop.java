class Loop {

    public static void main (String[] args) {
        if(args.length == 0){
            System.out.println("non-terminating");
        }else{
            Node circ;
            circ = new Node(0, new Nil());
            circ.setTail(circ);
            circ.append(args.length);
        }
    }

}
