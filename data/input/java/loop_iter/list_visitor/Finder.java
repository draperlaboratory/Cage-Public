class Finder extends Visitor<String, Boolean> {


    String elt;

    Finder(String s){
        elt = s;
    }

    public Boolean visitNil() { return Boolean.FALSE; }

    public Boolean visitCons(String s, List<String, Boolean> l) {
        if (elt.equals(s))
            return Boolean.TRUE;
        else
            return this.visit(l);
    }

}
