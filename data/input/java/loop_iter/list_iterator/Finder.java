class Finder {

    String elt;

    Finder(String s) {
        elt = s;
    }

    public boolean find(List<String> l) {
        Iterator<String> i = l.iterator();
        while(i.hasNext())
            if (elt.equals(i.next()))
                return true;
        return false;
    }
}
