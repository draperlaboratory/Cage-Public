public class CombSort extends SortAlg{

    protected static boolean swapped = false;

    protected static Sortable combSortAux(int gap, Sortable s){
        int j;
        swapped = false;
        int sz = s.size();
        for(int i = 0; i < sz - gap; i++){
            j = i + gap;
            if(!s.sorted(i,j)){
                s = s.swap(i,j);
                swapped = true;
            }
        }
        return s;
    }

    public static void sort(double fact, Sortable s){
        int gap = (int)((s.size() - 1) * fact);
        swapped = true;
        while(gap != 1 || swapped){
            gap /= fact;
            if(gap < 1){
                gap = 1;
            }
            s = combSortAux(gap,s);
        }
    }
}
