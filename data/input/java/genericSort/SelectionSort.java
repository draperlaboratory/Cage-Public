public class SelectionSort extends SortAlg{
    public static void sort(Sortable s){
        int sz = s.size();
        for(int i = 0; i < sz; i++){
            int j = s.minIndexBeyond(i);
            s = s.swap(i,j);
        }
    }
}
