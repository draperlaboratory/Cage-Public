public class InsertionSort extends SortAlg{

    public static void sort(Sortable s){
        int sz = s.size();
        for(int i = 0; i < sz; i++){
            int j = i;
            while(j > 0 && !s.sorted(j-1,j)){
                s = s.swap(j,j-1);
                j--;
            }
        }
    }
}
