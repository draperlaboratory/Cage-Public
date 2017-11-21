public class BubbleSort extends SortAlg{

    public static void whileSort(Sortable s){
        int j;
        boolean swapped = true;
        int sz = s.size();
        while(swapped){
            swapped = false;
            for(int i = 0; i < sz - 1; i++){
                j = i + 1;
                if(!s.sorted(i,j)){
                    s = s.swap(i,j);
                    swapped = true;
                }
            }
        }
    }

    public static void forSort(Sortable s){
	int j;
        boolean swapped = true;
        int sz = s.size();
	    for(int lastInd = sz - 1; lastInd > 0; lastInd --){
            swapped = false;
            for(int i = 0; i < lastInd; i++){
                j = i + 1;
                if(!s.sorted(i,j)){
                    s = s.swap(i,j);
                    swapped = true;
                }
            }
        }
    }

    public static void sort(Sortable s){
	forSort(s);
    }
}
