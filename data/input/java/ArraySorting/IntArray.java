public class IntArray{

    private int[] contents;

    public IntArray(int size){
        this.contents = new int[size];
        for(int i = 0; i < size; i++){
            this.contents[i] = size - i;
        }
    }

    public void display(){
        System.out.print("[");
        for(int i = 0; i < this.contents.length; i++){
            System.out.print(" " + this.contents[i]);
        }
        System.out.println(" ]");
    }

    public int minElement(){
        int min = this.contents[0];
        for(int i = 1; i < this.contents.length; i++){
            int el = this.contents[i];
            if (el < min){
                min = el;
            }
        }
        return min;
    }

    private int minElementBeyond(int start){
        int min = this.contents[start];
        int retInd = start;
        for(int i = start; i < this.contents.length; i++){
            int el = this.contents[i];
            if (el < min){
                min = el;
                retInd = i;
            }
        }
        return retInd;
    }

    public int maxElement(){
        int max = this.contents[0];
        for(int i = 1; i < this.contents.length; i++){
            int el = this.contents[i];
            if (el > max){
                max = el;
            }
        }
        return max;
    }

    private void swap(int i, int j){
        int swap;
        swap = this.contents[i];
        this.contents[i] = this.contents[j];
        this.contents[j] = swap;
    }

    public void bucketSort(){
        int minVal = this.minElement();
        int maxVal = this.maxElement();
        int range = maxVal - minVal + 1;
        int[] buckets = new int[range];
        // set up the buckets
        for(int i = 0; i < range; i++){
            buckets[i] = 0;
        }

        // figure out what the counts on the buckets are.
        for(int i = 0; i < this.contents.length; i++){
            buckets[this.contents[i] - minVal]++;
        }

        int cIndex = 0;

        // reconstruct the list from the buckets
        for(int i = 0; i < range; i++){
            int toAdd = buckets[i];
            while(toAdd > 0){
                this.contents[cIndex++] = i + minVal;
                toAdd--;
            }
        }
    }

    public void whileBubbleSort(){
        int j = 0;
	int inplace;
        boolean swapped = true;
        while(swapped){
            swapped = false;
	    for(int i = 0; i < contents.length - 1; i++){
		j = i + 1;
                if(contents[i] > contents[j]){
                    this.swap(i,j);
                    swapped = true;
                }
	    }
        }
    }

    public void forBubbleSort(){
        int j = 0;
	int inplace;
        boolean swapped = true;
        for(int lastInd = contents.length - 1; lastInd > 0; lastInd --){
            swapped = false;
	    for(int i = 0; i < lastInd; i++){
		j = i + 1;
                if(contents[i] > contents[j]){
                    this.swap(i,j);
                    swapped = true;
                }
	    }
	    if (!swapped){
		return;
	    }
        }
    }

    public void bubbleSort(){
	this.forBubbleSort();
    }

    private boolean combSortAux(int gap){
        int j;
        boolean swapped = false;
        for(int i = 0; i < contents.length - gap; i++){
            j = i + gap;
            if(contents[i] > contents[j]){
                this.swap(i,j);
                swapped = true;
            }
        }
        return swapped;
    }

    public void combSort(double fact){
        int gap = (int)((this.contents.length - 1) * fact);
        boolean swapped = true;
        while(gap != 1 || swapped){
            gap /= fact;
            if(gap < 1){
                gap = 1;
            }
            swapped = combSortAux(gap);
        }
    }

    public void shellSort(int[] schedule){
        for(int i = 0; i < schedule.length; i++){
            while(this.combSortAux(schedule[i]));
        }
    }

    public void insertionSort(){
        for(int i = 0; i < this.contents.length; i++){
            int j = i;
            while(j > 0 && this.contents[j-1] > this.contents[j]){
                this.swap(j,j-1);
                j--;
            }
        }
    }

    public void selectionSort(){
        for(int i = 0; i < this.contents.length; i++){
            int j = this.minElementBeyond(i);
            this.swap(i,j);
        }
    }

    public static void main(String[] argv){
        IntArray iar1 = new IntArray(20);
        IntArray iar2 = new IntArray(20);
        IntArray iar3 = new IntArray(20);
        IntArray iar4 = new IntArray(20);
        IntArray iar5 = new IntArray(20);
        IntArray iar6 = new IntArray(20);

        iar1.display();
        iar1.bubbleSort();
        iar1.display();

        iar2.display();
        iar2.bucketSort();
        iar2.display();

        iar3.display();
        iar3.insertionSort();
        iar3.display();

        iar4.display();
        iar4.selectionSort();
        iar4.display();

        iar5.display();
        iar5.combSort(2);
        iar5.display();

        int[] schedule = new int[] {701, 301, 132, 57, 23, 10, 4, 1};
        iar6.display();
        iar6.shellSort(schedule);
        iar6.display();
    }
}
