public class ShellSort extends CombSort{

    public static void sort(int[] schedule, Sortable s){
        for(int i = 0; i < schedule.length; i++){
            swapped = true;
            while(swapped){
                s = ShellSort.combSortAux(schedule[i], s);
            }
        }
    }

    public static void sort(Sortable s){
        int[] schedule = new int[] {701, 301, 132, 57, 23, 10, 4, 1};
        ShellSort.sort(schedule,s);
    }
}
