public class ShellSort{
    public static void main(String[] argv){
        int[] schedule = new int[] {701, 301, 132, 57, 23, 10, 4, 1};
        IntArray iar = new IntArray(argv.length);
        iar.display();
        iar.shellSort(schedule);
        iar.display();
    }
}
