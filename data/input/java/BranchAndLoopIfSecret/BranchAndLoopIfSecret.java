public class BranchAndLoopIfSecret {

    public static boolean getSecret() {
        return true;
    }

    public static void main(String[] args) {
        if (getSecret()) {
            while(true) {
            }
        } else {
        }
    }
}
