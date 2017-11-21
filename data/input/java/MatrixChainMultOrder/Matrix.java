public class Matrix{
    protected int[][] contents;

    public Matrix(int i, int j){
        this.contents = new int[i][j];
    }

    public int height(){
        if(this.contents != null)
            return this.contents[0].length;
        else
            return 0;
    }

    public int width(){
        if(this.contents != null)
            return this.contents.length;
        else
            return 0;
    }

    public static int[] getP(Matrix[] m){
        if (m == null) return null;
        int[] ret = new int[m.length * 2];
        int d = 0;
        for(int i = 0; i < m.length; i++){
            ret[d++] = m[i].height();
            ret[d++] = m[i].width();
        }
        return ret;
    }
}
