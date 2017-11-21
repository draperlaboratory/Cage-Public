public class Triple{
    private int a;
    private int b;
    private int c;

    public Triple(int f, int g, int h){
        a = f;
        b = g;
        c = h;
    }

    public void display(){
        int as = this.a * this.a;
        int bs = this.b * this.b;
        int cs = this.c * this.c;

        System.out.println("a: " + this.a +
                           "\tb: " + this.b +
                           "\tc: " + this.c);

        System.out.println(as + " + " + bs + " == " + cs);
    }

    public boolean validate(){
        return a*a + b*b == c*c;
    }
}
