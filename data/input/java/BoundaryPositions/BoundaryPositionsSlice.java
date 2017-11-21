public class BoundaryPositionsSlice {

    static byte[] r3;
    static int[] r4, r7;

    // Direct translation from grimpl
    public static int[] getBoundaryPositions(java.nio.ByteBuffer buf, byte[] ba) {
        int i29, i28;
        r7 = new int[0];

        if (buf.remaining() < ba.length) {
            return r7;
        }
        r3 = new byte[4096 + ba.length];

        if (buf.remaining() < r3.length){
            i28 = buf.remaining();
        } else {
            i28 = r3.length;
        }

        buf.get(r3, 0, i28);
        i29 = i28 - ba.length;

        //label04
        loop04(i29, ba, buf);
        return r7;

    }

    public static void loop04(int i29, byte[] ba, java.nio.ByteBuffer buf){
        //label04
        int i27 = 0;
        int i33 = 0;
        int i32 = 0;
        while(i29 > 0){
            //label05
            loop05(i29, i27, ba);
            i27 = i27 + i29;
            java.lang.System.arraycopy(r3, r3.length - ba.length, r3, 0, ba.length);
            i32 = r3.length - ba.length;
            if (buf.remaining() < i32) {
                i33 = buf.remaining();
            } else {
                i33 = i32;
            }
            i29 = i33;
            buf.get(r3, ba.length, i29);
        }
    }

    public static void loop05(int i29, int i27, byte[] ba){
        int i30 = 0;
        while (i30 < i29) {
            //label06
            loop06(i30, i27, ba);
            i30++;
        }
    }

    public static void loop06(int i30, int i27, byte[] ba){
        for(int i31 = 0; i31 < ba.length; i31++){
            if (r3[i30 + i31] == ba[i31]) {
                return;
            }
            // i31 modified in loop
            // i30, r2, r30 is fixed
            if (i31 == (ba.length - 1)){
                r4 = new int[r7.length + 1];
                java.lang.System.arraycopy(r7, 0, r4, 0, r7.length);
                r4[r7.length] = i27 + i30;
                r7 = r4;
            }
        }
    }

    public static void main(String[] args) {
        // Auto-generated method stub

    }

}
