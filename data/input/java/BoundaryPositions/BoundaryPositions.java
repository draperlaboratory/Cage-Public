
public class BoundaryPositions {

    // Original grimpl code from fi.iki.elonen.NanoHTTPD$HTTPSession

    // private int[] getBoundaryPositions(java.nio.ByteBuffer, byte[])
    // {
    //     fi.iki.elonen.NanoHTTPD$HTTPSession r0;
    //     java.nio.ByteBuffer r1;
    //     byte[] r2, r3;
    //     int i0, i27, $i28, i29, i30, i31, i32, $i33;
    //     int[] r4, r7;

    //     r0 := @this;

    //     r1 := @parameter0;

    //     r2 := @parameter1;

    //     r7 = newarray (int)[0];

    //     if r1.remaining() >= lengthof r2 goto label01;

    //     return r7;

    //  label01:
    //     i27 = 0;

    //     r3 = newarray (byte)[4096 + lengthof r2];

    //     if r1.remaining() >= lengthof r3 goto label02;

    //     $i28 = r1.remaining();

    //     goto label03;

    //  label02:
    //     $i28 = lengthof r3;

    //  label03:
    //     i0 = $i28;

    //     r1.get(r3, 0, i0);

    //     i29 = i0 - lengthof r2;

    //  label04:
    //     i30 = 0;

    //  label05:
    //     if i30 >= i29 goto label10;

    //     i31 = 0;

    //  label06:
    //     if i31 >= lengthof r2 goto label09;

    //     if r3[i30 + i31] == r2[i31] goto label07;

    //     goto label09;

    //  label07:
    //     if i31 != lengthof r2 - 1 goto label08;

    //     r4 = newarray (int)[lengthof r7 + 1];

    //     java.lang.System.arraycopy(r7, 0, r4, 0, lengthof r7);

    //     r4[lengthof r7] = i27 + i30;

    //     r7 = r4;

    //  label08:
    //     i31 = i31 + 1;

    //     goto label06;

    //  label09:
    //     i30 = i30 + 1;

    //     goto label05;

    //  label10:
    //     i27 = i27 + i29;

    //     java.lang.System.arraycopy(r3, lengthof r3 - lengthof r2, r3, 0, lengthof r2);

    //     i32 = lengthof r3 - lengthof r2;

    //     if r1.remaining() >= i32 goto label11;

    //     $i33 = r1.remaining();

    //     goto label12;

    //  label11:
    //     $i33 = i32;

    //  label12:
    //     i29 = $i33;

    //     r1.get(r3, lengthof r2, i29);

    //     if i29 > 0 goto label04;

    //     return r7;
    // }
	

    // Direct translation from grimpl
    // Descriptor: (Ljava/nio/ByteBuffer;[B)[I
	public static int[] getBoundaryPositions(java.nio.ByteBuffer buf, byte[] ba) {
		
		java.nio.ByteBuffer r1;
		byte[] r2, r3;
		int i0, i27, i28, i29, i30, i31, i32, i33;
		int[] r4, r7; 
		
		r1 = buf;
		r2 = ba;
		r7 = new int[0];
		
		if (r1.remaining() < r2.length) {
			return r7;
		}
		i27 = 0;
		r3 = new byte[4096 + r2.length];
		
		if (r1.remaining() < r3.length){
			i28 = r1.remaining();
		} else {
			i28 = r3.length;
		}

		i0 = i28;
		r1.get(r3, 0, i0);
		i29 = i0 - r2.length;
		
		//label04
		while(true){
			i30 = 0;
		
			//label05
			while (i30 < i29) {
				i31 = 0;
				//label06
				while (i31 < r2.length) {
					if (r3[i30 + i31] == r2[i31]) {
						if (i31 == (r2.length - 1)){
							r4 = new int[r7.length + 1];
							java.lang.System.arraycopy(r7, 0, r4, 0, r7.length);
							r4[r7.length] = i27 + i30;
							r7 = r4;
						}
						i31++;
					} else { break; }
				}
				i30++;
			}
			i27 = i27 + i29;
			java.lang.System.arraycopy(r3, r3.length - r2.length, r3, 0, r2.length);
			i32 = r3.length - r2.length;
			if (r1.remaining() < i32) {
				i33 = r1.remaining();
			} else {
				i33 = i32;
			}
			i29 = i33;
			r1.get(r3, r2.length, i29);
			if (i29 <= 0) { break; }
		}
		return r7;
		
	}
	
	public static void main(String[] args) {
		// Auto-generated method stub

	}

}
