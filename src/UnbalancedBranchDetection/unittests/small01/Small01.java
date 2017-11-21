import java.util.*;

public class Small01
{
  public static String foo(int num) {
    if(num % 2 == 0)
      return "even";
    else
      return "odd";
  }

  public static String bar(int num) {
    if(num % 2 == 0)
      return "even".toUpperCase();
    else
      return "ODD";
  }


  public static String baz(int num) {
    String returnVal;
    if(num % 2 == 0)
      returnVal = "even".toUpperCase();
    else
      returnVal = "ODD";
    return returnVal;
  }

  public static List<String> toStringList1(String digits) {
    int numDigits = digits.length();
    LinkedList<String> strs = new LinkedList<>();
    for(int i = 0; i < numDigits; i++) {
      int digit = Character.getNumericValue(digits.charAt(i));
      strs.add(foo(digit));
    }
    return strs;
  }

  public static List<String> toStringList2(String digits) {
    LinkedList<String> strs = new LinkedList<>();
    for(int i = 0; i < digits.length(); i++) {
      int digit = Character.getNumericValue(digits.charAt(i));
      strs.add(foo(digit));
    }
    return strs;
  }
  
  public static List<String> toStringList3(String digits) {
    int numDigits = digits.length();
    LinkedList<String> strs = new LinkedList<>();
    int i = 0;
    while(i < numDigits) {
      int digit = Character.getNumericValue(digits.charAt(i++));
      strs.add(foo(digit));
    }
    return strs;
  }

  public static void bothExpensiveBlocks(int num) {
    int ctr = 0;
    if(num % 2 == 0) {
      int half = num / 2;
      while(ctr < half) {
        System.out.println("ctr: " + ctr);
        ctr++;
      }
    }
    else {
      int moreThanHalf = (num+1) / 2;
      while(ctr < moreThanHalf) {
        System.out.println("ctr: " + ctr);
        ctr++;
      }
    }
    System.out.println("range of this statement is large (small cost when num == 0)");
  }

  public static void main(String[] args)
  {
    if(args.length == 0) {
      System.err.println("usage: Small01 <num>");
      System.exit(0);
    }
    int num = Integer.parseInt(args[0]);
    System.out.println(foo(num));
  }
}
