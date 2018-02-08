import java.lang.System.*;

public class test {

    public static void main(String[] args){
          float a = 3.0;
          int b = 1;
          int sum = add(a, b);
          System.out.println(sum);
          int diff = subtract(a, b);
          System.out.println(diff);
    }



    public static int add(int x, int y) {
    int c = x + y;
    return c;
}


public static int subtract(int x, int y) {
        int c = x - y;
        return c;
}




}