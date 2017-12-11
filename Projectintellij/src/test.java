package com.gobletsky.test;
import java.lang.System.*;

public class test {

    public static void main(String[] args){
          int a = 3;
          int b = 1;
          int addition = add(a, b);
          System.out.println(addition);
    }



    public int add(int x, int y) {
    int c = x + y;
    return c;
}


public int subtract(int x, int y) {
        int c = x - y;
        return c;
}




}