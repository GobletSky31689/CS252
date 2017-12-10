package com.gobletsky.test;
import java.lang.System.*;

public class test {

public int add(int x, int y) {
    int c = x + y;
    subtract(x, y);
    return c;
}


public int subtract(int x, int y) {
        int c = x - y;
        add(x, c);
        return c;
}




}