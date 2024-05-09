package basic;

import java.io.Closeable;
import java.io.IOException;
import java.util.*;

public class TestMe implements Cloneable {
    public static void main(String[] args) throws CloneNotSupportedException {
        PriorityQueue<String> pq = new PriorityQueue<>((x, y) -> y.length() - x.length());
        String string = "aaaaaabbbbccc";
        int j = 0;
        int i = 0;
        for (i = 1; i < string.length();) {
            if (string.charAt(i) == string.charAt(j)) {
                i++;
            } else {
                pq.add(string.substring(j, i));
                j = i;
                i++;
            }
        }
        pq.add(string.substring(j, i));
        int k = 3;
        String result = null;
        while (!pq.isEmpty() && k > 0) {
            result = pq.poll();
            k--;
        }
        System.out.println(result);

        StringBuilder sb = new StringBuilder();
        System.out.println(sb.toString().equals(""));
        new TestMe().clone();
    }
}
