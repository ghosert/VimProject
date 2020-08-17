package basic;

import java.util.*;

public class TestMe {
    public static void main(String[] args) {
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
    }
}
