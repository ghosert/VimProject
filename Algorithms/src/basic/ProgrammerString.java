package basic;

import java.io.*;
import java.util.*;

class Result {

    /*
     * Complete the 'programmerStrings' function below.
     *
     * The function is expected to return an INTEGER.
     * The function accepts STRING s as parameter.
     */

    public static int programmerStrings(String s) {
        // Write your code here
        return getLastIndex(s) - getFirstIndex(s) + 1;
    }

    private static Map<Character, Integer> getProgrammerMap() {
        String programmer = "programmer";
        Map<Character, Integer> map = new HashMap<>();
        for (int i = 0; i < programmer.length(); i++) {
            map.put(programmer.charAt(i), map.getOrDefault(programmer.charAt(i), 0) + 1);
        }
        return map;
    }

    private static void checkMapValue(Map<Character, Integer> map, Character c) {
        Integer value = map.get(c);
        if (value != null)  {
            if (value == 1) {
                map.remove(c);
            } else {
                map.put(c, value - 1);
            }
        }

    }
    private static int getFirstIndex(String s) {
        Map<Character, Integer> map = getProgrammerMap();
        for (int i = 0; i < s.length(); i++) {
            checkMapValue(map, s.charAt(i));
            if (map.size() == 0) {
                return i + 1;
            }
        }
        return -1;
    }

    private static int getLastIndex(String s) {
        Map<Character, Integer> map = getProgrammerMap();
        for (int i = s.length() - 1; i >= 0; i--) {
            checkMapValue(map, s.charAt(i));
            if (map.size() == 0) {
                return i - 1;
            }
        }
        return -1;
    }

}

public class ProgrammerString {
    public static void main(String[] args) throws IOException {
        System.out.println(Result.programmerStrings("programmerxxxprozmerqgram"));
        System.out.println(Result.programmerStrings("progxrammerrxproxgrammer"));
        System.out.println(Result.programmerStrings("xprogxrmaxemrppprmmograeiruu"));
        System.out.println(Result.programmerStrings("programmerprogrammer"));
    }
}

