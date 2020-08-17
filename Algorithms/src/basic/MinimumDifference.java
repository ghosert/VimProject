package basic;

import java.util.ArrayList;
import java.util.List;

class MinimumDifference {

    /**
     * An anagram is a word whose characters can be rearranged to create another word. Given two strings, determine the minimum number of characters in either string that must be modified to make the two strings anagrams .
     * If it is not possible to make the two strings anagrams, return -1.
     *
     * Example:
     * a = ['tea', 'tea', 'act']
     * b = ['ate', 'toe', 'acts']
     *
     * a[0] = tea and b[0] = ate are anagrams, so 0 characters need to be modified.
     * a[1] = tea and b[1] = toe are not anagrams.  Modify 1 character in either string (o → a or a → o) to make them anagrams.
     * a[2] = act and b[2] = acts are not anagrams and cannot be converted to anagrams because they contain different numbers of characters.
     *
     * The return array is [0, 1, -1]
     * @param a
     * @param b
     * @return
     */
    public static List<Integer> getMinimumDifference(List<String> a, List<String> b) {
        // Write your code here
        List<Integer> results = new ArrayList<>();
        for (int i = 0; i < a.size(); i++) {
            results.add(getDiff(a.get(i), b.get(i)));
        }
        return results;
    }

    private static int getDiff(String stringA, String stringB) {
        if (stringA.length() != stringB.length()) {
            return -1;
        }
        char[] charsA = stringA.toCharArray();
        char[] charsB = stringB.toCharArray();
        int[] table = new int[26];
        int count = 0;
        for (int i = 0; i < charsA.length; i++) {
            table[charsA[i] - 'a']++;
            table[charsB[i] - 'a']--;
        }
        for (int i = 0; i < table.length; i++) {
            if (table[i] != 0) {
                if (table[i] < 0) {
                    table[i] = -table[i];
                }
                count = count + table[i];
            }
        }
        return count / 2;
    }

    public static void main(String[] args) {
        List<String> a = new ArrayList<>();
        a.add("a");
        a.add("jk");
        a.add("abb");
        a.add("mn");
        a.add("abc");
        List<String> b = new ArrayList<>();
        b.add("bb");
        b.add("kj");
        b.add("bbc");
        b.add("op");
        b.add("def");
        List<Integer> list = MinimumDifference.getMinimumDifference(a, b);
        for (Integer i : list) {
            System.out.println(i);
        }
        /**
         * -1
         * 0
         * 1
         * 2
         * 3
         */
    }

}
