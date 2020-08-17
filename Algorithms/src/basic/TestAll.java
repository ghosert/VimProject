package basic;

import java.util.*;

class Reverse {
    public String reverse(String string) {
        int i = 0;
        int j = string.length() - 1;
        char[] chars = string.toCharArray();
        while (i < j) {
            char temp = chars[i];
            chars[i] = chars[j];
            chars[j] = temp;
            i++;
            j--;
        }
        return new String(chars);
    }
}

public class TestAll {
    public static void main(String[] args) {
        System.out.println(new Reverse().reverse("abcdefghijkl"));
        Queue<Integer> queue = new LinkedList<>();
        queue.offer(1);
        System.out.println("queue is empty and value: " + queue.isEmpty() + " " + queue.poll());
        Stack<Integer> stack = new Stack<>();
        stack.push(1);
        System.out.println("stack is empty and value: " + stack.isEmpty() + " " + stack.pop());
        char c = '3';
        int i = (int) c;
        System.out.println(i);
        Set<Character[]> set = new HashSet<>();
        String string = new String(new char[] {1, 0});
        System.out.println(string);
        String s = 1 + "" + 2;
        System.out.println(s);
        set.add(new Character[] {'1', '0'});
        System.out.println(set.size());
        String hello = "0000";
        char[] chars = hello.toCharArray();
        chars[0] = (char) (chars[0] - '0' + 1 + '0');
        System.out.println("==============");
        System.out.println(hello);
        System.out.println(chars);
        List<Integer> list = new ArrayList<>();
        list.add(1);
        list.add(2);
        list.remove(1);
        System.out.println("" + 's');
        int k = 2;
        int[] ss = new int[] {1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4};
        Map<Integer, Integer> map = new HashMap<>();
        for (int ii = 0; ii < ss.length; ii++) {
            map.put(ss[ii], map.getOrDefault(ss[ii], 0) + 1);
        }
        PriorityQueue<Integer> pq = new PriorityQueue<>((num1, num2) -> map.get(num2) - map.get(num1));
        for (Integer ii : map.keySet()) {
            pq.add(ii);
        }
        int[] result = new int[k];
        int j = k;
        while (k-- > 0) {
            System.out.println(pq.poll());
        }

        System.out.println("========================");
        FirstUnique solution = new FirstUnique(new int[] {2, 3, 5});
        System.out.println(solution.showFirstUnique());
        solution.add(5);
        System.out.println(solution.showFirstUnique());
        solution.add(2);
        System.out.println(solution.showFirstUnique());
        solution.add(3);
        System.out.println(solution.showFirstUnique());
        System.out.println("========================");

        new Solution().longestCommonPrefix(new String[] {"aflower", "bflow", "cflight"});
        List<StringBuffer> results = new ArrayList<>();

        Map<String, String> maps = new HashMap<>();
        for (int z = 1; z <= 26; z++) {
            maps.put(z + "", new String(new char[] {(char) ('a' + z - 1)}));
        }
        List<String> ssss = new TestAll().getStrings("1212", 0, maps);
        for (String sst : ssss) {
            System.out.println(sst);
        }
    }

    private List<String> getStrings(String string, int startIndex, Map<String, String> maps) {
        if (startIndex == string.length()) return new ArrayList<>();
        String s = maps.get(string.substring(startIndex, startIndex + 1));
        List<String> results = new ArrayList<>();
        List<String> rr = this.getStrings(string, startIndex + 1, maps);
        if (rr.size() == 0) {
            results.add(s);
        }
        for (String ss : rr) {
            results.add(s + ss);
        }
        if (startIndex <= string.length() - 2) {
            int value = Integer.valueOf(string.substring(startIndex, startIndex + 2));
            s = maps.get("" + value);
            if (s != null) {
                rr = this.getStrings(string, startIndex + 2, maps);
                if (rr.size() == 0) {
                    results.add(s);
                }
                for (String ss : rr) {
                    results.add(s + ss);
                }
            }
        }
        return results;
    }
}

class Solution {
    public String longestCommonPrefix(String[] strs) {
        if (strs.length == 0) return "";
        String prefix = strs[0];
        for (int i = 1; i < strs.length; i++)
            while (strs[i].indexOf(prefix) != 0) {
                prefix = prefix.substring(0, prefix.length() - 1);
                if (prefix.isEmpty()) return "";
            }
        return prefix;
    }
}

class Entry {
    String key;
    String value;
    Entry next;
    Entry(String key, String value, Entry next) {
        this.key = key;
        this.value = value;
        this.next = next;
    }
}

class FirstUnique {

    private Map<Integer, Integer> map = new LinkedHashMap<>();
    private Queue<Integer> queue = new LinkedList<>();

    public FirstUnique(int[] nums) {
        for (int n : nums) {
            map.put(n, map.getOrDefault(n, 0) + 1);
        }
        for (int key : map.keySet()) {
            if (map.get(key) == 1) {
                queue.add(key);
            }
        }
    }

    public int showFirstUnique() {
        return 1;
    }

    public void add(int value) {
        map.put(value, map.getOrDefault(value, 0) + 1);
        if (map.get(value) == 1) {
            queue.add(value);
        } else {
            queue.remove(value);
        }
    }
}



class GenericsWildcards {

    public static void main(String[] args) {
        List<Integer> ints = new ArrayList<>();
        ints.add(3); ints.add(5); ints.add(10);
        double sum = sum(ints);
        System.out.println("Sum of ints="+sum);
    }

    public static double sum(List<? extends Number> list){
        double sum = 0;
        for(Number n : list){
            sum += n.doubleValue();
        }
        // You are not allowed to add new element to the upper bounded list except null:
        // not working: Integer i = 2; list.add(i);
        // working: list.add(null);
        return sum;
    }
}
