package basic;

import java.util.ArrayList;
import java.util.List;

public class Permutation {

    private List<String> results = new ArrayList<>();

    private List<String> permutation(String string) {
        this.helper(new StringBuffer(), string);
        return this.results;
    }

    private void helper(StringBuffer sb, String string)  {
        if (sb.length() == string.length()) {
            this.results.add(sb.toString());
            return;
        }
        for (int i = 0; i < string.length(); i++) {
            if (sb.toString().indexOf(string.charAt(i)) != -1) {
                continue;
            }
            sb.append(string.charAt(i));
            this.helper(sb, string);
            sb.deleteCharAt(sb.length() - 1);
        }
    }

    private List<String> permutation2(String string) {
        this.helper2(new StringBuffer(), string, 0);
        return this.results;
    }

    private void helper2(StringBuffer sb, String string, int index)  {
        this.results.add(sb.toString());
        if (sb.length() == string.length()) {
            return;
        }
        for (int i = index; i < string.length(); i++) {
            if (sb.toString().indexOf(string.charAt(i)) != -1) {
                continue;
            }
            sb.append(string.charAt(i));
            this.helper2(sb, string, i);
            sb.deleteCharAt(sb.length() - 1);
        }
    }

    public static void main(String[] args) {
        System.out.println(new Permutation().permutation("ABC"));
        System.out.println(new Permutation().permutation2("ABC"));
    }
}
