public class Solution2 {

    private String getString(String string) {
        int j = 0; // for first char.
        int k = 0;
        char firstChar = string.charAt(0);
        char secondChar = 0;
        String maxResult = "";
        for (int i = 1; i < string.length(); i++) {
            if (firstChar == string.charAt(i)) {
                continue;
            } else {
                if (secondChar == 0) {
                    secondChar = string.charAt(i);
                    k = i;
                }
            }
            if (string.charAt(i) != secondChar && string.charAt(i) != firstChar) {
                String result = string.substring(j, i);
                if (result.length() > maxResult.length()) {
                    maxResult = result;
                }
                j = k;
                firstChar = secondChar;
                k = i;
                secondChar = string.charAt(i);
            }
        }
        return maxResult;
    }

    private String getString2(String string) {
        int j = 0;
        int k = 0;
        char firstChar = string.charAt(0);
        char secondChar = 0;
        String maxString = "";
        for (int i = 0; i < string.length(); i++) {
            char iChar = string.charAt(i);
            if (iChar == firstChar) {
                continue;
            } else {
                if (secondChar == 0) {
                    secondChar = iChar;
                    k = i;
                    continue;
                }
            }
            if (iChar != firstChar && iChar != secondChar) {
                String result = string.substring(j, i);
                if (result.length() > maxString.length()) {
                    maxString = result;
                }
                j = k;
                k = i;
                firstChar = secondChar;
                secondChar = iChar;
            }
        }
        return maxString;
    }

    public static void main(String[] args) {
        // "AAABBDDAAADEE -> DDAAAAD ABBBAEAEAEAEEAG -> AEAEAEAEE A->A DA->DA DAA->DAA DAAB->DAA or DAAB->AAB "
        String result = new Solution2().getString("AAABBBEDDAAAADEE");
        System.out.println(result);
        String result2 = new Solution2().getString2("AAABBBEDDAAAADEE");
        System.out.println(result2);
    }
}

