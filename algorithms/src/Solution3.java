public class Solution3 {

    public void flip(int[] nums, int k) {
        int i = 0;
        int j = k - 1;
        while (i < j) {
            int swap = nums[i];
            nums[i] = nums[j];
            nums[j] = swap;
        }
    }

    public void sort(int[] nums) {

    }

    public static void main(String[] args) {
    }
}
