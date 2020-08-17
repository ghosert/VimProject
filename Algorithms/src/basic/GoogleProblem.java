package basic;

public class GoogleProblem {

    private int[] sortingWithMoveTarget(int[] nums, int target) {
        for (int i = 0; i < nums.length; i++) {
            for (int j = nums.length - 1; j > i; j--) {
                if (nums[j] < nums[j - 1]) {
                    int swap = nums[j];
                    nums[j] = nums[j - 1];
                    nums[j - 1] = swap;
                }
            }
        }
        int i = 0;
        int j = nums.length - 1;
        while (i <= j) {
            int mid = i + (j - i) / 2;
            if (nums[mid] >= target) {
                j = mid - 1;
            } else {
                i = mid + 1;
            }
        }
        if (i == nums.length) return nums;
        int s = 0;
        int l = i;
        for (int k = 0; k < nums.length; k++) {
            if (nums[l++] == target) {
                s++;
            } else {
                break;
            }
        }
        int z = i - 1;
        for (int k = s + i - 1; z >= 0; k--) {
            nums[k] = nums[z--];
        }
        for (int k = 0; k < s; k++) {
            nums[k] = target;
        }
        return nums;
    }

    public static void main(String[] args) {
        int[] nums = new GoogleProblem().sortingWithMoveTarget(new int[] {3, 5, 2, 1, 5, 7, 9, 8, 4}, 5);
        for (int num : nums) {
            System.out.print(num + " ");
        }
    }
}
