package basic;

import java.util.Stack;

class TreeNode2 {
    int val;
    TreeNode2 left;
    TreeNode2 right;

    TreeNode2() {
    }

    TreeNode2(int val) {
        this.val = val;
    }

    TreeNode2(int val, TreeNode2 left, TreeNode2 right) {
        this.val = val;
        this.left = left;
        this.right = right;
    }
}

public class CousinTree {
    public boolean isCousins(TreeNode2 root, int x, int y) {
        if (root == null) return false;
        if (root.val == x || root.val == y) return false;
        Stack<TreeNode2> stack = new Stack<>();
        stack.add(root);
        while (!stack.isEmpty()) {
            int size = stack.size();
            boolean findXY = false;
            for (int i = 0; i < size; i++) {
                TreeNode2 node = stack.pop();
                if (node.left != null && node.right != null) {
                    if (node.left.val == x || node.left.val == y) {
                        if (node.right.val == x || node.right.val == y) {
                            return false;
                        }
                    }
                }
                if (node.left != null) {
                    if (node.left.val == x || node.left.val == y) {
                        if (findXY) {
                            return true;
                        } else {
                            findXY = true;
                        }
                    }
                    stack.push(node.left);
                }
                if (node.right != null) {
                    if (node.right.val == x || node.right.val == y) {
                        if (findXY) {
                            return true;
                        } else {
                            findXY = true;
                        }
                    }
                    stack.push(node.right);
                }
            }
        }
        return false;
    }

    public static void main(String[] args) {
        TreeNode2 t1 = new TreeNode2(1);
        TreeNode2 t2 = new TreeNode2(2);
        TreeNode2 t3 = new TreeNode2(3);
        TreeNode2 t4 = new TreeNode2(4);
        TreeNode2 t5 = new TreeNode2(5);
        t1.left = t2;
        t1.right = t3;
        t2.left = null;
        t2.right = t4;
        t3.left = null;
        t3.right = t5;
        System.out.println(new CousinTree().isCousins(t1, 5, 4));
    }
}
