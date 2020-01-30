package basic;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

class TreeNode {
    int val;
    TreeNode left;
    TreeNode right;
    TreeNode(int x) { val = x; }
}

class TraversalBinaryTreeIteratively {
    public List<Integer> preorderTraversal(TreeNode root) {
        List<Integer> results = new ArrayList<>();
        Stack<TreeNode> stack = new Stack<>();
        TreeNode node = root;
        while (node != null || !stack.empty()) {
            if (node != null) {
                results.add(node.val);
                stack.push(node);
                node = node.left;
            } else {
                node = stack.pop();
                node = node.right;
            }
        }
        return results;
    }

    public List<Integer> inorderTraversal(TreeNode root) {
        List<Integer> results = new ArrayList<>();
        Stack<TreeNode> stack = new Stack<>();
        TreeNode node = root;
        while (node != null || !stack.isEmpty()) {
            if (node != null) {
                stack.push(node);
                node = node.left;
            } else {
                node = stack.pop();
                results.add(node.val);
                node = node.right;
            }
        }
        return results;
    }

    public List<Integer> postorderTraversal(TreeNode root) {
        List<Integer> results = new LinkedList<>();
        if (root == null) {
            return results;
        }
        Stack<TreeNode> stack = new Stack<>();
        stack.push(root);
        while (!stack.isEmpty()) {
            TreeNode node = stack.pop();
            results.add(0, node.val);
            if (node.left != null) {
                stack.push(node.left);
            }
            if (node.right != null) {
                stack.push(node.right);
            }
        }
        return results;
    }
}

public class BinaryTree {
    public static void main(String[] args) {
        TreeNode root = BinaryTree.buildBinaryTree();
        TraversalBinaryTreeIteratively tbti = new TraversalBinaryTreeIteratively();
        // expected output: [1, 2, 3]
        BinaryTree.printNumbers("TraversalBinaryTreeIteratively preorder: ", tbti.preorderTraversal(root));
        // expected output: [1, 3, 2]
        BinaryTree.printNumbers("TraversalBinaryTreeIteratively inorder: ", tbti.inorderTraversal(root));
        // expected output: [3, 2, 1]
        BinaryTree.printNumbers("TraversalBinaryTreeIteratively postorder: ", tbti.postorderTraversal(root));
    }

    private static TreeNode buildBinaryTree() {
        // Input: [1, null, 2, 3]
        // A Tree like below:
        //          1
        //    null      2
        //           3     null
        TreeNode node = new TreeNode(1);
        TreeNode secondNode = new TreeNode(2);
        TreeNode thirdNode = new TreeNode(3);
        node.right = secondNode;
        secondNode.left = thirdNode;
        return node;
    }

    private static void printNumbers(String statement, List<Integer> numbers) {
        System.out.println(statement);
        for (int n : numbers) {
            System.out.printf("%d ", n);
        }
        System.out.println();
    }
}
