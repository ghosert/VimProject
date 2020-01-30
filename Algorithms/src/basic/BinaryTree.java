package basic;

import java.util.*;

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

class TraversalBinaryTreeLevelOrder {

    public List<List<Integer>> levelOrderTraversal(TreeNode root) {
        List<List<Integer>> results = new ArrayList<>();
        if (root == null) {
            return results;
        }
        Queue<TreeNode> queue = new LinkedList<>();
        queue.offer(root);
        while (!queue.isEmpty()) {
            int size = queue.size();
            List<Integer> result = new ArrayList<>(size);
            for (int i = 0; i < size; i++) {
                TreeNode node = queue.poll();
                result.add(node.val);
                if (node.left != null) {
                    queue.offer(node.left);
                }
                if (node.right != null) {
                    queue.offer(node.right);
                }
            }
            results.add(result);
        }
        return results;
    }
}

public class BinaryTree {
    public static void main(String[] args) {
        // preorder/inorder/postorder cases:
        TreeNode root = BinaryTree.buildBinaryTree();
        TraversalBinaryTreeIteratively tbti = new TraversalBinaryTreeIteratively();
        // expected output: [1, 2, 3]
        BinaryTree.printNumbers(TraversalBinaryTreeIteratively.class.getSimpleName() + " preorder: ", tbti.preorderTraversal(root));
        // expected output: [1, 3, 2]
        BinaryTree.printNumbers(TraversalBinaryTreeIteratively.class.getSimpleName() + " inorder: ", tbti.inorderTraversal(root));
        // expected output: [3, 2, 1]
        BinaryTree.printNumbers(TraversalBinaryTreeIteratively.class.getSimpleName() + " postorder: ", tbti.postorderTraversal(root));

        // level order case:
        TreeNode rootForLevelOrder = BinaryTree.buildBinaryTreeForLevelOrder();
        TraversalBinaryTreeLevelOrder tbtlo = new TraversalBinaryTreeLevelOrder();
        // expected output:
        // [ [3], [9,20], [15,7] ]
        BinaryTree.printNumbers(TraversalBinaryTreeLevelOrder.class.getSimpleName() + " levelorder: ", tbtlo.levelOrderTraversal(rootForLevelOrder));
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

    private static TreeNode buildBinaryTreeForLevelOrder() {
        // Input: [3, 9, 20, null, null, 15, 7]
        // A Tree like below:
        //         3
        //    9        20
        //         15      7
        TreeNode node = new TreeNode(3);
        TreeNode secondNode = new TreeNode(9);
        TreeNode thirdNode = new TreeNode(20);
        TreeNode fourthNode = new TreeNode(15);
        TreeNode fifthNode = new TreeNode(7);
        node.left = secondNode;
        node.right = thirdNode;
        thirdNode.left = fourthNode;
        thirdNode.right = fifthNode;
        return node;
    }

    private static void printNumbers(String statement, List numbers) {
        System.out.println(statement);
        System.out.println(numbers.toString());
    }
}
