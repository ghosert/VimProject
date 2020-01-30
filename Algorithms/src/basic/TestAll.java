package basic;

import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

public class TestAll {
    public static void main(String[] args) {
        Queue<Integer> queue = new LinkedList<>();
        queue.offer(1);
        System.out.println("queue is empty and value: " + queue.isEmpty() + " " + queue.poll());
        Stack<Integer> stack = new Stack<>();
        stack.push(1);
        System.out.println("stack is empty and value: " + stack.isEmpty() + " " + stack.pop());
    }
}
