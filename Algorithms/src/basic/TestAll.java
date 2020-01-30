package basic;

import java.util.LinkedList;
import java.util.Queue;

public class TestAll {
    public static void main(String[] args) {
        Queue<Integer> queue = new LinkedList<>();
        queue.offer(1);
        System.out.println("queue is empty and value: " + queue.isEmpty() + " " + queue.poll());
    }
}
