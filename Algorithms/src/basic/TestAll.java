package basic;

import java.util.*;

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
