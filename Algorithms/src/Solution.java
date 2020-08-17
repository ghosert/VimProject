import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;
import java.util.regex.*;

/**

 Imagine we are building an application that is used by many different customers. We want to avoid one customer being able to overload the system by sending too many requests, so we enforce a per-customer rate limit. The rate limit is defined as:  Each customer can make X requests per Y seconds  Assuming that customer ID is extracted somehow from the request, implement the following function
 // Perform rate limiting logic for provided customer ID. Return true if the // request is allowed, and false if it is not.
 boolean rateLimit(int customerId)
 **/


public class Solution {

    private Queue<Long> timeQueue = new LinkedList<>();
    private Map<Long, Long> requests = new HashMap<>();
    private int REQ = 5;
    private int SECS = 5;

    public boolean rateLimit(int customerId) {
        long timestamp = System.currentTimeMillis() / 1000; // 1000 1200 1500 1 sec

        while (!timeQueue.isEmpty()) {
            long earlierTS = timeQueue.peek();
            if (timestamp - earlierTS >= SECS) {
                Long ts = timeQueue.poll();
                requests.remove(ts);
            } else {
                break;
            }
        }

        long totalCounts = 0;
        for (Long counts : requests.values()) {
            totalCounts = totalCounts + counts;
        }

        if (totalCounts >= REQ) {
            return false;
        } else {
            boolean counts = requests.containsKey(timestamp);
            if (counts == false) {
                timeQueue.add(timestamp); // 1st - 10,
                requests.put(timestamp, 1L);
            } else {
                requests.put(timestamp, requests.get(timestamp) + 1);
            }
            return true;
        }

    }


    public static void main(String[] args) throws Exception {
        Solution solution = new Solution();
        // 5 requests
        boolean result1 = solution.rateLimit(1);
        System.out.println(result1);
        result1 = solution.rateLimit(1);
        System.out.println(result1);
        result1 = solution.rateLimit(1);
        System.out.println(result1);
        result1 = solution.rateLimit(1);
        System.out.println(result1);
        result1 = solution.rateLimit(1);
        System.out.println(result1);
        result1 = solution.rateLimit(1);
        System.out.println(result1);
        /// result1 = true;
        // wait for 3 seconds
        Thread.sleep(3000);
        boolean result = solution.rateLimit(1);
        System.out.println(result);
        /// result = false;
        // wait for 3 seconds
        Thread.sleep(3000);
        result = solution.rateLimit(1);
        System.out.println(result);
        /// result = true;
    }
}

// https://www.hackerrank.com/paper/txxmgwzhiuychrvuhvaykqfdgrthbwch?b=eyJpbnRlcnZpZXdfaWQiOjEzMDUyODEsInJvbGUiOiJpbnRlcnZpZXdlciIsInNob3J0X3VybCI6Imh0dHA6Ly9oci5ncy81MzFmNjciLCJjYW5kaWRhdGVfdXJsIjoiaHR0cDovL2hyLmdzLzVkZjZhMCJ9

