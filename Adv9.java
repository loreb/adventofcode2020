// https://adventofcode.com/2020/day/9
// Choice of language inspired by some sillyness in javax.crypto.*
import java.io.*;
import java.util.*;

public class Adv9 {
    public static void main(String[] args) throws IOException {
        int[] example5 = new int[] {
            35,
                20,
                15,
                25,
                47,
                40,
                62,
                55,
                65,
                95,
                102,
                117,
                150,
                182,
                127,
                219,
                299,
                277,
                309,
                576,
        };
        XMAS test = new XMAS(5);
        for (int x : example5) {
            if (!test.add(x)) {
                System.out.println("borked in test: " + x);
            }
        }
        System.out.println("TEST EXPECTED: 127\n\n");
        // for real now
        for (String arg : args) {
            XMAS xmas = new XMAS(25);
            //XMAS xmas = new XMAS(5);
            var all = new ArrayList<Long>();
            BufferedReader reader = new BufferedReader(new FileReader(arg));
            for(;;) {
                String line = reader.readLine();
                if (line == null)
                    break;
                long n = Long.parseLong(line);
                all.add(n);
                if (!xmas.add(n)) {
                    System.out.println("invalid: " + n);
                    // part2: you must find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.
                    // => add together the smallest & biggest numbers in the set.
                    long[] set = ContiguousSetOfNumbersThatSumTo(all, n);
                    long min = set[0];
                    long max = set[0];
                    for (long x : set) {
                        if (x < min) min = x;
                        if (x > max) max = x;
                    }
                    System.out.println("SUM: " + (min+max));
                }
            }
        }
    }
    public static long[] ContiguousSetOfNumbersThatSumTo(ArrayList<Long> a, long sum)
    {
        boolean verbose = true;
        for (int i = 0; i < a.size(); i++) {
            if (verbose)
                System.out.println("");
            long acc = a.get(i);
            for (int j = i+1; j < a.size(); j++) {
                acc += a.get(j);
                if (verbose)
                        System.out.println("acc += " + a.get(j) + " ==> " + acc);
                if (acc == sum) {
                    // XXX isn't there a slice or something in java?
                    var tmp = new ArrayList<Long>();
                    while (i <= j) {
                        tmp.add(a.get(i));
                        i++;
                    }
                    long[] ret = new long[tmp.size()];
                    for (i = 0; i < ret.length; i++) {
                        ret[i] = tmp.get(i);
                    }
                    return ret;
                }
                if (acc > sum)
                    break;
            }
        }
        throw new RuntimeException("crap");
    }
}

// The data appears to be encrypted with the eXchange-Masking Addition System (XMAS) which, conveniently for you, is an old cypher with an important weakness.
//
// XMAS starts by transmitting a preamble of 25 numbers.
// After that, each number you receive should be the sum of any two of the 25 immediately previous numbers.
// The two numbers will have different values, and there might be more than one such pair.
class XMAS {
    // The input contains "2272406775" => need more than 32 bit...
    private final int preambleLength;
    private final long[] x;
    private int pos;
    private int n;

    public XMAS(int len)
    {
        this.preambleLength = len;
        x = new long[preambleLength];
    }

    public boolean nextok(long number) {
        for (int i = 0; i < x.length; i++) {
            for (int j = 0; j < x.length; j++) {
                if (i == j)
                    continue;
                if (x[i] + x[j] == number)
                    return true;
            }
        }
        return false;
    }

    // add; return borked?
    public boolean add(long number)
    {
        if (n < preambleLength) {
            x[pos++] = number;
            n++;
            return true;
        } else {
            boolean rv = nextok(number);
            if (pos == preambleLength)
                pos = 0;
            x[pos++] = number;
            return rv;
        }
    }
    public boolean add(int number)
    {
        return add((long)number);
    }
}

