// C# after java is just natural.
using System;
using System.Collections.Generic;

public class AdapterArray {
    const int SeatJoltage = 0;

    public static void doit(string filename)
    {
        System.Console.WriteLine("\nfile: " + filename);
        System.IO.StreamReader file = new System.IO.StreamReader(filename);
        string line;

        int max = -1;
        var all = new List<Adapter>();
        while((line = file.ReadLine()) != null) {
            int j = int.Parse(line);
            all.Add(new Adapter(j));
            if (j > max) max = j;
        }
        all.Add(new Adapter(max+3));
        all.Sort(Adapter.Cmp);
        // now count the 1-differences and the 3-differences
        // SKIPPING NO ADAPTER!
        int output = 0;
        int num1 = 0;
        int num3 = 0;
        foreach (Adapter x in all) {
            if (!x.CanConnect(output)) {
                throw new Exception("going from " + output + " to " + x.ToString());
            }
            if (x.output - output == 1) {
                output = x.output;
                num1++;
                continue;
            }
            if (x.output - output == 3) {
                output = x.output;
                num3++;
                continue;
            }
            throw new Exception("WTF from " + output + " to " + x.ToString());
        }
        int product = num1 * num3;
        Console.WriteLine("PART1: " + num1 + " x " + num3 + " = " + product);
        // part2: count ALL the possible ways to do it...
        // I'll be lazy and do it with an array of booleans.
        // THEN I read how many lines there are in the input
        // "ah, le potenze di due!"
        if (all[0].output != 0) {
            // URGH! I special cased the zero in part1...
            all.Add(new Adapter(0));
            all.Sort(Adapter.Cmp);
        }
        Console.WriteLine("#combinations = " + MemoizedCountCombinations(all,
                    new Dictionary<int,long>()
                    ));
        if (all.Count < 40) {
            Console.WriteLine("naive#combinations = " + CountCombinations(all));
        }
    }

    // I had to run it to realize its complexity;
    // leaving it here to appreciate how stupid I can be.
    static int CountCombinations(IList<Adapter> list, int from=0)
    {
        if (from == list.Count - 1)
            return 1;
        int n = 0;
        for (int i = from+1; i < list.Count; i++) {
            if (!list[i].CanConnect(list[from].output)) {
                // the list is SORTED
                break;
            }
            n += CountCombinations(list, i);
        }
        return n;
    }

    // Take away the Set and you'll be counting the same path many times...
    // XXX overflows int32!
    // I'm starting to wonder if the authors are fans of infinite precision,
    // ie python/lisp/... programmers.
    static long MemoizedCountCombinations(IList<Adapter> list, IDictionary<int,long> memo, int from=0)
    {
        if (from == list.Count - 1) {
            return 1;
        }
        long n = 0;
        for (int i = from+1; i < list.Count; i++) {
            long lucky = 0;
            if (!list[i].CanConnect(list[from].output)) {
                // the list is SORTED
                break;
            }
            // Somehow I managed to put the IF below BEFORE the CanConnect...
            // Having a playstation in the background is distracting,
            // but still I'm ashamed how long it took me to notice...
            if (memo.TryGetValue(i, out lucky)) {
                n += lucky;
                continue;
            }
            long sub = MemoizedCountCombinations(list, memo, i);
            memo[i] = sub;
            n += sub;
        }
        if (n < 0)
            throw new Exception("long overflow! " + n);
        return n;
    }


    public static void Main(string[] args)
    {
        foreach (string arg in args) {
            doit(arg);
        }
    }
}

// The laptop has a builtin adapter that's 3 + the highest joltage we have.
class Adapter {
    // Each of your joltage adapters is rated for a specific output joltage (your puzzle input).
    // Any given adapter can take an input 1, 2, or 3 jolts lower than its rating and still produce its rated output joltage.
    public readonly int output, tolerance;
    public Adapter(int output, int tolerance=3)
    {
        this.output = output;
        this.tolerance = tolerance;
    }
    public bool CanConnect(int joltage)
    {
        for (int i = 0; i <= tolerance; i++) {
            if (joltage+i == output)
                return true;
        }
        return false;
    }
    public static int Cmp(Adapter x, Adapter y)
    {
        if (x.output.CompareTo(y.output) != 0)
            return x.output.CompareTo(y.output);
        // XXX
        return x.tolerance.CompareTo(y.tolerance);
    }
    public override string ToString()
    {
        return "(output="+output + ", tolerance="+tolerance+")";
    }
}
