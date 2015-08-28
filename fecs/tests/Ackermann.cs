using System;

public static class Program
{
    public static int Ackermann(int m, int n)
    {
        if (m == 0)
            return n + 1;
        else if (n == 0)
            return Ackermann(m - 1, 1);
        else
            return Ackermann(m - 1, Ackermann(m, n - 1));
    }

    public static void Main(string[] Args)
    {
        if (Args.Length == 2)
            Console.WriteLine(Ackermann(int.Parse(Args[0]), int.Parse(Args[1])));
        else
            Console.WriteLine("This program takes exactly two integer arguments.")
    }
}
