using System;

namespace GuessProduct
{
    public static class Program
    {
        // This is a simple "hello, world"-like C# program.
        // It should compile fine under both csc and fecs.

        private static int ReadGuess(string[] Args, ref int Index)
        {
            if (Index < Args.Length)
            {
                return int.Parse(Args[Index++]);
            }
            else
            {
                return int.Parse(Console.ReadLine());
            }
        }

        public static void Main(string[] Args)
        {
            int n, m;
            if (Args.Length >= 2)
            {
                n = int.Parse(Args[0]);
                m = int.Parse(Args[1]);
            }
            else
            {
                // Generate two random numbers between zero and ten,
                // then have the user calculate their product.

                var rand = new Random();

                n = rand.Next(0, 10);
                m = rand.Next(0, 10);
            }

            int result = n * m;
            int argIndex = 2;

            Console.WriteLine("How much is " + n + " * " + m + "?");
            int answer = ReadGuess(Args, ref argIndex);
            while (answer != result)
            {
                Console.WriteLine("Seriously? Try again.");
                answer = ReadGuess(Args, ref argIndex);
            }
            Console.WriteLine("Congratulations. You did it.");
        }
    }
}
