using System;

namespace GuessProduct
{
    public static class Program
    {
        // This is a simple "hello, world"-like C# program.
        // It should compile fine under both csc and fecs.

        public static void Main(string[] Args)
        {
            // Generate two random numbers between zero and ten,
            // then have the user calculate their product.

            var rand = new Random();

            int n = rand.Next(0, 10), m = rand.Next(0, 10);
            int result = n * m;

            Console.WriteLine("How much is " + n + " * " + m + "?");
            int answer = int.Parse(Console.ReadLine());
            while (answer != result)
            {
                Console.WriteLine("Seriously? Try again.");
                answer = int.Parse(Console.ReadLine());
            }
            Console.WriteLine("Congratulations. You did it.");
        }
    }
}
