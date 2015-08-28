using System;

namespace ArraysTest
{
    public static class Program
    {
        public static void Main(string[] Args)
        {
            var copy = new string[Args.Length];
            for (int i = 0; i < copy.Length; i++)
            {
                Console.WriteLine(copy[i] = Args[i]);
            }

            var items = new[] { "hello", "world" };
            for (int i = 0; i < items.Length; i++)
            {
                Console.WriteLine(items[i]);
            }
        }
    }
}
