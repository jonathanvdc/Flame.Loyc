using System;

namespace PointersTest
{
    public static class Program
    {
        public static void Assign<T>(out T Target, T Value)
        {
            Target = Value;
        }

        public static void Main(string[] Args)
        {
            int x;
            Assign<int>(out x, 42);
            Console.WriteLine(x);
        }
    }
}
