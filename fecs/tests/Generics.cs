using System;
using System.Collections.Generic;
using System.Linq;

namespace GenericsTest
{
    public static class Program
    {
        public static void Main(string[] Args)
        {
            var list = new List<int>();
            list.Add(1);
            list.Add(2);
            Console.WriteLine(list.Count);
        }
    }
}
