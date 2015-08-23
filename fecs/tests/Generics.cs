using System;
using System.Collections.Generic;
using System.Linq;

namespace GenericsTest
{
    public static class Program
    {
        public static void Main(string[] Args)
        {
            var list = new List<string>(Args);
            Console.WriteLine(list.Count);
        }
    }
}
