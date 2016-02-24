using System;
using System.Collections.Generic;
using System.Linq;

namespace GenericsTest
{
    public class Ref<T>
    {
        public Ref() { Assign<T>(out this.Value, GetDefault<T>()); }

        public T Value;

        public static U GetDefault<U>()
        {
            return default(U);
        }

        public static void Assign<U>(out U Target, U Value)
        {
            Target = Value;
        }
    }

    public static class Program
    {
        public static void Main(string[] Args)
        {
            var refObj = new Ref<List<string>>();
            refObj.Value = new List<string>(Args);
            Console.WriteLine(refObj.Value.Count);
        }
    }
}
