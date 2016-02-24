using Flame.Front.Cli;
using Flame.Front.Projects;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace fecs
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            ProjectHandlers.RegisterHandler(new ECSharpProjectHandler());
            var compiler = new ConsoleCompiler("fecs", "the Flame EC# compiler", "https://github.com/jonathanvdc/Flame.Loyc/releases");
            Environment.Exit(compiler.Compile(args));
        }
    }
}
