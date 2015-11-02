using Flame;
using Flame.Compiler;
using Flame.Compiler.Visitors;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace fecs
{
    public class LogPass : IPass<Tuple<IStatement, IMethod, ICompilerLog>, IStatement>
    {
        private LogPass()
        {
        }

        public static readonly LogPass Instance = new LogPass();

        public IStatement Apply(Tuple<IStatement, IMethod, ICompilerLog> Value)
        {
            var logVisitor = new LoggingVisitor(Value.Item3, true, false);
            return logVisitor.Visit(Value.Item1);
        }
    }
}
