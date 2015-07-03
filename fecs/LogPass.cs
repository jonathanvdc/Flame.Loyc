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
    public class LogPass : IPass<Tuple<IStatement, IMethod>, Tuple<IStatement, IMethod>>
    {
        public LogPass(ICompilerLog Log)
        {
            this.Log = Log;
        }

        public ICompilerLog Log { get; private set; }

        public Tuple<IStatement, IMethod> Apply(Tuple<IStatement, IMethod> Value)
        {
            var logVisitor = new LoggingVisitor(Log, true);
            return new Tuple<IStatement, IMethod>(logVisitor.Visit(Value.Item1), Value.Item2);
        }
    }
}
