using Flame;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace fecs
{
    public class RootNamespace : INamespaceBranch
    {
        public RootNamespace(IAssembly DeclaringAssembly, IEnumerable<INamespaceBranch> Namespaces)
        {
            this.DeclaringAssembly = DeclaringAssembly;
            this.Namespaces = Namespaces;
        }

        public IAssembly DeclaringAssembly { get; private set; }
        public IEnumerable<INamespaceBranch> Namespaces { get; private set; }

        public IEnumerable<INamespaceBranch> GetNamespaces()
        {
            return Namespaces;
        }

        public string FullName
        {
            get { return Name; }
        }

        public IEnumerable<IAttribute> GetAttributes()
        {
            return Enumerable.Empty<IAttribute>();
        }

        public string Name
        {
            get { return ""; }
        }

        public IType[] GetTypes()
        {
            return new IType[] { };
        }
    }
}
