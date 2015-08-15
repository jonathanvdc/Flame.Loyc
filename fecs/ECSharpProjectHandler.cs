using Ecs.Parser;
using Flame;
using Flame.Binding;
using Flame.Compiler;
using Flame.Compiler.Projects;
using Flame.Compiler.Variables;
using Flame.DSProject;
using Flame.Front;
using Flame.Front.Options;
using Flame.Front.Projects;
using Flame.Front.Target;
using Flame.Functional;
using Flame.Loyc;
using Flame.Verification;
using LeMP;
using Loyc;
using Loyc.Collections;
using Loyc.Syntax;
using Loyc.Syntax.Lexing;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace fecs
{
    public class ECSharpProjectHandler : IProjectHandler
    {
        public ECSharpProjectHandler()
        {
            processor = new MacroProcessor(typeof(LeMP.Prelude.Macros), MessageSink.Console);

            processor.AddMacros(typeof(global::LeMP.StandardMacros).Assembly, false);
            /*processor.PreOpenedNamespaces.Add((Symbol)"LeMP", false);
            processor.PreOpenedNamespaces.Add((Symbol)"LeMP.Prelude", false);*/

            converter = NodeConverter.DefaultNodeConverter;
        }

        private static MacroProcessor processor;
        private static NodeConverter converter;

        public IEnumerable<string> Extensions
        {
            get { return new string[] { "ecsproj", "ecs", "cs" }; }
        }

        public IProject Parse(ProjectPath Path, ICompilerLog Log)
        {
            if (Path.HasExtension("ecs") || Path.HasExtension("cs"))
            {
                return new SingleFileProject(Path, Log.Options.GetTargetPlatform());
            }
            else
            {
                return DSProject.ReadProject(Path.Path.Path);
            }
        }

        public IProject MakeProject(IProject Project, ProjectPath Path, ICompilerLog Log)
        {
            var newPath = Path.Path.Parent.Combine(Project.Name).ChangeExtension("ecsproj");
            var dsp = DSProject.FromProject(Project, newPath.AbsolutePath.Path);
            dsp.WriteTo(newPath.Path);
            return dsp;
        }

        public async Task<IAssembly> CompileAsync(IProject Project, CompilationParameters Parameters)
        {
            var name = Parameters.Log.GetAssemblyName(Project.AssemblyName ?? Project.Name ?? "");
            var extBinder = await Parameters.BinderTask;
            var asm = new DescribedAssembly(name, extBinder.Environment);

            var asmBinder = new CachingBinder(new DualBinder(asm.Binder, extBinder));

            var units = await ParseCompilationUnitsAsync(Project.GetSourceItems(), Parameters, asmBinder, asm);
            var rootNs = new RootNamespace(asm, units);

            // Perform a bait-and-switch here such that lazy evaluation will
            // take the types defined in this namespace into account when resolving symbols.
            asm.MainNamespace = rootNs;

            asm.EntryPoint = InferEntryPoint(asm);

            return asm;
        }

        private static IMethod InferEntryPoint(IAssembly Assembly)
        {
            foreach (var type in Assembly.CreateBinder().GetTypes())
            {
                foreach (var method in type.GetMethods())
                {
                    // Basically match anything that looks like `static void main(string[] Args)`
                    if (method.IsStatic && method.Name.Equals("main", StringComparison.OrdinalIgnoreCase) && method.ReturnType.Equals(PrimitiveTypes.Void))
                    {
                        var parameters = method.GetParameters();
                        if (parameters.Length == 1 && parameters[0].ParameterType.Equals(PrimitiveTypes.String.MakeArrayType(1)))
                        {
                            return method;
                        }
                    }
                }
            }
            return null;
        }

        public static Task<IFunctionalNamespace[]> ParseCompilationUnitsAsync(List<IProjectSourceItem> SourceItems, CompilationParameters Parameters, IBinder Binder, IAssembly DeclaringAssembly)
        {
            var units = new Task<IFunctionalNamespace>[SourceItems.Count];
            for (int i = 0; i < units.Length; i++)
            {
                var item = SourceItems[i];
                units[i] = ParseCompilationUnitAsync(item, Parameters, Binder, DeclaringAssembly);
            }
            return Task.WhenAll(units);
        }

        private static IReadOnlyDictionary<string, IVariable> GetParameters(IMethod Value)
        {
            var dict = new Dictionary<string, IVariable>();
            if (Value != null)
            {
                if (!Value.IsStatic && Value.DeclaringType != null)
                {
                    dict[CodeSymbols.This.Name] = ThisReferenceVariable.Instance.Create(Value.DeclaringType);
                }

                var parameters = Value.GetParameters();
                for (int i = 0; i < parameters.Length; i++)
                {
                    dict[parameters[i].Name] = new ArgumentVariable(parameters[i], i);
                }

                return dict;
            }

            return dict;
        }

        public static Task<IFunctionalNamespace> ParseCompilationUnitAsync(IProjectSourceItem SourceItem, CompilationParameters Parameters, IBinder Binder, IAssembly DeclaringAssembly)
        {
            Parameters.Log.LogEvent(new LogEntry("Status", "Parsing " + SourceItem.SourceIdentifier));
            return Task.Run(() =>
            {
                var code = ProjectHandlerHelpers.GetSourceSafe(SourceItem, Parameters);
                if (code == null)
                {
                    return null;
                }
                var namer = ECSharpTypeNamer.Instance;
                var convRules = DefaultConversionRules.Create(namer.Convert);
                var globalScope = new GlobalScope(new FunctionalBinder(Binder), convRules, Parameters.Log, namer, new Flame.Syntax.MemberProvider(Binder).GetMembers, GetParameters);
                var nodes = ParseNodes(code.Source, SourceItem.SourceIdentifier);
                var unit = ParseCompilationUnit(nodes, globalScope, DeclaringAssembly);
                Parameters.Log.LogEvent(new LogEntry("Status", "Parsed " + SourceItem.SourceIdentifier));
                return unit;
            });
        }

        public static IEnumerable<LNode> ParseNodes(string Text, string Identifier)
        {
            ILexer lexer = EcsLanguageService.Value.Tokenize(new UString(Text), Identifier, MessageSink.Console);

            var nodes = EcsLanguageService.Value.Parse(lexer, MessageSink.Console);

            return processor.ProcessSynchronously(new RVList<LNode>(nodes));
        }

        public static IFunctionalNamespace ParseCompilationUnit(IEnumerable<LNode> Nodes, GlobalScope Scope, IAssembly DeclaringAssembly)
        {
            return converter.ConvertCompilationUnit(Scope, DeclaringAssembly, Nodes);
        }

        public IEnumerable<ParsedProject> Partition(IEnumerable<ParsedProject> Projects)
        {
            return new ParsedProject[] { new ParsedProject(Projects.First().CurrentPath, UnionProject.CreateUnion(Projects.Select(item => item.Project).ToArray())) };
        }

        public PassPreferences GetPassPreferences(ICompilerLog Log)
        {
            return new PassPreferences(new string[] { },
                new PassInfo<Tuple<IStatement, IMethod>, Tuple<IStatement, IMethod>>[] 
                { 
                    new PassInfo<Tuple<IStatement, IMethod>, Tuple<IStatement, IMethod>>(new LogPass(Log), "check-nodes", true),
                    new PassInfo<Tuple<IStatement, IMethod>, Tuple<IStatement, IMethod>>(new VerifyingDeadCodePass(Log, 
                        "This method may not always return or throw. " + Warnings.Instance.GetWarningNameMessage("missing-return"), 
                        Log.UseDefaultWarnings("missing-return"),
                        "Unreachable code detected and removed. " + Warnings.Instance.GetWarningNameMessage("dead-code"),
                        Log.UsePedanticWarnings("dead-code")),
                        PassExtensions.EliminateDeadCodePassName, 
                        true)
                });
        }
    }
}
