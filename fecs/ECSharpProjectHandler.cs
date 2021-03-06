﻿using Loyc.Ecs;
using Flame;
using Flame.Analysis;
using Flame.Binding;
using Flame.Compiler;
using Flame.Compiler.Projects;
using Flame.Compiler.Variables;
using Flame.Compiler.Visitors;
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
using Loyc.Syntax.Les;
using Loyc.Syntax.Lexing;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace fecs
{
    public class ECSharpProjectHandler : IProjectHandler
    {
        static ECSharpProjectHandler()
        {
            converter = NodeConverter.DefaultNodeConverter;
        }

        private static NodeConverter converter;

        public IEnumerable<string> Extensions
        {
            get { return new string[] { "ecsproj", "ecs", "cs", "les" }; }
        }

        public IProject Parse(ProjectPath Path, ICompilerLog Log)
        {
            if (Path.HasExtension("ecs") || Path.HasExtension("cs") || Path.HasExtension("les"))
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

        /// <summary>
        /// Infers an assembly's entry point, which is any function that matches the following pattern:
        /// `static void main(string[] Args)`.
        /// </summary>
        /// <param name="Assembly"></param>
        /// <returns></returns>
        private static IMethod InferEntryPoint(IAssembly Assembly)
        {
            foreach (var type in Assembly.CreateBinder().GetTypes())
            {
                foreach (var method in type.GetMethods())
                {
                    // Basically match anything that looks like `static void main(...)`
                    if (method.IsStatic && method.Name.Equals("main", StringComparison.OrdinalIgnoreCase) && method.ReturnType.Equals(PrimitiveTypes.Void))
                    {
                        return method;
                    }
                }
            }
            return null;
        }

        public static Task<IFunctionalNamespace[]> ParseCompilationUnitsAsync(List<IProjectSourceItem> SourceItems, CompilationParameters Parameters, IBinder Binder, IAssembly DeclaringAssembly)
        {
            var sink = new CompilerLogMessageSink(Parameters.Log);
            var processor = new MacroProcessor(typeof(LeMP.Prelude.BuiltinMacros), sink);

            processor.AddMacros(typeof(global::LeMP.StandardMacros).Assembly, false);

            return ParseCompilationUnitsAsync(SourceItems, Parameters, Binder, DeclaringAssembly, processor, sink);
        }

        public static Task<IFunctionalNamespace[]> ParseCompilationUnitsAsync(List<IProjectSourceItem> SourceItems, CompilationParameters Parameters, IBinder Binder, IAssembly DeclaringAssembly, MacroProcessor Processor, IMessageSink Sink)
        {
            var units = new Task<IFunctionalNamespace>[SourceItems.Count];
            for (int i = 0; i < units.Length; i++)
            {
                var item = SourceItems[i];
                units[i] = ParseCompilationUnitAsync(item, Parameters, Binder, DeclaringAssembly, Processor, Sink);
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
                    var arg = new ArgumentVariable(parameters[i], i);
                    if (parameters[i].ParameterType.GetIsPointer() && 
                        parameters[i].ParameterType.AsContainerType().AsPointerType().PointerKind.Equals(PointerKind.ReferencePointer))
                    {
                        dict[parameters[i].Name] = new AtAddressVariable(arg.CreateGetExpression());
                    }
                    else
                    {
                        dict[parameters[i].Name] = arg;
                    }
                }

                return dict;
            }

            return dict;
        }

        private static IParsingService GetParsingService(ICompilerOptions Options, string Key, IParsingService Default)
        {
            switch (Options.GetOption<string>(Key, "").ToLower())
            {
                case "les":
                    return LesLanguageService.Value;
                case "ecs":
                    return EcsLanguageService.Value;
                case "cs":
                    return EcsLanguageService.WithPlainCSharpPrinter;
                default:
                    return Default;
            }
        }

        public static Task<IFunctionalNamespace> ParseCompilationUnitAsync(IProjectSourceItem SourceItem, CompilationParameters Parameters, IBinder Binder, IAssembly DeclaringAssembly, MacroProcessor Processor, IMessageSink Sink)
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
                bool isLes = Enumerable.Last(SourceItem.SourceIdentifier.Split('.')).Equals("les", StringComparison.OrdinalIgnoreCase);
                var service = isLes ? (IParsingService)LesLanguageService.Value : EcsLanguageService.Value;
                var nodes = ParseNodes(code.Source, SourceItem.SourceIdentifier, service, Processor, Sink);

                if (Parameters.Log.Options.GetOption<bool>("E", false))
                {
                    var outputService = GetParsingService(Parameters.Log.Options, "syntax-format", service);
                    string newFile = outputService.Print(nodes, Sink, indentString: new string(' ', 4));
                    Parameters.Log.LogMessage(new LogEntry("'" + SourceItem.SourceIdentifier + "' after macro expansion", Environment.NewLine + newFile));
                }

                var unit = ParseCompilationUnit(nodes, globalScope, DeclaringAssembly);
                Parameters.Log.LogEvent(new LogEntry("Status", "Parsed " + SourceItem.SourceIdentifier));
                return unit;
            });
        }

        public static IEnumerable<LNode> ParseNodes(string Text, string Identifier, IParsingService Service, MacroProcessor Processor, IMessageSink Sink)
        {
            var lexer = Service.Tokenize(new UString(Text), Identifier, Sink);

            var nodes = Service.Parse(lexer, Sink);

            return Processor.ProcessSynchronously(new VList<LNode>(nodes));
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
            return new PassPreferences(new PassCondition[] 
            {
                new PassCondition("check-nodes", _ => true),
                new PassCondition(AutoInitializationPass.AutoInitializationPassName, _ => true),
                new PassCondition(ValueTypeDelegateVisitor.ValueTypeDelegatePassName, 
                    optInfo => ValueTypeDelegateVisitor.ValueTypeDelegateWarning.UseWarning(optInfo.Log.Options)),
                new PassCondition(Flame.Front.Target.PassExtensions.EliminateDeadCodePassName,
                    optInfo => optInfo.OptimizeMinimal || optInfo.OptimizeDebug),
                new PassCondition(InfiniteRecursionPass.InfiniteRecursionPassName,
                    optInfo => InfiniteRecursionPass.IsUseful(optInfo.Log)),
            },
            new PassInfo<Tuple<IStatement, IMethod, ICompilerLog>, IStatement>[] 
            { 
                new PassInfo<Tuple<IStatement, IMethod, ICompilerLog>, IStatement>(
                    LogPass.Instance, 
                    "check-nodes"),

                new PassInfo<Tuple<IStatement, IMethod, ICompilerLog>, IStatement>(
                    AnalysisPasses.ValueTypeDelegatePass,
                    ValueTypeDelegateVisitor.ValueTypeDelegatePassName),

                new PassInfo<Tuple<IStatement, IMethod, ICompilerLog>, IStatement>(
                    VerifyingDeadCodePass.Instance,
                    Flame.Front.Target.PassExtensions.EliminateDeadCodePassName),

                new PassInfo<Tuple<IStatement, IMethod, ICompilerLog>, IStatement>(
                    AutoInitializationPass.Instance,
                    AutoInitializationPass.AutoInitializationPassName),

                new PassInfo<Tuple<IStatement, IMethod, ICompilerLog>, IStatement>(
                    InfiniteRecursionPass.Instance,
                    InfiniteRecursionPass.InfiniteRecursionPassName)
            });
        }
    }
}
