using System.Collections.Specialized;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;

/// <summary>
/// Static class for generates tests.
/// </summary>
public class TestsGenerator
{
    /// <summary>
    /// Generates tests for class from `classes`
    /// </summary>
    /// <param name="source">Source code for generation.</param>
    /// <returns>Generated tests</returns>
    public List<TestInfo> Generate(string source)
    {
        var root = CSharpSyntaxTree.ParseText(source).GetCompilationUnitRoot();
        var classes = root.DescendantNodes().OfType<ClassDeclarationSyntax>();
        var usings = root.DescendantNodes().OfType<UsingDirectiveSyntax>();
        
        var result = classes
            .Where(cl => cl.Modifiers.Any(m => m.Kind() == SyntaxKind.PublicKeyword))
            .Select(cl => GenerateTest(usings, cl))
            .ToList();

        return result;
    }

    private TestInfo GenerateTest(IEnumerable<UsingDirectiveSyntax> usings, ClassDeclarationSyntax classDeclaration)
    {
        var testMethodGenerator = new TestMethodGenerator(classDeclaration);
        var setUpMethodGenerator = new SetUpMethodGenerator(classDeclaration);
        var globalVariablesGenerator = new GlobalVariablesGenerator(classDeclaration);
        

        var methods = classDeclaration.Members
            .Where(mem => mem.Kind() == SyntaxKind.MethodDeclaration)
            .Where(mem => mem.Modifiers.Any(m => m.Kind() == SyntaxKind.PublicKeyword));
        
        var testCode = methods.Select(m => 
            testMethodGenerator.GenerateTestMethod((MethodDeclarationSyntax)m));


        var classDecl =
            ClassDeclaration(classDeclaration.Identifier + "Tests")
                .WithMembers(new SyntaxList<MemberDeclarationSyntax>(
                    globalVariablesGenerator.GenerateGlobalVarsSection()
                        .Append(setUpMethodGenerator.GenerateSetUpMethod())
                        .Concat(testCode)));
        
        var source = CompilationUnit()
            .WithUsings(new SyntaxList<UsingDirectiveSyntax>(usings)
                .Add(UsingDirective(QualifiedName(IdentifierName("NUnit"), IdentifierName("Framework"))))
                .Add(UsingDirective(IdentifierName("Moq")))
                .Add(UsingDirective(IdentifierName(classDeclaration.Identifier))))
            .AddMembers(classDecl)
            .NormalizeWhitespace().ToFullString();
        
        return new TestInfo(
            classDeclaration.Identifier.ToString(), source);
    }
}