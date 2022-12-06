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

        var methods = classDeclaration.Members
            .Where(mem => mem.Kind() == SyntaxKind.MethodDeclaration)
            .Where(mem => mem.Modifiers.Any(m => m.Kind() == SyntaxKind.PublicKeyword));

        var testCode = methods.Select(m => 
            CreateTestMethod((MethodDeclarationSyntax)m, "_class"));

        var classDecl =
            ClassDeclaration(classDeclaration.Identifier + "Tests")
                .WithMembers(new SyntaxList<MemberDeclarationSyntax>(testCode));
        
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

    private MethodDeclarationSyntax CreateTestMethod(MethodDeclarationSyntax method, string classVariableName)
    {
        return MethodDeclaration(ParseTypeName("void"), method.Identifier + "Test")
            .WithAttributeLists(
                SingletonList(
                    AttributeList(SingletonSeparatedList(Attribute(IdentifierName("Test"))))))
            .WithModifiers(TokenList(Token(SyntaxKind.PublicKeyword)))
            .WithBody(
                Block(
                    SectionsGenerator.GenerateAllSections(method, classVariableName)
                )
            );
    }
}