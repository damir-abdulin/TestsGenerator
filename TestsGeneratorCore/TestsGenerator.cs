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
                .Add(UsingDirective(QualifiedName(IdentifierName("NUnit"), IdentifierName("Framework")))))
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
                    GenerateArrangeSection(method)
                        .Concat(GenerateActSection(method, classVariableName))
                        .Concat(GenerateAssertSection(method))
                )
            );
    }

    private StatementSyntax[] GenerateArrangeSection(MethodDeclarationSyntax method)
    {
        var parameters = method.ParameterList.Parameters;
        return parameters.Select(
            p => LocalDeclarationStatement(
                VariableDeclaration(
                        IdentifierName(p.Type.ToString()))
                    .WithVariables(
                        SingletonSeparatedList(
                            VariableDeclarator(
                                    Identifier(p.Identifier.ToString()))
                                .WithInitializer(
                                    EqualsValueClause(
                                        LiteralExpression(
                                            SyntaxKind.DefaultLiteralExpression,
                                            Token(SyntaxKind.DefaultKeyword)))))))).ToArray();
    }

    private StatementSyntax[] GenerateActSection(MethodDeclarationSyntax method, string classVariableName)
    {
        var parameters = method.ParameterList.Parameters;

        var arguments = new SyntaxNodeOrToken[2 * parameters.Count - 1];

        var currParameter = 0;
        for (var i = 0; i < arguments.Length; i++)
        {
            if (i % 2 == 0)
            {
                arguments[i] = Argument(IdentifierName(parameters[currParameter].Identifier.ToString()));
                currParameter++;
            }
            else
            {
                arguments[i] = Token(SyntaxKind.CommaToken);
            }
        }

        return new StatementSyntax[]
        {
            LocalDeclarationStatement(
                VariableDeclaration(
                        IdentifierName(
                            Identifier(
                                TriviaList(),
                                SyntaxKind.VarKeyword,
                                "var",
                                "var",
                                TriviaList())))
                    .WithVariables(
                        SingletonSeparatedList<VariableDeclaratorSyntax>(
                            VariableDeclarator(
                                    Identifier("actual"))
                                .WithInitializer(
                                    EqualsValueClause(
                                        InvocationExpression(
                                                MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    IdentifierName(classVariableName),
                                                    IdentifierName(method.Identifier.ToString())))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SeparatedList<ArgumentSyntax>(arguments))))))))
        };
    }
    
    private StatementSyntax[] GenerateAssertSection(MethodDeclarationSyntax method)
    {
        var returnType = method.ReturnType.ToString();

        if (returnType == "void")
        {
            return new StatementSyntax[] { 
                ExpressionStatement(
                    InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("Assert"),
                            IdentifierName("Fail")))
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList<ArgumentSyntax>(
                                    Argument(
                                        LiteralExpression(
                                            SyntaxKind.StringLiteralExpression,
                                            Literal("autogenerated")))))))};
        }
        
        return new StatementSyntax[] {
            LocalDeclarationStatement(
                VariableDeclaration(
                        IdentifierName(returnType))
                    .WithVariables(
                        SingletonSeparatedList(
                            VariableDeclarator(
                                    Identifier("excepted"))
                                .WithInitializer(
                                    EqualsValueClause(
                                        LiteralExpression(
                                            SyntaxKind.DefaultLiteralExpression,
                                            Token(SyntaxKind.DefaultKeyword))))))),
            ExpressionStatement(
                InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("Assert"),
                            IdentifierName("That")))
                    .WithArgumentList(
                        ArgumentList(
                            SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Argument(
                                        IdentifierName("actual")),
                                    Token(SyntaxKind.CommaToken),
                                    Argument(
                                        InvocationExpression(
                                                MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    IdentifierName("Is"),
                                                    IdentifierName("EqualTo")))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                        Argument(
                                                            IdentifierName("expected"))))))})))),
            ExpressionStatement(
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        IdentifierName("Assert"),
                        IdentifierName("Fail")))
                .WithArgumentList(
                    ArgumentList(
                        SingletonSeparatedList<ArgumentSyntax>(
                            Argument(
                                LiteralExpression(
                                    SyntaxKind.StringLiteralExpression,
                                    Literal("autogenerated")))))))};;
    }
}