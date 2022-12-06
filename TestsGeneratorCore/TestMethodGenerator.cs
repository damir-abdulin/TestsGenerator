using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;


/// <summary>
/// Contains methods for generating methods for tests.
/// </summary>
internal class TestMethodGenerator : MemberGenerator
{
    public TestMethodGenerator(ClassDeclarationSyntax classDeclarationSyntax)
        : base(classDeclarationSyntax, "Test") { }

    /// <summary>
    /// Generates test method with sections:
    /// <list type="bullet">
    ///     <item>
    ///     <term>Arrange</term>
    ///     <description>
    ///         contains input variables declaration with default values.
    ///     </description>
    ///     </item>
    ///     <item>
    ///     <term>act</term>
    ///     <description>
    ///         contains method call and declaration value 'actual', which contain source method output.
    ///     </description>
    ///     </item>
    ///     <item>
    ///     <term>assert</term>
    ///     <description>
    ///         contains expected output declaration and checks actual and expected values
    ///     </description>
    ///     </item>
    /// </list> 
    /// </summary>
    /// <param name="method">source method</param>
    /// <returns></returns>
    public MemberDeclarationSyntax GenerateTestMethodBlock(MethodDeclarationSyntax method)
    {
        var bodyBlock = GenerateBodyBlock(method);
        return 
            MethodDeclaration(
                PredefinedType(
                    Token(
                        SyntaxKind.VoidKeyword)),
                Identifier(
                    $"{method.Identifier}Test"))
            .WithAttributeLists(
                SingletonList(
                    AttributeList(
                        SingletonSeparatedList(
                            Attribute(
                                IdentifierName(Attribute))))))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.PublicKeyword)))
            .WithBody(bodyBlock);
    }

    private BlockSyntax GenerateBodyBlock(MethodDeclarationSyntax method)
    {
        var statements = new List<StatementSyntax>();
        AddArrangeSection(statements, method);
        AddActSection(statements, method);

        if (method.ReturnType.ToString() == "void")
            AddAssertSectionVoidMethod(statements, method);
        else
            AddAssertSectionNotVoidMethod(statements, method);
        
        return Block(statements);
    }

    private static void AddArrangeSection(ICollection<StatementSyntax> statements, BaseMethodDeclarationSyntax method)
    {
        var parameters = method.ParameterList.Parameters.ToList();
        parameters.ForEach(p => statements.Add(
            LocalDeclarationStatement(
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
                                            Token(SyntaxKind.DefaultKeyword)))))))));
    }
    private void AddActSection(ICollection<StatementSyntax> statements, MethodDeclarationSyntax method)
    {
        var parameters = method.ParameterList.Parameters;
        var arguments = GetArgumentsForFunction(parameters);

        statements.Add(
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
                                                    IdentifierName(ObjName),
                                                    IdentifierName(method.Identifier.ToString())))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SeparatedList<ArgumentSyntax>(arguments))))))))
            );
    }

    private static void AddAssertSectionVoidMethod(ICollection<StatementSyntax> statements, MethodDeclarationSyntax method)
    {
        statements.Add(
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
                                        Literal("autogenerated"))))))));
    }
    private static void AddAssertSectionNotVoidMethod(ICollection<StatementSyntax> statements, MethodDeclarationSyntax method)
    {
        statements.Add(
                LocalDeclarationStatement(
                    VariableDeclaration(
                            IdentifierName(method.ReturnType.ToString()))
                        .WithVariables(
                            SingletonSeparatedList(
                                VariableDeclarator(
                                        Identifier("excepted"))
                                    .WithInitializer(
                                        EqualsValueClause(
                                            LiteralExpression(
                                                SyntaxKind.DefaultLiteralExpression,
                                                Token(SyntaxKind.DefaultKeyword)))))))
            );

        statements.Add(
            ExpressionStatement(
                InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("Assert"),
                            IdentifierName("That")))
                    .WithArgumentList(
                        ArgumentList(
                            SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]
                                {
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
                                                            IdentifierName("expected"))))))
                                })))));
        statements.Add(
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
                                    Literal("autogenerated"))))))));
    }
    
    
}