using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;

internal static class SectionsGenerator
{
    /// <summary>
    /// Generates arrange, act and assert sections.
    /// </summary>
    /// <param name="method">source method</param>
    /// /// <param name="classVariableName">object name for using in tests</param>
    /// <returns></returns>
    public static IEnumerable<StatementSyntax> GenerateAllSections(MethodDeclarationSyntax method, string classVariableName)
    {
        return GenerateArrangeSection(method)
            .Concat(GenerateActSection(method, classVariableName))
            .Concat(GenerateAssertSection(method));
    }
    
    /// <summary>
    /// Generates arrange section
    /// </summary>
    /// <param name="method">source method</param>
    /// <returns>List with statements</returns>
    public static IEnumerable<StatementSyntax> GenerateArrangeSection(MethodDeclarationSyntax method)
    {
        var parameters = method.ParameterList.Parameters;
        return parameters.Select(p =>
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
                                            Token(SyntaxKind.DefaultKeyword)))))))).ToArray();
    }

    /// <summary>
    /// Generates act section.
    /// </summary>
    /// <param name="method">source method</param>
    /// <param name="classVariableName">object name for using in tests</param>
    /// <returns>List with statements</returns>
    public static IEnumerable<StatementSyntax> GenerateActSection(MethodDeclarationSyntax method, string classVariableName)
    {
        var parameters = method.ParameterList.Parameters;
        var arguments = GetArguments(parameters);
        
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
    
    /// <summary>
    /// Generates assert section
    /// </summary>
    /// <param name="method">source method</param>
    /// <returns>List with statements</returns>
    public static IEnumerable<StatementSyntax> GenerateAssertSection(MethodDeclarationSyntax method)
    {
        var returnType = method.ReturnType.ToString();
        return returnType == "void" ? GenerateAssertVoidSection() : GenerateAssertNotVoidSection(returnType);
    }


    private static IEnumerable<SyntaxNodeOrToken> GetArguments(SeparatedSyntaxList<ParameterSyntax> parameters)
    {
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

        return arguments;
    }
    
    private static IEnumerable<StatementSyntax> GenerateAssertVoidSection()
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

    private static IEnumerable<StatementSyntax> GenerateAssertNotVoidSection(string returnType)
    {
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