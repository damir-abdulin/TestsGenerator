using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;

internal static class SectionsGenerator
{
    /// <summary>
    /// Generates arrange, act and assert sections for test method.
    /// </summary>
    /// <param name="method">source method</param>
    /// <param name="classVariableName">object name for using in tests</param>
    /// <returns></returns>
    public static IEnumerable<StatementSyntax> GenerateTestMethodSections(MethodDeclarationSyntax method, string classVariableName)
    {
        return GenerateArrangeSection(method)
            .Concat(GenerateActSection(method, classVariableName))
            .Concat(GenerateAssertSection(method));
    }

    /// <summary>
    /// Generate global variables for Mock objects
    /// </summary>
    /// <param name="classDeclaration"></param>
    /// <param name="classVariableName"></param>
    /// <returns></returns>
    public static IEnumerable<MemberDeclarationSyntax> GenerateGlobalVarsSection
        (ClassDeclarationSyntax classDeclaration, string classVariableName)
    {
        var constructor = GetConstructor(classDeclaration);

        return 
            constructor is null
                ? GenerateGlobalVarsSectionWithoutCtor(classDeclaration, classVariableName)
                : GenerateGlobalVarsSectionWithCtor(constructor, classDeclaration, classVariableName);
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

    private static IEnumerable<MemberDeclarationSyntax> GenerateGlobalVarsSectionWithoutCtor
        (BaseTypeDeclarationSyntax classDeclaration, string classVariableName)
    {
        return new MemberDeclarationSyntax[] {
            FieldDeclaration(
                    VariableDeclaration(
                            IdentifierName(classDeclaration.Identifier))
                        .WithVariables(
                            SingletonSeparatedList<VariableDeclaratorSyntax>(
                                VariableDeclarator(
                                    Identifier(classVariableName)))))
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.PrivateKeyword)))}; 
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateGlobalVarsSectionWithCtor
        (ConstructorDeclarationSyntax constructor,
            BaseTypeDeclarationSyntax classDeclaration, string classVariableName)
    {
        var result = new MemberDeclarationSyntax[constructor.ParameterList.Parameters.Count + 1];
        result[0] = 
            FieldDeclaration(
                VariableDeclaration(
                        IdentifierName(classDeclaration.Identifier))
                    .WithVariables(
                        SingletonSeparatedList<VariableDeclaratorSyntax>(
                            VariableDeclarator(
                                Identifier(classVariableName)))))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.PrivateKeyword)));

        var membersCount = 1;
        
        var parameters = constructor.ParameterList.Parameters;

        foreach (var parameter in parameters)
        {
            var type = parameter.Type.ToString();

            if (type[0] == 'I')
            {
                result[membersCount] = 
                    FieldDeclaration(
                        VariableDeclaration(
                                IdentifierName(type))
                            .WithVariables(
                                SingletonSeparatedList<VariableDeclaratorSyntax>(
                                    VariableDeclarator(
                                        Identifier("_" + parameter.Identifier)))))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PrivateKeyword)));

                membersCount++;
            }
            
        }

        Array.Resize(ref result, membersCount);
        return result;
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
    private static ConstructorDeclarationSyntax? GetConstructor(TypeDeclarationSyntax classDeclaration)
    {
        return (ConstructorDeclarationSyntax?)classDeclaration.Members.FirstOrDefault(
            m => m.Kind() == SyntaxKind.ConstructorDeclaration);
    }
}