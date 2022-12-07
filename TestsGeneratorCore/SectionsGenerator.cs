using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;

internal static class SectionsGenerator
{
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

            if (IsInterface(type))
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
    

    private static bool IsInterface(string typeName)
    {
        return typeName[0] == 'I';
    }
    
    private static ConstructorDeclarationSyntax? GetConstructor(TypeDeclarationSyntax classDeclaration)
    {
        return (ConstructorDeclarationSyntax?)classDeclaration.Members.FirstOrDefault(
            m => m.Kind() == SyntaxKind.ConstructorDeclaration);
    }
}