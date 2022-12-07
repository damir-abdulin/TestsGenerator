using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;

internal class GlobalVariablesGenerator : MemberGenerator
{
    public GlobalVariablesGenerator(ClassDeclarationSyntax classDeclarationSyntax) : base(classDeclarationSyntax)
    { }
    
    /// <summary>
    /// Generate global variables for Mock objects
    /// </summary>
    /// <returns></returns>
    public IEnumerable<MemberDeclarationSyntax> GenerateGlobalVarsSection()
    {
        var constructor = GetConstructor(ClassDeclarationSyntax);

        return 
            constructor is null
                ? GenerateForClassWithoutConstructor()
                : GenerateForClassWithConstructor(constructor);
    }

    private IEnumerable<MemberDeclarationSyntax> GenerateForClassWithConstructor(ConstructorDeclarationSyntax constructor)
    {
        var result = new MemberDeclarationSyntax[constructor.ParameterList.Parameters.Count + 1];
        result[0] = 
            FieldDeclaration(
                    VariableDeclaration(
                            IdentifierName(ClassDeclarationSyntax.Identifier))
                        .WithVariables(
                            SingletonSeparatedList<VariableDeclaratorSyntax>(
                                VariableDeclarator(
                                    Identifier(ObjName)))))
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.PrivateKeyword)));

        var membersCount = 1;
        
        var parameters = constructor.ParameterList.Parameters;

        foreach (var parameter in parameters)
        {
            var type = parameter.Type.ToString();

            if (!IsInterface(type)) continue;
            
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

        Array.Resize(ref result, membersCount);
        return result;
    }
    
    private IEnumerable<MemberDeclarationSyntax> GenerateForClassWithoutConstructor()
    {
        return new MemberDeclarationSyntax[] {
            FieldDeclaration(
                    VariableDeclaration(
                            IdentifierName(ClassDeclarationSyntax.Identifier))
                        .WithVariables(
                            SingletonSeparatedList<VariableDeclaratorSyntax>(
                                VariableDeclarator(
                                    Identifier(ObjName)))))
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.PrivateKeyword)))}; 
    }
}