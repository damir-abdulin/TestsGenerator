using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;

internal class SetUpMethodGenerator : MemberGenerator
{
    public SetUpMethodGenerator(ClassDeclarationSyntax classDeclarationSyntax)
        : base(classDeclarationSyntax, "SetUp") { }

    /// <summary>
    /// Generates SetUp method which inits class with mock objects
    /// </summary>
    /// <returns></returns>
    public MemberDeclarationSyntax GenerateSetUpMethod()
    {
        return MethodDeclaration(
                PredefinedType(
                    Token(SyntaxKind.VoidKeyword)),
                Identifier("SetUp"))
            .WithAttributeLists(
                SingletonList<AttributeListSyntax>(
                    AttributeList(
                        SingletonSeparatedList<AttributeSyntax>(
                            Attribute(
                                IdentifierName("SetUp"))))))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.PublicKeyword)))
            .WithBody(
                Block(GenerateMockObjectsInitSection()!));
    }
    
    private IEnumerable<StatementSyntax>? GenerateMockObjectsInitSection()
    {
        var constructor = GetConstructor(ClassDeclarationSyntax);
        
        if (constructor is null) return null;
        
        var result = new StatementSyntax[constructor.ParameterList.Parameters.Count];
        var parameters = constructor.ParameterList.Parameters;

        for (var i = 0; i < result.Length; i++)
        {
            var parameter = parameters[i];
            
            if (IsInterface(parameter.Type.ToString()))
                result[i] = GetMockObjectInitialization(parameter);
            else
                result[i] = GetLocalVariableDeclaration(parameter);
        }

        return result;
    }
    
    private static ConstructorDeclarationSyntax? GetConstructor(TypeDeclarationSyntax classDeclaration)
    {
        return (ConstructorDeclarationSyntax?)classDeclaration.Members.FirstOrDefault(
            m => m.Kind() == SyntaxKind.ConstructorDeclaration);
    }
    
    private static bool IsInterface(string typeName)
    {
        return typeName[0] == 'I';
    }

    private static StatementSyntax GetLocalVariableDeclaration(ParameterSyntax parameter)
    {
        return LocalDeclarationStatement(
            VariableDeclaration(
                    IdentifierName(parameter.Type.ToString()))
                .WithVariables(
                    SingletonSeparatedList<VariableDeclaratorSyntax>(
                        VariableDeclarator(
                                Identifier(parameter.Identifier.ToString()))
                            .WithInitializer(
                                EqualsValueClause(
                                    LiteralExpression(
                                        SyntaxKind.DefaultLiteralExpression,
                                        Token(SyntaxKind.DefaultKeyword)))))));
    }
    private static StatementSyntax GetMockObjectInitialization(ParameterSyntax parameter)
    {
        return ExpressionStatement(
            AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                IdentifierName("_" + parameter.Identifier),
                ObjectCreationExpression(
                    GenericName(
                            Identifier("Mock"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SingletonSeparatedList<TypeSyntax>(
                                    IdentifierName(parameter.Type.ToString())))))));
    }
}