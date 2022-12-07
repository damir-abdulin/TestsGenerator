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
                Block(GenerateMockObjectsInitSection()?.Append(GenerateConstructorCall())!));
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
                                        IdentifierName(parameter.Type.ToString())))))
                    .WithArgumentList(
                        ArgumentList())));
    }

    private StatementSyntax? GenerateConstructorCall()
    {
        var constructor = GetConstructor(ClassDeclarationSyntax);
        
        if (constructor is null) return null;

        var arguments = GetArguments(constructor.ParameterList.Parameters);

        return ExpressionStatement(
            AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                IdentifierName(ObjName),
                ObjectCreationExpression(
                        IdentifierName(ClassDeclarationSyntax.Identifier))
                    .WithArgumentList(
                        ArgumentList(
                            SeparatedList<ArgumentSyntax>(arguments)))));
    }

    private static IEnumerable<SyntaxNodeOrToken> GetArguments(SeparatedSyntaxList<ParameterSyntax> parameters)
    {
        var arguments = new SyntaxNodeOrToken[2 * parameters.Count - 1];

        var currParameter = 0;
        for (var i = 0; i < arguments.Length; i++)
        {
            if (i % 2 == 0)
            {
                if (IsInterface(parameters[currParameter].Type.ToString()))
                {
                    arguments[i] = Argument(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("_" + parameters[currParameter].Identifier),
                            IdentifierName("Object")));
                }
                else
                {
                    arguments[i] = Argument(IdentifierName(parameters[currParameter].Identifier.ToString()));
                }
                
                currParameter++;
            }
            else
            {
                arguments[i] = Token(SyntaxKind.CommaToken);
            }
        }

        return arguments;
    }
}