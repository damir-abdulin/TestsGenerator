using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace TestsGeneratorCore;

/// <summary>
/// Describe class which generates members for testing class.
/// </summary>
internal abstract class MemberGenerator
{
    public string ObjName { get; }
    protected ClassDeclarationSyntax ClassDeclarationSyntax { get; }
    protected string Attribute { get; }

    protected MemberGenerator(ClassDeclarationSyntax classDeclarationSyntax, string attribute = "")
    {
        ObjName = GenerateObjName(classDeclarationSyntax.Identifier.ToString());
        ClassDeclarationSyntax = classDeclarationSyntax;
        Attribute = attribute;
    }
    
    protected static IEnumerable<SyntaxNodeOrToken> GetArgumentsForFunction
        (SeparatedSyntaxList<ParameterSyntax> parameters)
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
    
    private static string GenerateObjName(string typeName)
    {
        var result = char.ToLowerInvariant(typeName[0]) + typeName[1..];
        return "_" + result;
    }
}