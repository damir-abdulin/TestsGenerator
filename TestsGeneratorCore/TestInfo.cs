namespace TestsGeneratorCore;

/// <summary>
/// Stores info about generated test.
/// </summary>
public struct TestInfo
{
    public string Name { get; }
    public string Source { get; }

    public TestInfo(string sourceClassName, string source)
    {
        Name = sourceClassName + "Test";
        Source = source;
    }
}