namespace TestsGeneratorCore;

public static class TestsGenerator
{
    private const int MaxInputFiles = 5;
    private const int MaxOutputFiles = 5;
    
    public static void GenerateTests(string inputDirectory, string outputDirectory, params string[] fileNames)
    {
        var inputFiles = GetFullFilesPath(inputDirectory, fileNames);
        LoadFilesAsync(inputFiles);
        
        GenerateTestsAsync();
        
        var outputFiles = GetFullFilesPath(outputDirectory, fileNames);
        WriteFilesAsync(outputFiles);
    }

    private static string[] GetFullFilesPath(string directory, params string[] files)
    {
        return files.Select(filename => $"{directory}\\{filename}").ToArray();
    }

    private static void LoadFile(string filePath)
    {
        throw new NotImplementedException();
    }
    private static void GenerateTest(string file)
    {
        throw new NotImplementedException();
    }
    private static void WriteFile(string file)
    {
        throw new NotImplementedException();
    }

    private static void LoadFilesAsync(params string[] filesPaths)
    {
        throw new NotImplementedException();
    }

    private static void GenerateTestsAsync(params string[] files)
    {
        throw new NotImplementedException();
    }
    
    private static void WriteFilesAsync(params string[] files)
    {
        throw new NotImplementedException();
    }
}