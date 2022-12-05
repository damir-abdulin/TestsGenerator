namespace TestsGeneratorsCui;

public static class Program
{
    public static async Task Main(string[] args)
    {
        if (!CheckArgsCount(args.Length)) return;

        var inputFiles = GetInputFiles(args[0]);
        var outputDirectory = args[1];

        var degreeOfParallelismRead = GetDegreesOfParallelism(args[2]);
        if (degreeOfParallelismRead == -1)
        {
            Console.WriteLine($"Invalid degree of parallelism READ. Expected integer, got {args[2]}");
            return;
        }
        
        var degreeOfParallelismGenerate = GetDegreesOfParallelism(args[2]);
        if (degreeOfParallelismGenerate == -1)
        {
            Console.WriteLine($"Invalid degree of parallelism GENERATE. Expected integer, got {args[3]}");
            return;
        }
        
        var degreeOfParallelismWrite = GetDegreesOfParallelism(args[2]);
        if (degreeOfParallelismWrite == -1)
        {
            Console.WriteLine($"Invalid degree of parallelism WRITE. Expected integer, got {args[4]}");
            return;
        }

        var testsGeneratorService = new TestsGeneratorService(degreeOfParallelismRead, degreeOfParallelismGenerate,
            degreeOfParallelismWrite, outputDirectory);
        await testsGeneratorService.Generate(inputFiles);
    }

    private static bool CheckArgsCount(int argsLength)
    {
        if (argsLength == 5) return true;
        
        Console.WriteLine("[ERROR] Not enough arguments." +
                          "Usage: <input files separated with '|'> <output directory> " +
                          "<degree of parallelism READ> <degree of parallelism GENERATE>" +
                          "<degree of parallelism WRITE>");
        return false;

    }
    private static List<string> GetInputFiles(string inputFiles)
    {
        var userInputFiles = inputFiles.Split('|');
        var correctInputFiles = new List<string>();
        foreach (var inputFile in userInputFiles)
        {
            if (File.Exists(inputFile))
            {
                correctInputFiles.Add(inputFile);
            }
            else
            {
                Console.WriteLine($"File '{inputFile}' doesn't exist or you don't have permissions to open it. " +
                                  "It would be removed from generating");
            }
        }

        return correctInputFiles;
    }
    private static int GetDegreesOfParallelism(string arg)
    {
        if (int.TryParse(arg, out var degreeOfParallelism))
            return degreeOfParallelism;
        
        return -1;
    }
}