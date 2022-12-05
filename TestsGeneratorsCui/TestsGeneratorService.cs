using System.Net.Sockets;
using System.Security;
using System.Threading.Tasks.Dataflow;
using TestsGeneratorCore;

namespace TestsGeneratorsCui;

/// <summary>
/// Reads, generates and writes generated tests.
/// </summary>
public class TestsGeneratorService
{
    private readonly TestsGenerator _testsGenerator = new();

    private readonly TransformBlock<string, string> _readerBlock;
    private readonly TransformManyBlock<string, string> _generatorBlock;
    private readonly ActionBlock<string> _writerBlock;
    
    /// <summary>
    /// Constructor for service
    /// </summary>
    /// <param name="degreeOfParallelismRead">degree of parallelism for input files reading</param>
    /// <param name="degreeOfParallelismGenerate">degree of parallelism for tests generation</param>
    /// <param name="degreeOfParallelismWrite">>degree of parallelism for writes tests to output files</param>
    /// <param name="outputDirectory">directory with generated tests.</param>
    public TestsGeneratorService(int degreeOfParallelismRead, int degreeOfParallelismGenerate,
        int degreeOfParallelismWrite, string outputDirectory)
    {
        _readerBlock = InitReadBlock(degreeOfParallelismRead);
        _generatorBlock = InitGenerateBlock(degreeOfParallelismGenerate);
        _writerBlock = InitWriteBlock(degreeOfParallelismWrite, outputDirectory);

        _readerBlock.LinkTo(_generatorBlock, new DataflowLinkOptions { PropagateCompletion = true });
        _generatorBlock.LinkTo(_writerBlock, new DataflowLinkOptions { PropagateCompletion = true });
    }

    /// <summary>
    /// Launch tests generation.
    /// </summary>
    /// <param name="fileNames">Input files with classes</param>
    public async Task Generate(List<string> fileNames)
    {
        foreach (var fileName in fileNames)
        {
            _readerBlock.Post(fileName);
        }
        
        _readerBlock.Complete();
        await _writerBlock.Completion;
    }

    private TransformBlock<string, string> InitReadBlock(int degreeOfParallelism)
    {
        return new TransformBlock<string, string>(async filename =>
        {
            using var reader = File.OpenText(filename);
            return await reader.ReadToEndAsync();
        }, new ExecutionDataflowBlockOptions { MaxDegreeOfParallelism = degreeOfParallelism });
    }

    private TransformManyBlock<string, string> InitGenerateBlock(int degreeOfParallelism)
    {
        return new TransformManyBlock<string, string>(source => _testsGenerator.Generate(),
            new ExecutionDataflowBlockOptions { MaxDegreeOfParallelism = degreeOfParallelism });
    }

    private ActionBlock<string> InitWriteBlock(int degreeOfParallelism, string outputDirectory)
    {
        return new ActionBlock<string>(async classInfo =>
        {
            await using var writer = new StreamWriter($"{outputDirectory}\\test.txt");
            await writer.WriteAsync(classInfo);
        }, new ExecutionDataflowBlockOptions { MaxDegreeOfParallelism = degreeOfParallelism });
    }
}