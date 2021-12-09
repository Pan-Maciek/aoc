using System.Text.RegularExpressions;

if (args.Length != 1)
{
    Console.Error.WriteLine("Usage dotnet run <input>");
    Environment.Exit(1);
}

try
{
    var total = File.ReadLines(args[0])
        .AsParallel()
        .Sum(line =>
        {
            var (examples, output) = InputParser.ParseLine(line);
            return DisplayMapping.FromExamples(examples).NumberFromDigits(output);
        });
    Console.WriteLine(total);
}
catch (Exception e)
{
    Console.WriteLine(e.Message);
    Environment.Exit(1);
}

public static class InputParser
{
    private static readonly Regex SegmentSplitRegex = new(@"\s+", RegexOptions.Compiled);
    public static (IEnumerable<int> examples, IEnumerable<int> output) ParseLine(string line)
    {
        var groups = line.Split(" | ");
        return (
            SegmentSplitRegex.Split(groups[0]).Select(ParseSegments),
            SegmentSplitRegex.Split(groups[1]).Select(ParseSegments)
        );
    }
    public static int ParseSegments(string segments) => segments.Aggregate(0, (current, segment) => current | 1 << (segment - 'a'));
}

public class DisplayMapping
{
    private readonly int[] _digits;
    private DisplayMapping(int[] orderedDigits)
    {
        _digits = orderedDigits;
    }

    private int Translate(int digit) => Array.IndexOf(_digits, digit);

    public int NumberFromDigits(IEnumerable<int> digits) => digits.Aggregate(0, (current, digit) => current * 10 + Translate(digit));

    public static DisplayMapping FromExamples(IEnumerable<int> examples)
    {
        var groups  = examples.GroupBy(CountOnes).ToDictionary(x => x.Key);

        var one = groups[2].First();
        var four = groups[4].First();
        var seven = groups[3].First();
        var eight = groups[7].First();

        var len6 = groups[6].ToList(); // 0, 6, 9
        var len5 = groups[5].ToList(); // 2, 3, 5

        var nine = len6.First(x => (x & four) == four);
        len6.Remove(nine); // 0, 6

        var six = len6.First(x => (x | one) == eight);
        len6.Remove(six); // 0

        var zero = len6[0];

        var three = len5.First(x => (x & one) == one);
        len5.Remove(three); // 2, 5

        var five = len5.Find(x => (x | one) == nine);
        len5.Remove(five); // 2

        var two = len5[0];

        return new DisplayMapping(new[]
        {
            zero, one, two, three, four, five, six, seven, eight, nine
        });
    }

    private static int CountOnes(int value)
    {
        var count = 0;
        while (value > 0)
        {
            count += value & 1;
            value >>= 1;
        }
        return count;
    }
}