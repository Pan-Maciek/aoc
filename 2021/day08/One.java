
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;

public class One {

    public static void main(String[] args) {

        if (args.length != 1) {
            System.err.println("Usage: java one.java <input>");
            System.exit(1);
        }

        var fileName = args[0];

        try {
            var count = Files.lines(Path.of(fileName), StandardCharsets.UTF_8).map(One::getOutputValuesCount)
                    .reduce(Long::sum);

            System.out.println(count.get());
        } catch (IOException ex) {
            System.err.println(ex);
            System.exit(1);
        }

    }

    public static long getOutputValuesCount(String line) {
        var output = line.split(" \\| ")[1];
        return Arrays.stream(output.split("\\s+")).map(BitSet::fromString).filter(BitSet::isKnown).count();
    }

}

record BitSet(int value) {

    public static BitSet fromString(String letterSet) {
        var value = letterSet.chars().map(BitSet::LetterToSegment).reduce((a, b) -> a | b);
        return value.isPresent() ? new BitSet(value.getAsInt()) : null;
    }

    public int countOnes() {
        int count = 0;
        int x = value;
        while (x > 0) {
            count += x & 1;
            x = x >> 1;
        }
        return count;
    }

    public static int LetterToSegment(int letter) {
        return 1 << (letter - 'a');
    }

    private static boolean[] knownValues = new boolean[] {
            false, false,
            true, // 2 segments - One
            true, // 3 segments - Seven
            true, // 4 segments - Four
            false,
            false,
            true // 7 segments - Eight
    };

    public boolean isKnown() {
        return knownValues[countOnes()];
    }
}
