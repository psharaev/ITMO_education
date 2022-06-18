package info.kgeorgiy.ja.sharaev.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;

public class Walk {

    public static void main(final String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Run program with two arguments, example: java Walk <input file> <output file>");
            return;
        }

        try {
            walk(args[0], args[1]);
        } catch (final WalkException e) {
            System.err.println(e.getMessage());
        }
    }

    public static void walk(final String input, final String output) throws WalkException {
        final Path inputPath = getPath(input, "Incorrect input path");
        final Path outputPath = getPath(output, "Incorrect output path");

        walk(inputPath, outputPath);
    }

    public static void walk(final Path input, final Path output) throws WalkException {
        final Sha1Hasher hasher;
        try {
            hasher = new Sha1Hasher();
        } catch (final CalcHashException e) {
            throw new WalkException("SHA-1 algorithm not found", e);
        }

        try {
            final Path outputPathParent = output.getParent();
            if (outputPathParent != null) {
                Files.createDirectories(outputPathParent);
            }
        } catch (final IOException | SecurityException e) {
            System.err.println("Failed create parent path for output file: " + e.getMessage());
        }

        try (final BufferedWriter writer = Files.newBufferedWriter(output)) {
            try (final BufferedReader reader = Files.newBufferedReader(input)) {
                String filePath;
                while ((filePath = readLine(reader)) != null) {
                    byte[] hashResult = null;
                    try {
                        hashResult = hasher.getSha1File(filePath);
                    } catch (final CalcHashException ignored) {
                    }

                    try {
                        writer.write(hasher.encodeSha1(hashResult));
                        writer.write(' ');
                        writer.write(filePath);
                        writer.newLine();
                    } catch (final IOException e) {
                        throw new WalkException("Failed write output file", e);
                    }
                }
            } catch (final IOException e) {
                throw new WalkException("Failed open input file", e);
            }
        } catch (final IOException e) {
            throw new WalkException("Failed opening or creating output file", e);
        }
    }

    private static String readLine(final BufferedReader reader) throws WalkException {
        try {
            return reader.readLine();
        } catch (final IOException e) {
            throw new WalkException("Failed read input file", e);
        }
    }

    private static Path getPath(final String path, final String errorMessage) throws WalkException {
        try {
            return Path.of(path);
        } catch (final InvalidPathException e) {
            throw new WalkException(errorMessage + ": " + path, e);
        }
    }
}
