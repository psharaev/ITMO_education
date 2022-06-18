package info.kgeorgiy.ja.sharaev.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.Objects;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;

/**
 * Implementation of {@link JarImpler} interface
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Implementor implements JarImpler {

    /**
     * Recursive delete files and directories based {@link SimpleFileVisitor}
     */
    private static final SimpleFileVisitor<Path> DELETE_VISITOR = new SimpleFileVisitor<>() {
        /**
         * Delete file
         *
         * @param file current file
         * @param attrs attributes of file
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if failed delete file
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Delete directory
         *
         * @param dir current post visit directory
         * @param exc null if the post directory without an error. Otherwise, the {@link IOException} that caused
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if failed delete directory
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };

    /**
     * Suffix for add to generated class name
     */
    private static final String IMPL_SUFFIX = "Impl";

    /**
     * Generate implementation
     * <ul>
     *     <li> use {@link #implement(Class, Path)} first arg: className, second arg: output directory</li>
     *     <li> use {@link #implementJar(Class, Path)} first arg: -jar, second argument: className, third arg: path to jarFile</li>
     * </ul>
     *
     * @param args arguments for generate implementation
     */
    public static void main(final String[] args) {
        if (!checkArgs(args)) {
            System.err.printf("Usage: java %s [-jar] <Class name> <Target path>", Implementor.class.getSimpleName());
            System.err.println();
            return;
        }

        final Implementor implementor = new Implementor();
        try {
            if (args.length == 3) {
                implementor.implementJar(Class.forName(args[1]), Path.of(args[2]));
            } else {
                implementor.implement(Class.forName(args[0]), Path.of(args[1]));
            }
        } catch (final ClassNotFoundException e) {
            System.err.println("Class not found");
        } catch (final ImplerException e) {
            System.err.println("Fail implement this class");
        }
    }

    /**
     * Check input arguments
     *
     * @param args arguments for start main
     * @return true if arguments correct
     */
    private static boolean checkArgs(final String[] args) {
        if (args == null) {
            return false;
        }
        if (args.length != 2 && args.length != 3) {
            return false;
        }
        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            return false;
        }

        return args.length != 3 || "-jar".equals(args[0]);
    }

    /**
     * Create <var>.jar</var> file. Implements class by {@link #implement(Class, Path)}
     * in temp directory nearby of the generated file. Compile it and pack to the .jar file
     *
     * @param token   type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException if the given class cannot be implemented, causes:
     *                         <ul>
     *                             <li>Token is not interface</li>
     *                             <li>Some method have private class on arguments, exceptions or return</li>
     *                             <li>IO exception</li>
     *                             <li>Fail compile implementation</li>
     *                         </ul>
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        final Path tempDir;
        try {
            final Path parent = jarFile.toAbsolutePath().getParent();
            if (parent == null) {
                throw new ImplerException("parent of jar file is null");
            }
            tempDir = Files.createTempDirectory(parent, "temp");
        } catch (final IOException e) {
            throw new ImplerException("Failed create temp directory");
        }

        implement(token, tempDir);
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("Compiler not found");
        }
        final String[] args = new String[]{
                "-cp",
                getClassPath(token),
                getJavaFilePath(token, tempDir).toString()
        };
        if (compiler.run(null, null, null, args) != 0) {
            throw new ImplerException("Fail compile generated code");
        }
        final Manifest manifest = new Manifest();
        final Attributes attributes = manifest.getMainAttributes();
        attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");
        try (final JarOutputStream writer = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
            final String className;
            if (token.getPackageName().equals("")) {
                className = token.getSimpleName();
            } else {
                className = token.getPackageName().replace('.', '/') + "/" + token.getSimpleName();
            }
            writer.putNextEntry(new ZipEntry(className + IMPL_SUFFIX + ".class"));
            final Path javaFile = resolvePackage(token, tempDir).resolve(token.getSimpleName() + IMPL_SUFFIX + ".class");
            Files.copy(javaFile, writer);
        } catch (final IOException e) {
            throw new ImplerException("Fail write JAR file", e);
        } finally {
            try {
                recursivelyCleanDirectory(tempDir);
            } catch (final IOException e) {
                System.err.println("Failed clean temp directory: " + e.getMessage());
            }
        }
    }

    /**
     * String with unicode characters
     *
     * @param input input string
     * @return String with unicode characters
     */
    private String convertToUnicode(final String input) {
        final StringBuilder res = new StringBuilder();
        for (final char c : input.toCharArray()) {
            if (c >= 128) {
                res.append(String.format("\\u%04X", (int) c));
            } else {
                res.append(c);
            }
        }
        return res.toString();
    }

    /**
     * Generate implementation by token and put this to root
     *
     * @param token type token to create implementation for.
     * @param root  root directory.
     * @throws ImplerException if the given class cannot be implemented, causes:
     *                         <ul>
     *                             <li>Token is not interface</li>
     *                             <li>Some method have private class on arguments, exceptions or return</li>
     *                             <li>IO exception</li>
     *                             <li>Fail compile implementation</li>
     *                         </ul>
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        final Path filePath = createJavaFilePath(token, root);

        try (final BufferedWriter writer = Files.newBufferedWriter(filePath)) {
            final CodeGenerator codeGenerator = new CodeGenerator(token, IMPL_SUFFIX);
            final String clazz = codeGenerator.generateImplementation();
            writer.write(convertToUnicode(clazz));
        } catch (final IOException e) {
            throw new ImplerException("Fail write generated implementation", e);
        }
    }

    /**
     * Returns class path to .jar file with {@link JarImpler} class.
     *
     * @param token token for get class path
     * @return class path to .jar file
     * @throws ImplerException if failed get class path for token
     */
    private static String getClassPath(final Class<?> token) throws ImplerException {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new ImplerException("Failed get class path for token", e);
        }
    }

    /**
     * Package path relative to root
     *
     * @param token type token to create implementation for.
     * @param root  directory
     * @return root + packagePath
     */
    private static Path resolvePackage(final Class<?> token, final Path root) {
        return root.resolve(token.getPackageName().replace('.', File.separatorChar));
    }

    /**
     * Java file implementation relative to root
     *
     * @param token type token to create implementation for.
     * @param root  directory
     * @return root + package + name java file
     */
    private static Path getJavaFilePath(final Class<?> token, final Path root) {
        return resolvePackage(token, root).resolve(token.getSimpleName() + IMPL_SUFFIX + ".java");
    }

    /**
     * Create path to java file implementation relative to root
     *
     * @param token type token to create implementation for.
     * @param root  directory
     * @return root + package + name java file
     */
    private static Path createJavaFilePath(final Class<?> token, final Path root) {
        final Path javaFilePath = getJavaFilePath(token, root);
        try {
            Files.createDirectories(javaFilePath.getParent());
        } catch (final IOException e) {
            System.err.println("Fail create directories");
        }
        return javaFilePath;
    }

    /**
     * Recursively deletes files and directories in root
     *
     * @param root directory for recursively deletes
     * @throws IOException if error recursively delete
     */
    private static void recursivelyCleanDirectory(final Path root) throws IOException {
        if (Files.exists(root)) {
            Files.walkFileTree(root, DELETE_VISITOR);
        }
    }
}