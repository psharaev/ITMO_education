package info.kgeorgiy.ja.sharaev.walk;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Sha1Hasher {

    private static final String FAILED_SHA1 = "0".repeat(40);

    private final MessageDigest sha1Digest;
    private final byte[] sha1Buffer = new byte[8192];


    public Sha1Hasher() throws CalcHashException {
        try {
            sha1Digest = MessageDigest.getInstance("SHA-1");
        } catch (NoSuchAlgorithmException e) {
            throw new CalcHashException("SHA-1 not found", e);
        }
    }

    public byte[] getSha1File(final String filePath) throws CalcHashException {
        try (final InputStream inputStream = Files.newInputStream(Path.of(filePath))) {
            return getSha1(inputStream);
        } catch (final IOException e) {
            throw new CalcHashException("Failed open input file", e);
        } catch (final InvalidPathException e) {
            throw new CalcHashException("Invalid input path", e);
        }
    }

    public byte[] getSha1(final InputStream inputStream) throws CalcHashException {
        try {
            sha1Digest.reset();
            int count;
            while ((count = inputStream.read(sha1Buffer)) >= 0) {
                sha1Digest.update(sha1Buffer, 0, count);
            }
            return sha1Digest.digest();
        } catch (final IOException e) {
            throw new CalcHashException("Failed read input stream", e);
        }
    }

    public String encodeSha1(final byte[] byteArray) {
        if (byteArray == null) {
            return FAILED_SHA1;
        }

        final StringBuilder stringBuilder = new StringBuilder(40);
        for (final byte b : byteArray) {
            stringBuilder.append(String.format("%02x", b));
        }

        return stringBuilder.toString();
    }
}
