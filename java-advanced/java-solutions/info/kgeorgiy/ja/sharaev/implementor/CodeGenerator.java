package info.kgeorgiy.ja.sharaev.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Generate implementation by class token
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 * @version 11
 */
public class CodeGenerator {

    /**
     * Type token to create implementation
     */
    private final Class<?> token;

    /**
     * Implementation storage
     */
    private final StringBuilder sb;
    /**
     * Suffix to add class name implementation
     */
    private final String implSuffix;

    /**
     * Create CodeGenerator
     *
     * @param token      type token to create implementation for.
     * @param implSuffix suffix to add generated implementation
     * @throws ImplerException if token is not interface or token is private
     */
    public CodeGenerator(final Class<?> token, final String implSuffix) throws ImplerException {
        if (!token.isInterface()) {
            throw new ImplerException("support only interfaces");
        } else if (Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Can't implement private interface");
        }
        this.token = token;
        this.sb = new StringBuilder();
        this.implSuffix = implSuffix;
    }


    /**
     * Create implementation. Methods have body with default values
     *
     * @return implementation
     * @throws ImplerException if some method have private class on arguments, exceptions or return
     */
    public String generateImplementation() throws ImplerException {
        generatePackage();
        generateTitle();
        sb.append(" {");
        generateNewLine();
        generateMethods();
        sb.append("}");
        return sb.toString();
    }

    /**
     * Generate package if exist
     */
    private void generatePackage() {
        if (!token.getPackageName().equals("")) {
            sb.append("package ");
            sb.append(token.getPackageName());
            sb.append(";");
            generateNewLine();
            generateNewLine();
        }
    }

    /**
     * Generate class declaration. Name with added {@link #implSuffix} and implement token.
     * Title without opening curly brace on the end
     */
    private void generateTitle() {
        final String className = token.getSimpleName() + implSuffix;
        sb.append(String.format("public class %s implements %s", className, token.getCanonicalName()));
    }

    /**
     * Generate methods declaration. Methods have default values
     *
     * @throws ImplerException if some method have private class on arguments, exceptions or return
     */
    private void generateMethods() throws ImplerException {
        for (final Method method : token.getMethods()) {
            generateNewLine();
            sb.append("\t");
            sb.append(getSignature(method));
            sb.append(" ");
            generateMethodBody(method.getReturnType());
            generateNewLine();
        }
    }

    /**
     * Generate method signature. Signature without opening curly brace on the end
     *
     * @param method implementing method
     * @return implemented method
     * @throws ImplerException if method have private class on arguments, exceptions or return
     */
    private static String getSignature(final Method method) throws ImplerException {
        checkNonPrivate(Stream.of(method.getReturnType()), "Return type", method);
        checkNonPrivate(Stream.of(method.getParameters()).map(Parameter::getType),
                "Argument", method);
        checkNonPrivate(Stream.of(method.getExceptionTypes()), "Exception", method);

        final int modifiers = method.getModifiers() & ~Modifier.ABSTRACT & ~Modifier.TRANSIENT;
        final String modifications = Modifier.toString(modifiers) + " "
                + method.getReturnType().getCanonicalName() + " "
                + method.getName();

        final String arguments = Arrays.stream(method.getParameters())
                .map(parameter -> parameter.getType().getCanonicalName() + " " + parameter.getName())
                .collect(Collectors.joining(", ", "(", ")"));

        final String exceptions;
        if (method.getExceptionTypes().length != 0) {
            exceptions = Arrays.stream(method.getExceptionTypes())
                    .map(Class::getCanonicalName)
                    .collect(Collectors.joining(", ", " throws ", ""));
        } else {
            exceptions = "";
        }
        return modifications + arguments + exceptions;
    }

    /**
     * Check private and throw if some is private
     *
     * @param types    checked types
     * @param nameType name checked types
     * @param method   method who contains this types
     * @throws ImplerException if some type is private
     */
    private static void checkNonPrivate(final Stream<Class<?>> types, final String nameType, final Method method)
            throws ImplerException {
        for (final Class<?> t : types.toList()) {
            if (Modifier.isPrivate(t.getModifiers())) {
                throw new ImplerException(nameType + " " + t.getName() + " is private in method " + method.getName());
            }
        }
    }

    /**
     * Generate method body with default value
     *
     * @param returnValue returned value on method
     */
    private void generateMethodBody(final Class<?> returnValue) {
        sb.append("{");
        generateNewLine();
        sb.append("\t\t");
        generateReturnStatement(returnValue);
        generateNewLine();
        sb.append("\t}");
    }

    /**
     * Generate return statement with default value
     *
     * @param returnValue returned value on method
     */
    private void generateReturnStatement(final Class<?> returnValue) {
        if (returnValue == void.class) {
            return;
        }

        sb.append("return ");
        if (returnValue == boolean.class) {
            sb.append("false");
        } else if (returnValue.isPrimitive()) {
            sb.append("0");
        } else {
            sb.append("null");
        }
        sb.append(";");
    }


    /**
     * Generate new line by {@link System#lineSeparator()}
     */
    private void generateNewLine() {
        sb.append(System.lineSeparator());
    }
}
