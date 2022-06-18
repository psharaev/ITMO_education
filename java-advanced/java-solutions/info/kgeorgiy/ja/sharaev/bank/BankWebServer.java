package info.kgeorgiy.ja.sharaev.bank;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Simple HTTP server example.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class BankWebServer {
    private static final String BUTTONS_TEMPLATE = "" +
            "<div>" +
            "  <label>{action}</label>" +
            "  {buttons}" +
            "</div>";
    private static final String BUTTON_TEMPLATE = "<input type='submit' name='{action}' value='{name}'>";

    private static final String HTML_TEMPLATE = "" +
            "<html>" +
            "  <head>" +
            "    <meta charset='UTF-8'>" +
            "    <style>" +
            "      html, body {height: 100%; margin: 0; padding: 0;}" +
            "      body {display: flex; flex-flow: column; }" +
            "      body > * {flex: 0 1 auto; margin: 1ex; } " +
            "      label {min-width: 3em; display: inline-block;}" +
            "      #log-table { color: lightgray; }" +
            "      #log {" +
            "        flex: 1 1 auto; overflow-y: scroll; " +
            "        display: flex; flex-direction: column-reverse;" +
            "        font-family: monospace; " +
            "        background: black; " +
            "      }" +
            "      td { white-space: nowrap; vertical-align: top; }" +
            "      td:last-child { width: 100%; }" +
            "      td.output { color: white; }" +
            "      td.info   { color: white; }" +
            "      td.error  { color: red;   }" +
            "      td.Client    { color: lightgreen; }" +
            "      td.Server    { color: lightblue; }" +
            "      td.Registry  { color: yellow;   }" +
            "    </style>" +
            "  </head>" +
            "  <body>" +
            "    <h1>Bank</h1>" +
            "    <form method='POST'>" +
            "      {start}" +
            "      {stop}" +
            "    </form>" +
            "    <h3>Log</h3>" +
            "    <div id='log'><table id='log-table'>{log}</table></div>" +
            "    <script>" +
            "      const log = document.getElementById('log-table');" +
            "      function refresh() {" +
            "        const request = new XMLHttpRequest();" +
            "        request.onreadystatechange = function(e) {" +
            "          console.log(this.readyState + ' ' + this.status);" +
            "          if (this.readyState == 4 && this.status == 200) {" +
            "            log.innerHTML = this.responseText;" +
            "          }" +
            "        };" +
            "        request.open('GET', 'bank/log');" +
            "        request.send();" +
            "      }" +
            "      setInterval(refresh, 300);" +
            "    </script>" +
            "  </body>" +
            "</html>";
    private static final String LOG_ENTRY_TEMPLATE = "" +
            "<tr>" +
//            "  <td>{time}</td>" +
//            "  <td>{level}</td>" +
            "  <td class='{app}'>{app}</td>" +
            "  <td class='{level}'>{message}</td>" +
            "</tr>";
    private static final ThreadLocal<SimpleDateFormat> DATE_FORMAT =
            ThreadLocal.withInitial(() -> new SimpleDateFormat("HH:mm:ss"));
    private static final String CLASSPATH = getClassPath();
    private static final String LOCATION = "/bank";
    private static final String ACTION_START = "Start";
    private static final String ACTION_STOP = "Stop";

    private static final String PACKAGE = BankWebServer.class.getPackageName();
    private final Map<String, App> apps = Map.ofEntries(
            app("Client 1", "java", "-cp", CLASSPATH, PACKAGE + ".Client", "client-1"),
            app("Client 2", "java", "-cp", CLASSPATH, PACKAGE + ".Client", "client-2"),
            app("Client 3", "java", "-cp", CLASSPATH, PACKAGE + ".Client", "client-3"),
            app("Server 1", "java", "-cp", CLASSPATH, PACKAGE + ".Server", "8881"),
            app("Server 2", "java", "-cp", CLASSPATH, PACKAGE + ".Server", "8882"),
            app("Registry", "rmiregistry"),
            app("Registry CP", "rmiregistry", "-J--class-path=" + CLASSPATH)
    );

    private final List<LogEntry> logs = new ArrayList<>();
    private final Log log = new Log("Web");

    public static void main(final String[] args) throws IOException {
        new BankWebServer().start(args.length > 0 ? Integer.parseInt(args[0]) : 8088);
    }

    private Map.Entry<String, App> app(final String name, final String... command) {
        return Map.entry(name, new App(name, new Log(name), command));
    }

    private void start(final int port) throws IOException {
        System.err.println(CLASSPATH);
        final HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext(LOCATION, exchange -> {
            if (!"GET".equals(exchange.getRequestMethod())) {
                execute(getParams(exchange));
                exchange.getResponseHeaders().set("Location", LOCATION);
                exchange.sendResponseHeaders(HttpURLConnection.HTTP_SEE_OTHER, 0);
                return;
            }

            send(exchange, template(HTML_TEMPLATE, Map.of(
                    "start", buttons(ACTION_START),
                    "stop", buttons(ACTION_STOP),
                    "log", getLog()
            )));
        });
        server.createContext(LOCATION + "/log", exchange -> send(exchange, getLog()));
        server.start();
        log.log(LogLevel.INFO, "Started on port " + port);
        log.log(LogLevel.INFO, "URL: http://localhost:" + port + "/bank");
    }

    private String getLog() {
        synchronized (logs) {
            return logs.stream().map(entry -> entry.html).collect(Collectors.joining());
        }
    }

    private static void send(final HttpExchange exchange, final String responseText) throws IOException {
        final byte[] response = responseText.getBytes(StandardCharsets.UTF_8);
        exchange.sendResponseHeaders(HttpURLConnection.HTTP_OK, response.length);
        exchange.getResponseHeaders().set("Content-Type", "text/html; charset=UTF-8");
        try (final OutputStream os = exchange.getResponseBody()) {
            os.write(response);
        }
    }

    private void execute(final Map<String, List<String>> params) {
        execute(params, ACTION_START, App::start);
        execute(params, ACTION_STOP, App::stop);
    }

    private void execute(final Map<String, List<String>> params, final String key, final Consumer<App> action) {
        try {
            params.getOrDefault(key, List.of()).stream().flatMap(this::getApp).forEach(action);
        } catch (final Exception e) {
            log.error(LogLevel.ERROR, e);
        }
    }

    private Stream<App> getApp(final String key) {
        final App app = apps.get(key);
        if (app == null) {
            log.log(LogLevel.ERROR, "Unknown app " + key);
        }
        return Stream.ofNullable(app);
    }

    private String buttons(final String action) {
        return template(BUTTONS_TEMPLATE, Map.of(
                "action", action,
                "buttons", apps.keySet().stream().sorted().map(name -> template(BUTTON_TEMPLATE, Map.of(
                        "action", action,
                        "name", name
                ))).collect(Collectors.joining())
        ));
    }

    private static String template(String template, final Map<String, String> params) {
        for (final Map.Entry<String, String> entry : params.entrySet()) {
            template = template.replace("{" + entry.getKey() + "}", entry.getValue());
        }
        return template;
    }

    private static String getClassPath() {
        try {
            return Path.of(BankWebServer.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new AssertionError(e);
        }
    }

    private static LinkedHashMap<String, List<String>> getParams(final HttpExchange t) throws IOException {
        final String params;
        if ("GET".equals(t.getRequestMethod())) {
            params = t.getRequestURI().getQuery();
        } else {
            try (final InputStream is = t.getRequestBody()) {
                params = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8)).readLine();
            }
        }

        return splitParams(Objects.requireNonNullElse(params, ""));
    }

    /**
     * Quick-and-dirty solution due to no dependencies in examples.
     * Better use Apache <a href="https://www.javadoc.io/doc/org.apache.httpcomponents/httpclient/4.5.1/org/apache/http/client/utils/URLEncodedUtils.html">URLEncodedUtils</a>
     * or Spring <a href="https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/util/UriComponentsBuilder.html">UriComponentsBuilder</a>.
     */
    private static LinkedHashMap<String, List<String>> splitParams(final String params) {
        return Arrays.stream(params.split("&"))
                .map(param -> Arrays.stream(param.split("=", 2))
                        .map(v -> URLDecoder.decode(v, StandardCharsets.UTF_8))
                        .toArray(String[]::new))
                .collect(Collectors.groupingBy(
                        kv -> kv[0],
                        LinkedHashMap::new,
                        Collectors.mapping(kv -> kv.length == 2 ? kv[1] : null, Collectors.toList())
                ));
    }

    private static final Map<Integer, String> HTML_ESCAPES = Map.of(
            '"', "&quot;",
            '\'', "&apos;",
            '<', "&lt;",
            '>', "&gt;",
            '&', "&amp;"
    ).entrySet().stream().collect(Collectors.toMap(e -> (int) e.getKey(), Map.Entry::getValue));

    /**
     * Another quick-and-dirty solution due to no dependencies in examples.
     * Better use Apache <a href="https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringEscapeUtils.html">StringEscapeUtils</a>
     * or Spring <a href="https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/util/HtmlUtils.html">HtmlUtils</a>.
     */
    private static String escapeHTML(final String value) {
        return value.chars()
                .mapToObj(c -> HTML_ESCAPES.getOrDefault(c, Character.toString(c)))
                .collect(Collectors.joining());
    }

    private static class App {
        private final String name;
        private final ProcessBuilder processBuilder;
        private Process process;
        private final Log log;

        public App(final String name, final Log log, final String... command) {
            this.name = name;
            this.log = log;
            processBuilder = new ProcessBuilder(command);
            processBuilder.environment().put("CLASSPATH", CLASSPATH);
        }

        public String getName() {
            return name;
        }

        public void start() {
            if (process != null && process.isAlive()) {
                logPid(LogLevel.ERROR, "Already running");
                return;
            }

            process = null;

            try {
                process = processBuilder.start();
                handle(LogLevel.OUTPUT, process.getInputStream());
                handle(LogLevel.ERROR, process.getErrorStream());
                logPid(LogLevel.INFO, "Started process");
            } catch (final IOException e) {
                log.error(LogLevel.ERROR, e);
            }
        }

        public void stop() {
            if (process == null) {
                log.log(name, LogLevel.INFO, "No process");
                return;
            }

            process.destroy();
            try {
                if (process.waitFor(1, TimeUnit.SECONDS)) {
                    logPid(LogLevel.INFO, "Stopped process");
                    process = null;
                } else {
                    logPid(LogLevel.ERROR, "Process still alive after 1s");
                }
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }

        private void logPid(final LogLevel level, final String message) {
            log.log(name, level, message + ", pid=" + process.pid());
        }

        private void handle(final LogLevel level, final InputStream stream) {
            new Thread(() -> {
                // Using default encoding
                try (final BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
                    while (true) {
                        final String line = reader.readLine();
                        if (line == null) {
                            return;
                        }
                        log.log(name, level, line.replace("\t", "\u00a0\u00a0\u00a0\u00a0"));
                    }
                } catch (final IOException e) {
                    log.error(LogLevel.ERROR, e);
                }
            }).start();
        }
    }

    private enum LogLevel {
        OUTPUT,
        INFO,
        ERROR
    }

    private static class LogEntry {
        private final String html;

        public LogEntry(final String app, final LogLevel level, final String message) {
            this.html = template(LOG_ENTRY_TEMPLATE, Map.of(
                    "time", DATE_FORMAT.get().format(new Date()),
                    "app", app,
                    "level", level.name().toLowerCase(),
                    "message", escapeHTML(message)
            ));
        }

        public String html() {
            return html;
        }
    }

    private class Log {
        private final String name;

        public Log(final String name) {
            this.name = name;
        }

        public void log(final LogLevel level, final String message) {
            log(name, level, message);
        }

        private void log(final String app, final LogLevel level, final String message) {
            synchronized (logs) {
                logs.add(new LogEntry(app, level, message));
            }
            System.out.format("%11s %7s %s%n", app, level, message);
        }

        public void error(final LogLevel level, final Throwable e) {
            log(level, e.getClass().getSimpleName() + ": " + e.getMessage());
        }
    }
}
