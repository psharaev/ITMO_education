package info.kgeorgiy.ja.sharaev.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;

/**
 * Implementation of {@link Crawler}
 * Crawls web sites.
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class WebCrawler implements AdvancedCrawler {
    private final static int TIME_TO_SHUTDOWN_SECOND = 60;
    private static final int LAYER_CAPACITY = 1200;
    private final Downloader downloader;
    private final ExecutorService downloaders;
    private final ExecutorService extractors;
    private final int perHost;


    /**
     * Create new instance {@link WebCrawler}
     *
     * @param downloader  - {@link Downloader} for download pages
     * @param downloaders - count pool thread size for download pages
     * @param extractors  - count pool thread size for extract pages
     * @param perHost     - limiting the number of simultaneous downloads from the host
     */
    public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
    }

    private static void printlnWrongArgsMessage(final String errorMessage) {
        System.err.println(errorMessage + ", usage: WebCrawler url [depth [downloads [extractors [perHost]]]]");
    }

    /**
     * Usage: WebCrawler url [depth [downloads [extractors [perHost]]]], where:
     * url - url web site for download
     * downloaders - count pool thread size for download pages
     * extractors - count pool thread size for extract pages
     * perHost - limiting the number of simultaneous downloads from the host
     *
     * @param args - Arguments for run program
     */
    public static void main(final String[] args) {
        if (args == null) {
            printlnWrongArgsMessage("Args is null");
            return;
        }
        if (args.length < 1 || args.length > 5) {
            printlnWrongArgsMessage("Wrong args count");
            return;
        }
        for (int i = 0; i < args.length; i++) {
            if (args[i] == null) {
                printlnWrongArgsMessage("Arg number " + i + " is null");
                return;
            }
        }

        final int[] params = new int[]{2, 10, 10, 1};
        for (int i = 1; i < args.length; i++) {
            try {
                params[i - 1] = Integer.parseInt(args[i]);
            } catch (final NumberFormatException e) {
                printlnWrongArgsMessage("Arg number " + i + " is not a integer");
                return;
            }
        }

        try (final WebCrawler crawler = new WebCrawler(new CachingDownloader(), params[1], params[2], params[3])) {
            final Result result = crawler.download(args[0], params[0]);
            System.out.println("Downloaded: " + result.getDownloaded());
            System.out.println("Exceptions: " + result.getErrors());
        } catch (final IOException e) {
            System.err.println("Failed create Downloader: " + e.getMessage());
        }
    }

    /**
     * Downloads web site up to specified depth.
     *
     * @param url   start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth download depth.
     * @return download result.
     */
    @Override
    public Result download(final String url, final int depth) {
        return new Bfs().doDetour(url, depth);
    }

    /**
     * Downloads web site up to specified depth.
     *
     * @param url   start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth download depth.
     * @param hosts domains to follow, pages on another domains should be ignored.
     * @return download result.
     */
//    @Override
    public Result download(final String url, final int depth, final List<String> hosts) {
        return new Bfs(hosts).doDetour(url, depth);
    }

    /**
     * Closes this web-crawler, relinquishing any allocated resources. But wait {@link WebCrawler#TIME_TO_SHUTDOWN_SECOND}
     */
    @Override
    public void close() {
        downloaders.shutdown();
        extractors.shutdown();

        shutdownAndAwaitTermination("Downloaders", downloaders);
        shutdownAndAwaitTermination("Extractors", extractors);
    }

    private static void shutdownAndAwaitTermination(final String poolName, final ExecutorService pool) {
        try {
            if (!pool.awaitTermination(TIME_TO_SHUTDOWN_SECOND, TimeUnit.SECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(TIME_TO_SHUTDOWN_SECOND, TimeUnit.SECONDS)) {
                    System.err.println(poolName + " pool did not terminate");
                }
            }
        } catch (final InterruptedException e) {
            pool.shutdownNow();
            System.err.println(poolName + " pool not waited did to terminate");
            Thread.currentThread().interrupt();
        }
    }

    /**
     * Implement parallel bfs for crawl sites. Implement of {@link Bfs}
     *
     * @author Pavel Sharaev (mail@pechhenka.ru)
     */
    public class Bfs {

        private final Phaser phaser;
        private final Set<String> downloaded;
        private final Map<String, IOException> exceptions;
        private final Set<String> inWork;
        private final ConcurrentHashMap<String, LimitedExecutor> hosts;

        private final Set<String> whitelistHosts;

        /**
         * Create new instance {@link Bfs}
         */
        public Bfs() {
            this(null);
        }

        public Bfs(final List<String> hosts) {
            this.hosts = new ConcurrentHashMap<>();

            this.downloaded = ConcurrentHashMap.newKeySet();
            this.exceptions = new ConcurrentHashMap<>();
            this.inWork = ConcurrentHashMap.newKeySet();
            this.phaser = new Phaser(1);
            if (hosts != null) {
                this.whitelistHosts = ConcurrentHashMap.newKeySet();
                whitelistHosts.addAll(hosts);
            } else {
                this.whitelistHosts = null;
            }
        }

        protected final void downloadLimitationOnHost(final String url, final boolean needExtract, final Collection<String> nextLayer) {
            final String host;
            try {
                host = URLUtils.getHost(url);
            } catch (final MalformedURLException e) {
                exceptions.put(url, e);
                return;
            }

            phaser.register();
            hosts.computeIfAbsent(host, s -> new LimitedExecutor(downloaders, perHost)).submit(() -> {
                try {
                    final Document doc = downloader.download(url);
                    downloaded.add(url);
                    if (needExtract) {
                        phaser.register();
                        extractors.submit(() -> {
                            try {
                                nextLayer.addAll(doc.extractLinks().stream().filter(link -> {
                                    if (whitelistHosts == null) {
                                        return true;
                                    }
                                    final String hostLink;
                                    try {
                                        hostLink = URLUtils.getHost(link);
                                    } catch (final MalformedURLException e) {
                                        exceptions.put(link, e);
                                        return false;
                                    }
                                    return whitelistHosts.contains(hostLink);
                                }).filter(inWork::add).toList());
                            } catch (final IOException e) {
                                exceptions.put(url, e);
                            } finally {
                                phaser.arriveAndDeregister();
                            }
                        });
                    }
                } catch (final IOException e) {
                    exceptions.put(url, e);
                } finally {
                    phaser.arriveAndDeregister();
                }
            });
        }

        /**
         * Release detour
         *
         * @param url   - start <a href="http://tools.ietf.org/html/rfc3986">URL</a>
         * @param depth - detour depth
         * @return {@link Result} detour
         */
        public final Result doDetour(final String url, final int depth) {
            Queue<String> layer = new ArrayDeque<>();
            layer.add(url);
            inWork.add(url);

            for (int curDepth = 1; !layer.isEmpty() && curDepth <= depth; curDepth++) {
                final Queue<String> nextLayer = new ArrayBlockingQueue<>(LAYER_CAPACITY);
                final boolean needExtract = curDepth != depth;
                layer.forEach(link -> downloadLimitationOnHost(link, needExtract, nextLayer));
                phaser.arriveAndAwaitAdvance();
                layer = nextLayer;
            }

            return new Result(List.copyOf(downloaded), Map.copyOf(exceptions));
        }
    }
}
