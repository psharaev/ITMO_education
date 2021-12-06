package ru.itmo.web.lesson4.web;

import freemarker.template.*;
import ru.itmo.web.lesson4.util.DataUtil;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

public class FreemarkerServlet extends HttpServlet {
    private static final String UTF_8 = StandardCharsets.UTF_8.name();
    private final Configuration freemarkerConfiguration = new Configuration(Configuration.VERSION_2_3_30);

    @Override
    public void init() throws ServletException {
        super.init();

        File dir = new File(getServletContext().getRealPath("."), "../../src/main/webapp/WEB-INF/templates");
        try {
            freemarkerConfiguration.setDirectoryForTemplateLoading(dir);
        } catch (IOException e) {
            throw new ServletException("Unable to set template directory [dir=" + dir + "].", e);
        }

        freemarkerConfiguration.setDefaultEncoding(UTF_8);
        freemarkerConfiguration.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
        freemarkerConfiguration.setLogTemplateExceptions(false);
        freemarkerConfiguration.setWrapUncheckedExceptions(true);
        freemarkerConfiguration.setFallbackOnNullLoopVariable(false);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        request.setCharacterEncoding(UTF_8);
        response.setCharacterEncoding(UTF_8);

        Template template;
        String uri = URLDecoder.decode(request.getRequestURI(), UTF_8);
        try {
            template = freemarkerConfiguration.getTemplate(uri + ".ftlh");
        } catch (TemplateNotFoundException ignored) {
            if (uri.isEmpty() || "/".equals(uri)) {
                response.setStatus(HttpServletResponse.SC_MOVED_TEMPORARILY);
                response.sendRedirect("/index");
                return;
            }

            template = freemarkerConfiguration.getTemplate("404.ftlh");
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
        }

        Map<String, Object> data = getData(request);

        response.setContentType("text/html");
        try {
            template.process(data, response.getWriter());
        } catch (TemplateException e) {
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    private Map<String, Object> getData(HttpServletRequest request) throws UnsupportedEncodingException {
        Map<String, Object> data = new HashMap<>();

        for (Map.Entry<String, String[]> e : request.getParameterMap().entrySet()) {
            if (e.getValue() != null && e.getValue().length == 1) {
                String s = e.getValue()[0];

                if (s.length() > 1 && s.charAt(0) != '-') {
                    try {
                        long num = Long.parseLong(s);
                        data.put(e.getKey(), num);
                        continue;
                    } catch (NumberFormatException ignored) {
                    }

                    if (s.length() > 3 && s.endsWith("_id")) {
                        try {
                            long num = Long.parseLong(s.substring(0, s.length() - 3));
                            data.put(e.getKey(), num);
                            continue;
                        } catch (NumberFormatException ignored) {
                        }
                    }
                }

                data.put(e.getKey(), s);
            }
        }

        data.put("URI", URLDecoder.decode(request.getRequestURI(), UTF_8));

        DataUtil.addData(request, data);
        return data;
    }
}
