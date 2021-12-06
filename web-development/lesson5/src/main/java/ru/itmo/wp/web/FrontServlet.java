package ru.itmo.wp.web;

import freemarker.template.*;
import ru.itmo.wp.web.exception.NotFoundException;
import ru.itmo.wp.web.exception.RedirectException;
import ru.itmo.wp.web.page.IndexPage;
import ru.itmo.wp.web.page.NotFoundPage;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

public class FrontServlet extends HttpServlet {
    private static final String BASE_PACKAGE = FrontServlet.class.getPackage().getName() + ".page";
    private static final String DEFAULT_ACTION = "action";

    private Configuration sourceConfiguration;
    private Configuration targetConfiguration;

    private Configuration newFreemarkerConfiguration(String templateDirName, boolean debug)
            throws ServletException {
        File templateDir = new File(templateDirName);
        if (!templateDir.isDirectory()) {
            return null;
        }

        Configuration configuration = new Configuration(Configuration.VERSION_2_3_31);
        try {
            configuration.setDirectoryForTemplateLoading(templateDir);
        } catch (IOException e) {
            throw new ServletException("Can't create freemarker configuration [templateDir="
                    + templateDir + "]");
        }
        configuration.setDefaultEncoding(StandardCharsets.UTF_8.name());
        configuration.setTemplateExceptionHandler(debug ? TemplateExceptionHandler.HTML_DEBUG_HANDLER :
                TemplateExceptionHandler.RETHROW_HANDLER);
        configuration.setLogTemplateExceptions(false);
        configuration.setWrapUncheckedExceptions(true);
        configuration.setLocalizedLookup(false);

        return configuration;
    }

    @Override
    public void init() throws ServletException {
        sourceConfiguration = newFreemarkerConfiguration(
                getServletContext().getRealPath("/") + "../../src/main/webapp/WEB-INF/templates", true);
        targetConfiguration = newFreemarkerConfiguration(
                getServletContext().getRealPath("WEB-INF/templates"), false);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        process(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        process(request, response);
    }

    private void process(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        Route route = Route.newRoute(request);
        try {
            process(route, request, response);
        } catch (NotFoundException e) {
            try {
                process(Route.newNotFoundRoute(), request, response);
            } catch (NotFoundException notFoundException) {
                throw new ServletException(notFoundException);
            }
        }
    }

    private void setLang(HttpServletRequest request) {
        String parameter = request.getParameter("lang");
        if (parameter != null && parameter.matches("[a-z]{2}")) {
            request.getSession().setAttribute("lang", parameter);
        }
    }

    private void process(Route route, HttpServletRequest request, HttpServletResponse response)
            throws NotFoundException, ServletException, IOException {
        setLang(request);

        Class<?> pageClass = initPageClass(route);
        Object page = initPage(pageClass);

        Method method = null;
        Class<?>[] methodParameters = null;
        Map<String, Object> view = new HashMap<>();
        Map<Class<?>, Object> parameterValues = Map.of(HttpServletRequest.class, request, Map.class, view);

        for (Class<?> clazz = pageClass; method == null && clazz != null; clazz = clazz.getSuperclass()) {
            for (var m : clazz.getDeclaredMethods()) {
                if (route.getAction().equals(m.getName())) {
                    m.setAccessible(true);
                    Class<?>[] parameterTypes = m.getParameterTypes();
                    methodParameters = getParameters(parameterValues, parameterTypes);
                    if (methodParameters.length == parameterTypes.length) {
                        method = m;
                        break;
                    }
                }
            }
        }

        if (method == null) {
            throw new NotFoundException();
        }

        method.setAccessible(true);
        try {
            method.invoke(page, mapToInstance(parameterValues, methodParameters));
        } catch (IllegalAccessException e) {
            throw new ServletException("Can't invoke action method [pageClass="
                    + pageClass + ", method=" + method + "]");
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof RedirectException) {
                RedirectException redirectException = (RedirectException) cause;
                response.sendRedirect(redirectException.getTarget());
                return;
            } else {
                throw new ServletException("Can't invoke action method [pageClass="
                        + pageClass + ", method=" + method + "]", cause);
            }
        }

        Template template = newTemplate(pageClass, getLang(request));

        response.setContentType("text/html");
        response.setCharacterEncoding(StandardCharsets.UTF_8.name());

        try {
            template.process(view, response.getWriter());
        } catch (TemplateException e) {
            if (sourceConfiguration == null) {
                throw new ServletException("Can't render template [pageClass="
                        + pageClass + ", action=" + method + "]", e);
            }
        }
    }

    private String getLang(HttpServletRequest request) {
        return (String) request.getSession().getAttribute("lang");
    }

    private Object[] mapToInstance(Map<Class<?>, Object> rule, Class<?>... parameterTypes) {
        return Arrays.stream(parameterTypes).map(rule::get).toArray();
    }

    private Class<?>[] getParameters(Map<Class<?>, Object> rule, Class<?>... parameterTypes) {
        return Arrays.stream(parameterTypes).filter(rule::containsKey).toArray(Class<?>[]::new);
    }

    private Object initPage(Class<?> pageClass) throws ServletException {
        Object page;
        try {
            //noinspection deprecation
            page = pageClass.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new ServletException("Can't create page [pageClass="
                    + pageClass + "]");
        }
        return page;
    }

    private Class<?> initPageClass(Route route) throws NotFoundException {
        Class<?> pageClass;
        try {
            pageClass = Class.forName(route.getClassName());
        } catch (ClassNotFoundException e) {
            throw new NotFoundException();
        }
        return pageClass;
    }

    private Template newTemplate(String templateName) throws ServletException, FileNotFoundException {
        Template template = null;

        if (sourceConfiguration != null) {
            try {
                template = sourceConfiguration.getTemplate(templateName);
            } catch (TemplateNotFoundException ignored) {
                // No operations.
            } catch (IOException e) {
                throw new ServletException("Can't load template [templateName=" + templateName + "]", e);
            }
        }

        if (template == null && targetConfiguration != null) {
            try {
                template = targetConfiguration.getTemplate(templateName);
            } catch (TemplateNotFoundException ignored) {
                // No operations.
            } catch (IOException e) {
                throw new ServletException("Can't load template [templateName=" + templateName + "]", e);
            }
        }

        if (template == null) {
            throw new FileNotFoundException("Can't find template [templateName=" + templateName + "]");
        }

        return template;
    }

    private Template newTemplate(Class<?> pageClass, String lang) throws ServletException {
        Template template;
        try {
            if (lang != null && !lang.equals("en")) {
                template = newTemplate(pageClass.getSimpleName() + "_" + lang + ".ftlh");
            } else {
                template = newTemplate(pageClass.getSimpleName() + ".ftlh");
            }
        } catch (FileNotFoundException e) {
            try {
                template = newTemplate(pageClass.getSimpleName() + ".ftlh");
            } catch (FileNotFoundException ex) {
                throw new ServletException("Can't load template [pageClass=" +
                        pageClass + "]", ex);
            }
        }
        return template;
    }

    private static class Route {
        private final String className;
        private final String action;

        private Route(String className, String action) {
            this.className = className;
            this.action = action;
        }

        private String getClassName() {
            return className;
        }

        private String getAction() {
            return action;
        }

        private static Route newNotFoundRoute() {
            return new Route(NotFoundPage.class.getName(), DEFAULT_ACTION);
        }

        private static Route newIndexRoute() {
            return new Route(IndexPage.class.getName(), DEFAULT_ACTION);
        }

        private static Route newRoute(HttpServletRequest request) {
            String uri = request.getRequestURI();

            List<String> classNameParts = Arrays.stream(uri.split("/"))
                    .filter(part -> !part.isEmpty())
                    .collect(Collectors.toList());

            if (classNameParts.isEmpty()) {
                return newIndexRoute();
            }


            StringBuilder simpleClassName = new StringBuilder(classNameParts.get(classNameParts.size() - 1));
            int lastDotIndex = simpleClassName.lastIndexOf(".");
            simpleClassName.setCharAt(lastDotIndex + 1,
                    Character.toUpperCase(simpleClassName.charAt(lastDotIndex + 1)));
            classNameParts.set(classNameParts.size() - 1, simpleClassName.toString());

            String className = BASE_PACKAGE + "." + String.join(".", classNameParts) + "Page";

            String action = request.getParameter("action");
            if (action == null || action.isEmpty()) {
                action = DEFAULT_ACTION;
            }

            return new Route(className, action);
        }
    }
}
