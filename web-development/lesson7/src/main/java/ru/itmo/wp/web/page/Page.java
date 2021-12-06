package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class Page {
    protected final UserService userService = new UserService();
    protected final ArticleService articleService = new ArticleService();

    protected void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    protected User getUser(HttpServletRequest request) {
        return (User) request.getSession().getAttribute("user");
    }

    protected User claimUser(HttpServletRequest request) {
        User user = getUser(request);
        if (user == null) {
            request.getSession().setAttribute("message", "You need be authorized!");
            throw new RedirectException("/index");
        }
        return user;
    }
}
