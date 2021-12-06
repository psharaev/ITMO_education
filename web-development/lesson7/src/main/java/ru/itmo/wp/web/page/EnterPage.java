package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.web.annotation.Json;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * @noinspection unused
 */
public class EnterPage extends Page {

    @Json
    private void enter(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        String login = request.getParameter("login");
        String password = request.getParameter("password");

        User user = userService.validateAndFindByLoginAndPassword(login, password);
        request.getSession().setAttribute("user", user);
        request.getSession().setAttribute("message", "Hello, " + user.getLogin());

        throw new RedirectException("/index");
    }
}
