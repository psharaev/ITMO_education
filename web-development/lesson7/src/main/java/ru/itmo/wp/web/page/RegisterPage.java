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
public class RegisterPage extends Page {

    @Json
    private void register(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User user = new User();
        user.setLogin(request.getParameter("login"));
        user.setAdmin(false);
        String password = request.getParameter("password");

        userService.validateRegistration(user, password);
        userService.register(user, password);

        request.getSession().setAttribute("message", "You are successfully registered!");
        request.getSession().setAttribute("user", user);

        throw new RedirectException("/index");
    }
}
