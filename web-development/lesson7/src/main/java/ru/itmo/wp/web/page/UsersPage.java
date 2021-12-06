package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.web.annotation.Json;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * @noinspection unused
 */
public class UsersPage extends Page {

    @Json
    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("users", userService.findAll());
    }

    @Json
    private void findUser(HttpServletRequest request, Map<String, Object> view) {
        view.put("user", userService.find(Long.parseLong(request.getParameter("userId"))));
    }

    @Json
    private void setAdmin(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        userService.validateUserIsAdmin(claimUser(request));
        User user = userService.validateUserIdAndGetUser(request.getParameter("id"));
        userService.setAdmin(user, true);
        view.put("id", user.getId());
        view.put("admin", true);
    }

    @Json
    private void unsetAdmin(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        userService.validateUserIsAdmin(claimUser(request));
        User user = userService.validateUserIdAndGetUser(request.getParameter("id"));
        userService.setAdmin(user, false);
        view.put("id", user.getId());
        view.put("admin", false);
    }
}
