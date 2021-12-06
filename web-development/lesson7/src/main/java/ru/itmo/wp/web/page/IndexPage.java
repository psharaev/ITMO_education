package ru.itmo.wp.web.page;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Entity;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.web.annotation.Json;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @noinspection unused
 */
public class IndexPage extends Page {

    protected void action(HttpServletRequest request, Map<String, Object> view) {
        putMessage(request, view);
    }

    private void putMessage(HttpServletRequest request, Map<String, Object> view) {
        String message = (String) request.getSession().getAttribute("message");
        if (!Strings.isNullOrEmpty(message)) {
            view.put("message", message);
            request.getSession().removeAttribute("message");
        }
    }

    @Json
    private void findAllArticles(HttpServletRequest request, Map<String, Object> view) {
        Map<Long, String> idToLogin = userService.findAll().stream().collect(Collectors.toMap(Entity::getId, User::getLogin, (a, b) -> b));
        view.put("idToLogin", idToLogin);
        view.put("articles", articleService.findAll().stream().filter(x -> !x.isHidden()).collect(Collectors.toList()));
    }
}
