package ru.itmo.wp.web.page;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.EventService;
import ru.itmo.wp.model.service.TalkService;
import ru.itmo.wp.model.service.UserService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings("unused")
public class Page {
    protected final UserService userService = new UserService();
    protected final EventService eventService = new EventService();
    protected final TalkService talkService = new TalkService();
    private HttpServletRequest request;

    protected void before(HttpServletRequest request, Map<String, Object> view) {
        this.request = request;
        putUser(view);
        putMessage(view);
        view.put("userCount", userService.findCount());
    }

    protected void action(HttpServletRequest request, Map<String, Object> view) {
    }

    protected void after(HttpServletRequest request, Map<String, Object> view) {
    }

    private void putUser(Map<String, Object> view) {
        User user = getUser();
        if (user != null) {
            view.put("user", user);
        }
    }

    private void putMessage(Map<String, Object> view) {
        String message = getMessage();
        if (!Strings.isNullOrEmpty(message)) {
            view.put("message", message);
            request.getSession().removeAttribute("message");
        }
    }

    protected void setMessage(String message) {
        request.getSession().setAttribute("message", message);
    }

    protected String getMessage() {
        return (String) request.getSession().getAttribute("message");
    }

    protected void setUser(User user) {
        request.getSession().setAttribute("user", user);
    }

    protected User getUser() {
        return (User) request.getSession().getAttribute("user");
    }
}
