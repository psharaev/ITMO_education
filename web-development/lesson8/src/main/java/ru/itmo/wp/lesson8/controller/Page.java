package ru.itmo.wp.lesson8.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import ru.itmo.wp.lesson8.domain.Notice;
import ru.itmo.wp.lesson8.domain.User;
import ru.itmo.wp.lesson8.service.NoticeService;
import ru.itmo.wp.lesson8.service.UserService;

import javax.servlet.http.HttpSession;
import java.util.List;

public class Page {
    private static final String USER_ID_SESSION_KEY = "userId";
    private static final String MESSAGE_SESSION_KEY = "message";

    @Autowired
    private UserService userService;

    @Autowired
    protected NoticeService noticeService;

    @ModelAttribute("user")
    public User getUser(HttpSession httpSession) {
        return userService.findById((Long) httpSession.getAttribute(USER_ID_SESSION_KEY));
    }

    @ModelAttribute("message")
    public String getMessage(HttpSession httpSession) {
        String message = (String) httpSession.getAttribute(MESSAGE_SESSION_KEY);
        httpSession.removeAttribute(MESSAGE_SESSION_KEY);
        return message;
    }

    @ModelAttribute("notice")
    public List<Notice> getNotices(Model model) {
        List<Notice> notices = noticeService.findAll();
        model.addAttribute("notices", notices);
        return notices;
    }

    protected void setUser(HttpSession httpSession, User user) {
        if (user != null) {
            httpSession.setAttribute(USER_ID_SESSION_KEY, user.getId());
        } else {
            unsetUser(httpSession);
        }
    }

    protected User idToUser(String id) {
        if (id == null) {
            return null;
        }
        try {
            return userService.findById(Long.parseLong(id));
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    protected void unsetUser(HttpSession httpSession) {
        httpSession.removeAttribute(USER_ID_SESSION_KEY);
    }

    protected void setMessage(HttpSession httpSession, String message) {
        httpSession.setAttribute(MESSAGE_SESSION_KEY, message);
    }
}
