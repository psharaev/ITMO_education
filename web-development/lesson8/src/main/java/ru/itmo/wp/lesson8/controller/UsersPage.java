package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.lesson8.domain.User;
import ru.itmo.wp.lesson8.service.UserService;

import javax.servlet.http.HttpSession;

@Controller
public class UsersPage extends Page {
    private final UserService userService;

    public UsersPage(UserService userService) {
        this.userService = userService;
    }

    @GetMapping("/users/all")
    public String users(Model model) {
        model.addAttribute("users", userService.findAll());
        return "UsersPage";
    }

    @PostMapping("/users/enable/{id}")
    public String setEnabled(@PathVariable(value = "id") String id,
                             HttpSession httpSession) {
        if (requireActiveUser(httpSession)) {
            return "redirect:/users/all";
        }

        User user = idToUser(id);
        if (user == null) {
            setMessage(httpSession, "No such user");
        } else {
            userService.setDisabled(user, false);
            setMessage(httpSession, "User " + user.getLogin() + " is enabled");
        }
        return "redirect:/users/all";
    }

    @PostMapping("/users/disable/{id}")
    public String setDisabled(@PathVariable(value = "id") String id,
                              HttpSession httpSession) {
        if (requireActiveUser(httpSession)) {
            return "redirect:/users/all";
        }

        User user = idToUser(id);
        if (user == null) {
            setMessage(httpSession, "No such user");
        } else {
            userService.setDisabled(user, true);
            setMessage(httpSession, "User " + user.getLogin() + " is disabled");
        }
        return "redirect:/users/all";
    }

    private boolean requireActiveUser(HttpSession httpSession) {
        User requestUser = getUser(httpSession);
        if (requestUser == null || requestUser.isDisabled()) {
            setMessage(httpSession, "You must be active user");
            return true;
        }
        return false;
    }
}
