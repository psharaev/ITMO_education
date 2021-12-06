package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import ru.itmo.wp.lesson8.domain.User;

@Controller
public class UserPage extends Page {

    @GetMapping({
            "/user/{id:[0-9]+}",
            "/user/{invalidId}",
            "/user"
    })
    public String users(Model model, @PathVariable(value = "id") String id) {
        User user = idToUser(id);
        model.addAttribute("user", user);
        return "UserPage";
    }
}
