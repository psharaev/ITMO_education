package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.lesson8.domain.User;
import ru.itmo.wp.lesson8.form.UserCredentials;
import ru.itmo.wp.lesson8.form.validator.UserCredentialsEnterValidator;
import ru.itmo.wp.lesson8.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class EnterPage extends Page {
    private final UserService userService;
    private final UserCredentialsEnterValidator userCredentialsEnterValidator;

    public EnterPage(UserService userService, UserCredentialsEnterValidator userCredentialsEnterValidator) {
        this.userService = userService;
        this.userCredentialsEnterValidator = userCredentialsEnterValidator;
    }

    @InitBinder
    public void initBinder(WebDataBinder binder) {
        binder.addValidators(userCredentialsEnterValidator);
    }

    @GetMapping("/enter")
    public String enter(Model model) {
        model.addAttribute("enterForm", new UserCredentials());
        return "EnterPage";
    }

    @PostMapping("/enter")
    public String enter(@Valid @ModelAttribute("enterForm") UserCredentials enterForm,
                        BindingResult bindingResult,
                        HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "EnterPage";
        }


        User user = userService.findByLoginAndPassword(enterForm.getLogin(), enterForm.getPassword());
        if (user.isDisabled()) {
            setMessage(httpSession, "User " + user.getLogin() + " is disabled");
            return "EnterPage";
        }
        
        setUser(httpSession, user);
        setMessage(httpSession, "Hello, " + getUser(httpSession).getLogin());

        return "redirect:";
    }
}
