package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.lesson8.form.NoticeForm;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class NoticePage extends Page {
    @GetMapping(path = "/notice")
    public String noticeGet(Model model) {
        model.addAttribute("noticeForm", new NoticeForm());
        return "NoticePage";
    }

    @PostMapping(path = "/notice")
    public String noticePost(@Valid @ModelAttribute("noticeForm") NoticeForm noticeForm,
                               BindingResult bindingResult, HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "NoticePage";
        }

        noticeService.save(noticeForm);
        setMessage(httpSession, "Your notice has been successfully added");
        return "redirect:";
    }
}
