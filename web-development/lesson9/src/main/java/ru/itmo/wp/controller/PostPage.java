package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.security.Guest;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class PostPage extends Page {
    @Guest
    @GetMapping("/post/{id}")
    public String getPost(Model model, @PathVariable String id) {
        model.addAttribute("post", idToPost(id));
        model.addAttribute("comment", new Comment());
        return "PostPage";
    }

    @Guest
    @GetMapping("/post")
    public String getPost(Model model) {
        model.addAttribute("post", null);
        return "PostPage";
    }

    @PostMapping("/post/{id}")
    public String writeComment(@PathVariable String id, Model model, @Valid @ModelAttribute("comment") Comment comment,
                               BindingResult bindingResult,
                               HttpSession httpSession) {
        User user = getUser(httpSession);
        if (user == null) {
            putMessage(httpSession, "You must be logged");
            return "PostPage";
        }

        Post post = idToPost(id);

        if (bindingResult.hasErrors()) {
            model.addAttribute("post", post);
            return "PostPage";
        }

        if (post == null) {
            return "PostPage";
        }

        comment.setPost(post);
        comment.setUser(user);
        commentService.save(comment);
        putMessage(httpSession, "You published new comment");

        return "redirect:/post/" + id;
    }
}
