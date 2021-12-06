package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings("unused")
public class TalksPage extends Page {
    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        super.action(request, view);
        if (request.getSession().getAttribute("user") == null) {
            setMessage("You need to be authorized to talk");
            throw new RedirectException("/index");
        }
        view.put("talks", talkService.findAllTalks());
        view.put("users", userService.findAllUsers());
    }

    private void addMessage(HttpServletRequest request, Map<String, Object> view) {
        User user = getUser();
        if (user == null) {
            setMessage("You need to be authorized to talk");
            throw new RedirectException("/index");
        }
        Talk talk = new Talk();
        String message = request.getParameter("message");
        if (message == null || message.isBlank()) {
            setMessage("set message");
            return;
        }
        talk.setText(message);
        talk.setSourceUserId(getUser().getId());
        User targetUser = userService.findByLogin(request.getParameter("targetUser"));
        talk.setTargetUserId(targetUser.getId());
        talkService.saveTalk(talk);
        throw new RedirectException("/talks");
    }
}
