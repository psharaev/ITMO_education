package ru.itmo.wp.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.google.gson.Gson;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

public class MessageServlet extends HttpServlet {

    private final List<Comment> messages = new ArrayList<>();

    private static class Comment {
        public final String user;
        public final String text;

        Comment(String user, String text) {
            this.user = user;
            this.text = text;
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String uri = request.getRequestURI();
        HttpSession session = request.getSession();
        String user;
        switch (uri) {
            case "/message/auth":
                user = request.getParameter("user");
                if (user != null && !user.isBlank()) {
                    session.setAttribute("user", user);
                    writeJson(user, response);
                }
                break;
            case "/message/findAll":
                user = (String) session.getAttribute("user");
                if (user != null) {
                    writeJson(messages, response);
                }
                break;
            case "/message/add":
                user = (String) session.getAttribute("user");
                String text = request.getParameter("text");
                if (user != null && text != null && !text.isBlank()) {
                    messages.add(new Comment(user, text));
                }
                break;
            default:
                response.sendError(HttpServletResponse.SC_BAD_REQUEST);
                return;
        }
        response.setContentType("application/json");
    }

    private void writeJson(Object objectToWrite, HttpServletResponse response) throws IOException {
        String json = new Gson().toJson(objectToWrite);
        response.getOutputStream().write(json.getBytes(StandardCharsets.UTF_8));
    }
}
