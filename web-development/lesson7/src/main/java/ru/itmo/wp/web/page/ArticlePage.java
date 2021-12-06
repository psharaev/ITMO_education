package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.web.annotation.Json;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class ArticlePage extends Page {

    @Json
    private void create(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User user = getUser(request);
        if (user == null) {
            throw new ValidationException("you must be logged");
        }

        Article article = new Article();
        article.setUserId(user.getId());
        article.setTitle(request.getParameter("title"));
        article.setText(request.getParameter("text"));

        articleService.validateArticle(article);

        articleService.save(article);
        request.getSession().setAttribute("message", "Your article is successfully created!");
        throw new RedirectException("/index");
    }
}
