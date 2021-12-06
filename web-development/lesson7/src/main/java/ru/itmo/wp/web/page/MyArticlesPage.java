package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.web.annotation.Json;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class MyArticlesPage extends Page {
    @Json
    private void findMyArticles(HttpServletRequest request, Map<String, Object> view) {
        long userId = claimUser(request).getId();
        view.put("articles", articleService.findAllByUserId(userId));
    }

    @Json
    private void setHidden(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User user = claimUser(request);
        String id = request.getParameter("id");
        Article article = articleService.validateAndFindByUserAndArticleId(user, id);
        articleService.setHidden(article);
        view.put("id", article.getId());
        view.put("hidden", true);
    }

    @Json
    private void setShown(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User user = claimUser(request);
        String id = request.getParameter("id");
        Article article = articleService.validateAndFindByUserAndArticleId(user, id);
        articleService.setShown(article);
        view.put("id", article.getId());
        view.put("hidden", false);
    }
}
