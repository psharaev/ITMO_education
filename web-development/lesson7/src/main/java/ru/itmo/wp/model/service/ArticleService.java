package ru.itmo.wp.model.service;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;

import java.util.List;

public class ArticleService {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();

    public void validateArticle(Article article) throws ValidationException {
        validateTitle(article.getTitle());
        validateText(article.getText());
    }

    private void validateTitle(String title) throws ValidationException {
        if (isNullOrBlank(title)) {
            throw new ValidationException("Title is required");
        }
        if (title.length() > 255) {
            throw new ValidationException("Title too long");
        }
    }

    private void validateText(String text) throws ValidationException {
        if (isNullOrBlank(text)) {
            throw new ValidationException("Text is required");
        }
        if (text.length() > 4095) {
            throw new ValidationException("Text too long");
        }
    }

    public Article validateAndFindByUserAndArticleId(User user, String id) throws ValidationException {
        long id_;
        try {
            id_ = Long.parseLong(id);
        } catch (NumberFormatException e) {
            throw new ValidationException("id is require");
        }
        Article article = articleRepository.find(id_);
        if (article.getUserId() != user.getId()) {
            throw new ValidationException("You could update only your own articles");
        }
        return article;
    }

    public Article find(long id) {
        return articleRepository.find(id);
    }

    public void save(Article article) {
        articleRepository.save(article);
    }

    public List<Article> findAll() {
        return articleRepository.findAll();
    }

    public List<Article> findAllByUserId(long id) {
        return articleRepository.findAllByUserId(id);
    }

    public void setHidden(Article article) {
        articleRepository.setHidden(article);
    }

    public void setShown(Article article) {
        articleRepository.setShown(article);
    }

    private boolean isNullOrBlank(String s) {
        return s == null || s.isBlank();
    }
}
