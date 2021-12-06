package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.repository.ArticleRepository;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public class ArticleRepositoryImpl extends BasicRepositoryImpl<Article> implements ArticleRepository {

    public ArticleRepositoryImpl() {
        super("Article");
    }

    @Override
    public Article find(long id) {
        return super.find(id);
    }

    @Override
    public void save(Article article) {
        save(article, article.getMapValues());
    }

    @Override
    public List<Article> findAll() {
        return super.findAll();
    }

    @Override
    public List<Article> findAllByUserId(long userId) {
        return super.findAllBy(Map.of("userId", Long.toString(userId)));
    }

    @Override
    public void setHidden(Article article) {
        updateFields(article, Map.of("hidden", "1"));
    }

    @Override
    public void setShown(Article article) {
        updateFields(article, Map.of("hidden", "0"));
    }


    @Override
    protected Article toEntity(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Article article = new Article();
        article.setId(resultSet.getLong("id"));
        article.setCreationTime(resultSet.getTimestamp("creationTime"));
        article.setUserId(resultSet.getLong("userId"));
        article.setTitle(resultSet.getString("title"));
        article.setText(resultSet.getString("text"));
        article.setHidden(resultSet.getBoolean("hidden"));
        return article;
    }
}
