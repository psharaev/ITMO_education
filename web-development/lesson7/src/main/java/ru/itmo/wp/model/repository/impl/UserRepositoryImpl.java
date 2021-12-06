package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.repository.UserRepository;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class UserRepositoryImpl extends BasicRepositoryImpl<User> implements UserRepository {

    public UserRepositoryImpl() {
        super("User");
    }

    @Override
    public User find(long id) {
        return super.find(id);
    }

    @Override
    public User findByLogin(String login) {
        return findBy(Map.of("login", login));
    }

    @Override
    public User findByLoginAndPasswordSha(String login, String passwordSha) {
        return findBy(Map.of("login", login,
                "passwordSha", passwordSha));
    }

    @Override
    public List<User> findAll() {
        return super.findAll();
    }

    @Override
    protected User toEntity(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        User user = new User();
        user.setId(resultSet.getLong("id"));
        user.setCreationTime(resultSet.getTimestamp("creationTime"));
        user.setLogin(resultSet.getString("login"));
        user.setAdmin(resultSet.getBoolean("admin"));
        return user;
    }

    @Override
    public void save(User user, String passwordSha) {
        Map<String, String> mapValues = new HashMap<>(user.getMapValues());
        mapValues.put("passwordSha", passwordSha);
        save(user, mapValues);
    }

    @Override
    public int getCountRows() {
        return super.getCountRows();
    }

    @Override
    public void setAdmin(User user, boolean isAdmin) {
        updateFields(user, Map.of("admin", isAdmin ? "1" : "0"));
    }
}
