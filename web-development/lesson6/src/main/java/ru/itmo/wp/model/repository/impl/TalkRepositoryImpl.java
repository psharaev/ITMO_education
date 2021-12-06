package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.repository.TalkRepository;

import java.sql.*;
import java.util.List;

public class TalkRepositoryImpl extends BasicRepositoryImpl<Talk> implements TalkRepository {

    public TalkRepositoryImpl() {
        super("Talk");
    }

    @Override
    public Talk find(long id) {
        return super.find(id);
    }

    @Override
    public List<Talk> findAll() {
        return super.findAll();
    }

    @Override
    public void save(Talk talk) {
        save(talk, talk.getMapValues());
    }

    @Override
    protected Talk toEntity(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Talk talk = new Talk();
        talk.setId(resultSet.getLong("id"));
        talk.setSourceUserId(resultSet.getLong("sourceUserId"));
        talk.setTargetUserId(resultSet.getLong("targetUserId"));
        talk.setText(resultSet.getString("text"));
        talk.setCreationTime(resultSet.getTimestamp("creationTime"));
        return talk;
    }
}
