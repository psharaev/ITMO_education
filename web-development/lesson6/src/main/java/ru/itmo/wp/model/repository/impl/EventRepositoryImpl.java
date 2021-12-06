package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.repository.EventRepository;

import java.sql.*;

public class EventRepositoryImpl extends BasicRepositoryImpl<Event> implements EventRepository {

    public EventRepositoryImpl() {
        super("Event");
    }

    @Override
    public Event find(long id) {
        return super.find(id);
    }

    @Override
    public void save(Event event) {
        save(event, event.getMapValues());
    }

    @Override
    protected Event toEntity(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Event event = new Event();
        event.setId(resultSet.getLong("id"));
        event.setCreationTime(resultSet.getTimestamp("creationTime"));
        event.setType((Event.TYPE.valueOf((String) resultSet.getObject("type"))));
        event.setUserId(resultSet.getLong("userId"));
        return event;
    }
}
