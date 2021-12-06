package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Event;

public interface EventRepository {
    Event find(long id);

    void save(Event event);
}
