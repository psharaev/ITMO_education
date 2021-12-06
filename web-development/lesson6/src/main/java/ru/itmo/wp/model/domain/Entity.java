package ru.itmo.wp.model.domain;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

public abstract class Entity implements Serializable {
    protected long id;
    protected Date creationTime;

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    public abstract Map<String, String> getMapValues();
}
