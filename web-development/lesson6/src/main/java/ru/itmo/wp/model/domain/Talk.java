package ru.itmo.wp.model.domain;

import java.util.Map;

public class Talk extends Entity {
    private long sourceUserId;
    private long targetUserId;
    private String text;


    public long getSourceUserId() {
        return sourceUserId;
    }

    public void setSourceUserId(long sourceUserId) {
        this.sourceUserId = sourceUserId;
    }

    public long getTargetUserId() {
        return targetUserId;
    }

    public void setTargetUserId(long targetUserId) {
        this.targetUserId = targetUserId;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public Map<String, String> getMapValues() {
        return Map.of(
                "sourceUserId", Long.toString(sourceUserId),
                "targetUserId", Long.toString(targetUserId),
                "text", text);
    }
}
