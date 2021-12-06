package ru.itmo.wp.model.domain;

import java.util.Map;

public class Article extends Entity {
    private long userId;
    private String title;
    private String text;
    private boolean hidden;

    public long getUserId() {
        return userId;
    }

    public void setUserId(long userId) {
        this.userId = userId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public boolean isHidden() {
        return hidden;
    }

    public void setHidden(boolean hidden) {
        this.hidden = hidden;
    }

    @Override
    public Map<String, String> getMapValues() {
        return Map.of("userId", Long.toString(userId),
                "title", title,
                "text", text,
                "hidden", hidden ? "1" : "0");
    }
}
