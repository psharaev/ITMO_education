package ru.itmo.web.lesson4.model;

import ru.itmo.web.lesson4.util.DataUtil;

import java.util.List;
import java.util.stream.Collectors;

public class User {

    public enum Color {
        RED, GREEN, BLUE
    }

    private final long id;
    private final String handle;
    private final String name;
    private final Color color;

    public User(long id, String handle, String name, Color color) {
        this.id = id;
        this.handle = handle;
        this.name = name;
        this.color = color;
    }

    public long getId() {
        return id;
    }

    public String getHandle() {
        return handle;
    }

    public String getName() {
        return name;
    }

    public Color getColor() {
        return color;
    }

    public List<Post> getPosts() {
        return DataUtil.POSTS.stream().filter(post -> post.getUserId() == id).collect(Collectors.toList());
    }

    public long getPostCount() {
        return DataUtil.POSTS.stream().filter(post -> post.getUserId() == id).count();
    }
}
