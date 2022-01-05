package ru.itmo.wp.domain;

import javax.persistence.*;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Set;
import java.util.TreeSet;

@Entity
@Table
public class Tag {
    @Id
    @GeneratedValue
    private long id;

    @NotNull
    @NotBlank
    @Column(unique = true)
    private String name;

    private static final Set<String> tags = new TreeSet<>();
    static {
        tags.add("hw");
        tags.add("web");
        tags.add("code");
    }

    /** @noinspection unused*/
    public Tag() {
    }

    public Tag(String name) {
        this.name = name;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(@NotNull @NotEmpty String name) {
        this.name = name;
    }

    public static Set<String> getTags() {
        return tags;
    }
}
