package ru.itmo.wp.lesson8.domain;

import org.hibernate.annotations.CreationTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.Date;

@SuppressWarnings("unused")
@Entity
@Table(indexes = {
        @Index(columnList = "creationTime"),
        @Index(columnList = "content")})
public class Notice {
    @Id
    @GeneratedValue
    private long id;

    @NotNull
    @NotBlank
    @Lob
    private String content;

    @CreationTimestamp
    private Date creationTime;

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }
}
