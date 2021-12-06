package ru.itmo.wp.lesson8.form;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@SuppressWarnings("unused")
public class NoticeForm {
    @NotNull
    @NotBlank
    @Size(max = 1024, message = "too long content")
    private String content;

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }
}
