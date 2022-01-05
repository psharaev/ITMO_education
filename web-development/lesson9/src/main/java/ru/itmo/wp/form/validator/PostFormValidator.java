package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.service.TagService;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class PostFormValidator implements Validator {
    private final TagService tagService;

    public PostFormValidator(TagService tagService) {
        this.tagService = tagService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return PostForm.class.equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            PostForm postForm = (PostForm) target;
            List<String> tags = Arrays.stream(postForm.getTags().split("\\s+"))
                    .map(String::toLowerCase)
                    .distinct().collect(Collectors.toList());
            postForm.setTags(String.join(" ", tags));
        }
    }
}
