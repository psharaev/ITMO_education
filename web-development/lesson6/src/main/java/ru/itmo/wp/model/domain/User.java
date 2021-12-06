package ru.itmo.wp.model.domain;

import java.util.Map;

public class User extends Entity {
    private String login;
    private String email;


    public String getLogin() {
        return login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    @Override
    public Map<String, String> getMapValues() {
        return Map.of(
                "login", login,
                "email", email);
    }
}
