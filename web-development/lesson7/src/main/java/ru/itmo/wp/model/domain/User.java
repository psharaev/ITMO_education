package ru.itmo.wp.model.domain;

import java.util.Map;

public class User extends Entity {
    private String login;
    private boolean admin;

    public String getLogin() {
        return login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    public boolean isAdmin() {
        return admin;
    }

    public void setAdmin(boolean admin) {
        this.admin = admin;
    }

    @Override
    public Map<String, String> getMapValues() {
        return Map.of("login", login,
                "admin", admin ? "1" : "0");
    }
}
