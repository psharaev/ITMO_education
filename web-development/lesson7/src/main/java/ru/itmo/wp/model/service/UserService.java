package ru.itmo.wp.model.service;

import com.google.common.hash.Hashing;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * @noinspection UnstableApiUsage
 */
public class UserService {
    private final UserRepository userRepository = new UserRepositoryImpl();
    private static final String PASSWORD_SALT = "177d4b5f2e4f4edafa7404533973c04c513ac619";

    public void validateRegistration(User user, String password) throws ValidationException {
        validateLogin(user.getLogin());
        validatePassword(password);
    }

    private void validateLogin(String login) throws ValidationException {
        if (isNullOrBlank(login)) {
            throw new ValidationException("Login is required");
        }
        if (!login.matches("[a-z]+")) {
            throw new ValidationException("Login can contain only lowercase Latin letters");
        }
        if (login.length() > 8) {
            throw new ValidationException("Login can't be longer than 8 letters");
        }
        if (userRepository.findByLogin(login) != null) {
            throw new ValidationException("Login is already in use");
        }
    }

    private void validatePassword(String password) throws ValidationException {
        if (isNullOrBlank(password)) {
            throw new ValidationException("Password is required");
        }
        if (password.length() < 4) {
            throw new ValidationException("Password can't be shorter than 4 characters");
        }
        if (password.length() > 12) {
            throw new ValidationException("Password can't be longer than 12 characters");
        }
    }

    public void register(User user, String password) {
        userRepository.save(user, getPasswordSha(password));
    }

    private String getPasswordSha(String password) {
        return Hashing.sha256().hashBytes((PASSWORD_SALT + password).getBytes(StandardCharsets.UTF_8)).toString();
    }

    public List<User> findAll() {
        return userRepository.findAll();
    }

    public User find(long id) {
        return userRepository.find(id);
    }

    public User validateAndFindByLoginAndPassword(String login, String password) throws ValidationException {
        User user = userRepository.findByLoginAndPasswordSha(login, getPasswordSha(password));
        if (user == null) {
            throw new ValidationException("Invalid login or password");
        }
        return user;
    }

    public User validateUserIdAndGetUser(String id) throws ValidationException {
        long id_;
        try {
            id_ = Long.parseLong(id);
            return find(id_);
        } catch (NumberFormatException e) {
            throw new ValidationException("Unknown userId");
        }
    }

    public void validateUserIsAdmin(User user) throws ValidationException {
        if (user == null || !user.isAdmin()) {
            throw new ValidationException("You must be admin");
        }
    }

    public void setAdmin(User user, boolean isAdmin) {
        userRepository.setAdmin(user, isAdmin);
    }

    private boolean isNullOrBlank(String s) {
        return s == null || s.isBlank();
    }
}
