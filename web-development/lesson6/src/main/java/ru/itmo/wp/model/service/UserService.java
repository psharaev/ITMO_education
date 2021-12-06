package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import com.google.common.hash.Hashing;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * @noinspection UnstableApiUsage
 */
public class UserService {
    private final UserRepository userRepository = new UserRepositoryImpl();

    private static final String PASSWORD_SALT = "177d4b5f2e4f4edafa7404533973c04c513ac619";

    public void validateRegistration(User user, String password, String passwordConfirmation) throws ValidationException {
        validateLogin(user.getLogin());
        validatePassword(password, passwordConfirmation);
        validateEmail(user.getEmail());
    }

    private void validateLogin(String login) throws ValidationException {
        if (Strings.isNullOrEmpty(login)) {
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

    private void validatePassword(String password, String passwordConfirmation) throws ValidationException {
        if (Strings.isNullOrEmpty(password)) {
            throw new ValidationException("Password is required");
        }
        if (password.length() < 4) {
            throw new ValidationException("Password can't be shorter than 4 characters");
        }
        if (password.length() > 12) {
            throw new ValidationException("Password can't be longer than 12 characters");
        }
        if (!Objects.equals(password, passwordConfirmation)) {
            throw new ValidationException("Password should be equal to password confirmation");
        }
    }

    private void validateEmail(String email) throws ValidationException {
        if (Strings.isNullOrEmpty(email)) {
            throw new ValidationException("Email is required");
        }
        if (!email.matches("[^@]*@[^@]*")) {
            throw new ValidationException("Email should contain only one @");
        }
        if (userRepository.findByEmail(email) != null) {
            throw new ValidationException("Email is already in use");
        }
    }

    public void register(User user, String password) {
        userRepository.save(user, getPasswordSha(password));
    }

    private String getPasswordSha(String password) {
        return Hashing.sha256().hashBytes((PASSWORD_SALT + password).getBytes(StandardCharsets.UTF_8)).toString();
    }

    public List<User> findAllUsers() {
        return userRepository.findAll();
    }

    public void validateEnter(String loginOrEmail, String password) throws ValidationException {
        User user = findByLoginOrEmailAndPassword(loginOrEmail, password);
        if (user == null) {
            throw new ValidationException("Invalid login/email or password");
        }
    }

    public User findByLoginOrEmailAndPassword(String loginOrEmail, String password) {
        if (loginOrEmail.matches("[^@]*@[^@]*")) {
            return userRepository.findByEmailAndPasswordSha(loginOrEmail, getPasswordSha(password));
        }
        return userRepository.findByLoginAndPasswordSha(loginOrEmail, getPasswordSha(password));
    }

    public int findCount() {
        return userRepository.getCountRows();
    }

    public User findByLogin(String login) {
        return userRepository.findByLogin(login);
    }
}
