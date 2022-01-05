package ru.itmo.wp.controller;

import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.service.JwtService;
import ru.itmo.wp.service.UserService;

import javax.validation.constraints.NotEmpty;
import java.util.List;

@RestController
@RequestMapping("/api/1")
public class UserController {
    private final JwtService jwtService;
    private final UserService userService;

    public UserController(JwtService jwtService, UserService userService) {
        this.jwtService = jwtService;
        this.userService = userService;
    }

    @GetMapping("users/auth")
    public User findUserByJwt(@RequestParam String jwt) {
        return jwtService.find(jwt);
    }

    @GetMapping("/users")
    public List<User> findAll() {
        return userService.findAll();
    }

    @GetMapping("/users/{id}")
    public User findById(@PathVariable String id) {
        try {
            return userService.findById(Long.parseLong(id));
        } catch (Exception e) {
            return null;
        }
    }

    @GetMapping("/users/find")
    public User findByLogin(@NonNull @NotEmpty String login) {
        return userService.findByLogin(login);
    }
}
