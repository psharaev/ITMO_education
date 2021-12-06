package ru.itmo.web.lesson4.util;

import ru.itmo.web.lesson4.model.Post;
import ru.itmo.web.lesson4.model.User;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class DataUtil {
    public static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirzayanov", "Mike Mirzayanov", User.Color.BLUE),
            new User(6, "pashka", "Pavel Mavrin", User.Color.GREEN),
            new User(9, "geranazarov555", "Georgiy Nazarov", User.Color.RED),
            new User(11, "tourist", "Gennady Korotkevich", User.Color.GREEN)
    );

    public static final List<Post> POSTS = Arrays.asList(
            new Post(1, "ICPC WF Moscow Invitational Contest — Online Mirror", "Hello, everybody!\n" +
                    "\n" +
                    "We would like to invite you to participate in the Mirror of The ICPC World Finals Moscow Invitational Contest. The original contest was made for the teams who cannot come to The World Finals in Moscow. They were competing for the medals and glory. The Invitational contest has already passed and the results will be revealed on October 5th.\n" +
                    "The mirror contest will be held by almost standard ICPC rules with 10 to 14 problems. The difference from the traditional ICPC format is that your team can use three computers instead of one. The problems are expected to be of The World Finals difficulty. Also, the round will be unrated!",
                    1),
            new Post(2, "Codeforces Round #745", "Hello Codeforces!\n" +
                    "\n" +
                    "CQXYM and I are glad to invite you to Codeforces Round #745 (Div. 1) and Codeforces Round #745 (Div. 2), which will be held on четверг, 30 сентября 2021 г. в 13:15. Note the unusual time of the round.\n" +
                    "Each division will have 6 problems and 2 hours to solve them. All problems were written and prepared by CQXYM and me. The round will be rated for both divisions.\n" +
                    "We would like to thank:\n" +
                    "Aleks5d for his excellent round coordination and many useful suggestions that greatly help with preparation.\n" +
                    "1-gon, voidmax, alexX512, ijxjdjd, m371, makogon2907, chctxdy68, Hemose, Hinai_Paulette, Omkar, 4qqqq, FBKdaisuki, kassutta, Mo2men and pkpawan for testing the round and providing useful feedback.\n" +
                    "MikeMirzayanov for great platforms Codeforces and Polygon.\n" +
                    "And you, for participating!\n" +
                    "This is our first round, and great efforts have been put into preparing this round. Were you to kindly participate in this round, we would be very grateful and hope you will enjoy it.",
                    9),
            new Post(3, "Competitive Programming Hall of Fame — cphof.org", "Немного истории\n" +
                    "Довольно давно я обнаружил, что в Интернете нет единого источника информации, который содержал бы результаты международных чемпионатов по спортивному программированию.",
                    6),
            new Post(4, "Codeforces Round #744 (Div. 3)", "Всем привет!", 11));

    public static void addData(HttpServletRequest request, Map<String, Object> data) {
        data.put("users", USERS);
        data.put("posts", POSTS);

        HttpSession session = request.getSession();

        if (request.getRequestURI().contains("/logout")) {
            session.setAttribute("user", null);
        }

        if (session.getAttribute("user") != null) {
            data.put("user", session.getAttribute("user"));
        }

        for (User user : USERS) {
            if (Long.toString(user.getId()).equals(request.getParameter("logged_user_id"))) {
                data.put("user", user);
                session.setAttribute("user", user);
            }
        }
    }
}
