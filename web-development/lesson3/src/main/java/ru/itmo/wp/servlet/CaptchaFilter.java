package ru.itmo.wp.servlet;

import ru.itmo.wp.util.ImageUtils;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.*;

import java.util.Base64;

public class CaptchaFilter extends HttpFilter {

    @Override
    protected void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpSession session = request.getSession();
        System.out.println(request.getRequestURI());
        if (!"pass".equals(session.getAttribute("captchaPass"))) {
            if (session.getAttribute("serverCode") == null) {
                session.setAttribute("firstUri", request.getRequestURI());
                sendCaptcha(request, response);
                return;
            }

            String userCode = request.getParameter("userCode");

            if (session.getAttribute("serverCode").equals(userCode)) {
                session.setAttribute("captchaPass", "pass");
                response.sendRedirect((String) session.getAttribute("firstUri"));
            } else {
                sendCaptcha(request, response);
            }

            return;
        }

        if ("/captcha".equals(request.getRequestURI())) {
            response.sendRedirect((String) session.getAttribute("firstUri"));
            return;
        }

        chain.doFilter(request, response);
    }

    private void sendCaptcha(HttpServletRequest request, HttpServletResponse response) throws IOException {
        HttpSession session = request.getSession();
        String serverCode = (String) session.getAttribute("serverCode");
        if (session.getAttribute("serverCode") == null) {
            serverCode = String.valueOf(((int) (Math.random() * 1000) % 900) + 100);
            session.setAttribute("serverCode", serverCode);
        }
        session.setAttribute("captchaPass", "process");

        OutputStream stream = response.getOutputStream();
        stream.write("<div><img src=\"data:image/png;base64,".getBytes());
        stream.write(Base64.getEncoder().encode(ImageUtils.toPng(serverCode)));
        stream.write("\"></div>\n".getBytes());
        stream.write(("<div class=\"captcha-form\">" +
                "    <form action=\"captcha\" method=\"post\">" +
                "        <input name=\"userCode\" type=\"text\">" +
                "    </form>" +
                "</div>").getBytes());
    }
}