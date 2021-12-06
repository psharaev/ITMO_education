<#import "commons.ftlh" as c>

<@c.page>
    <div class="entrance form-box">
        <div class="header">Enter</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="enter"/>
                <div class="field">
                    <div class="name">
                        <label for="login">Login</label>
                    </div>
                    <div class="value">
                        <input id="login" name="login"/>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="password">Password</label>
                    </div>
                    <div class="value">
                        <input id="password" name="password" type="password"/>
                    </div>
                </div>
                <div class="error"></div>
                <div class="button-field">
                    <input type="submit" value="Enter">
                </div>
            </form>
        </div>
    </div>
    <script>
        $(function () {
            $(".entrance form").submit(function () {
                const login = $(this).find("input[name='login']").val();
                const password = $(this).find("input[name='password']").val();
                const $error = $(this).find(".error");

                ajax({
                    action: "enter",
                    login,
                    password
                }, $error);

                return false;
            });
        })
    </script>
</@c.page>
