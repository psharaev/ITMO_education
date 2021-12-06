<#-- @ftlvariable name="user" type="ru.itmo.wp.model.domain.User" -->
<#import "commons.ftlh" as c>

<@c.page>
    <#if user??>
        <div class="form-box">
            <div class="header">Create article</div>
            <div class="body">
                <form class="send-article" method="post" action="">
                    <input type="hidden" name="action" value="create"/>
                    <div class="field">
                        <div class="name">
                            <label for="title">Title</label>
                        </div>
                        <div class="value">
                            <input id="title" name="title"/>
                        </div>
                    </div>
                    <p>
                        <label>
                            <textarea class="text-area" name="text" cols="44" rows="16" required></textarea>
                        </label>
                    </p>
                    <div class="error"></div>
                    <div class="button-field">
                        <input type="submit" value="Create">
                    </div>
                </form>
            </div>
        </div>
        <script>
            $(function () {
                $(".form-box form").submit(function () {
                    const title = $(this).find("input[name='title']").val();
                    const text = $(this).find("textarea[name='text']").val();
                    const $error = $(this).find(".error");

                    ajax({
                        action: "create",
                        title,
                        text
                    }, $error);

                    return false;
                });
            })
        </script>
    <#else>
        <p>You must be logged, to create article</p>
    </#if>
</@c.page>
