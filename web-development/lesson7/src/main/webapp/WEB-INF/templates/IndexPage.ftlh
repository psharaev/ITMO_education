<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="article">
        <list>
            <template>
                <article>
                    <div class="title"></div>
                    <div class="information">
                        By
                        <span class="userId"></span>
                    </div>
                    <div class="text"></div>
                    <div class="footer">
                        <div class="left">
                            <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                            <span class="positive-score">+173</span>
                            <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
                        </div>
                        <div class="right">
                            <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                            <span class="creationTime"></span>
                            <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                            <a href="#">68</a>
                        </div>
                    </div>
                </article>
            </template>
            <div class="noData">
                <span>Loading...</span>
            </div>
        </list>
    </div>

    <script>
        $(function () {
            const $noData = $(".article .noData");

            $.ajax({
                dataType: "json",
                type: "POST",
                data: {
                    action: "findAllArticles"
                },
                success: function (response) {
                    $noData.find("span").text("No Data");
                    if (response["articles"].length > 0) {
                        $noData.hide();
                    }
                    response["articles"].forEach(function (article) {
                        const $li = $($(".article template").prop("content")).clone();
                        $li.find(".title").text(article["title"]);
                        $li.find(".userId").text(response["idToLogin"][article["userId"]]);
                        $li.find(".text").text(article["text"]);
                        $li.find(".creationTime").text(article["creationTime"]);
                        $(".article list").append($li);
                    });
                }
            });
        })
    </script>
</@c.page>
