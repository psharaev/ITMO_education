<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <#if user??>
        <div class="articles datatable">
            <div class="caption">Articles</div>
            <table>
                <thead>
                <tr>
                    <th>Id</th>
                    <th>Title</th>
                    <th>Hide/Show</th>
                </tr>
                </thead>
                <tbody>
                <template>
                    <tr>
                        <td class="id"></td>
                        <td class="title"></td>
                        <td>
                            <div class="hide-button">
                                <input type="button" class="hidden" value="Hide">
                            </div>
                        </td>
                    </tr>
                </template>
                <tr class="noData">
                    <td colspan="3">
                        <span>Loading...</span>
                    </td>
                </tr>
                </tbody>
            </table>
        </div>
        <script>
            $(function () {
                const $noData = $(".articles .noData");

                $.ajax({
                    dataType: "json",
                    type: "POST",
                    data: {
                        action: "findMyArticles"
                    },
                    success: function (response) {
                        $noData.find("span").text("No Data");
                        if (response["articles"].length > 0) {
                            $noData.hide();
                        }
                        response["articles"].forEach(function (article) {
                            let $tr = $($(".articles template").prop("content")).clone();
                            $tr.find(".id").text(article["id"]);
                            $tr.find(".title").text(article["title"]);
                            $tr.find("input").attr({"id": article["id"]});
                            $tr.find("input").attr({"value": article["hidden"] ? "Show" : "Hide"});
                            $(".articles tbody").append($tr);
                        });
                    }
                });
            })

            $(function () {
                $(".articles").on("click", ".hide-button", function () {
                    const $root = $(this);
                    $.ajax({
                        dataType: "json",
                        type: "POST",
                        data: {
                            action: $root.find("input").val() === "Show" ? "setShown" : "setHidden",
                            id: $root.parent().parent().find(".id").text()
                        },
                        success: function (response) {
                            if (typeof response["hidden"] === 'undefined' || response["hidden"] !== null) {
                                $root.find("input").val(response["hidden"] ? "Show" : "Hide");
                                notify("Your article is " + (response["hidden"] ? "hidden" : "shown"));
                            }
                        }
                    });
                });
            })
        </script>
    <#else>
        <p>You must be logged, to watch your articles</p>
    </#if>
</@c.page>
