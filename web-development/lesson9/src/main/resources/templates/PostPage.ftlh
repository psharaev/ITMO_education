<#-- @ftlvariable name="post" type="ru.itmo.wp.domain.Post" -->
<#-- @ftlvariable name="user" type="ru.itmo.wp.domain.User" -->
<#-- @ftlvariable name="comment" type="ru.itmo.wp.domain.Comment" -->
<#import "commons.ftlh" as c>

<@c.page>
    <#if post??>
        <@c.post post false/>
        <#if user??>
            <div class="form">
                <div class="header">Write Comment</div>
                <div class="body">
                    <form method="post" action="">
                        <input type="hidden" name="action" value="writeComment"/>
                        <div class="field">
                            <div class="name">
                                <label for="text"></label>
                            </div>
                            <div class="value">
                                <textarea id="text" name="text" maxlength="65000" required>${comment.text!}</textarea>
                            </div>
                            <@c.error "comment.text"/>
                        </div>
                        <div class="button-field">
                            <input type="submit" value="Write">
                        </div>
                    </form>
                </div>
            </div>
        </#if>
        <article>
            <div class="title">Comments</div>
        </article>
        <#if post.comments?has_content>
            <#list post.comments as comment>
                <article>
                    <div class="information">By ${comment.user.login}, ${post.creationTime}</div>
                    <div class="body">${comment.text}</div>
                </article>
            </#list>
        <#else>
            <article>
                <div class="body">No data</div>
            </article>
        </#if>
    <#else>
        No such Post
    </#if>
</@c.page>
