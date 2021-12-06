<#-- @ftlvariable name="user_id" type="java.lang.String" -->
<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if handle?has_content>
        <#assign user = c.findBy(users, "handle", handle!)!/>
    <#else>
        <#assign user = c.findBy(users, "id", user_id!)!/>
    </#if>

    <#if user?has_content>
        <div class="user">
            <div class="data">
                <div class="info">
                    <h2>
                        <@c.userlink user true/>
                    </h2>
                    <ul>
                        <li>id: ${user.id}</li>
                        <li>handle: ${user.handle}</li>
                        <li>name: ${user.name}</li>
                        <li><a href="/posts?user_id=${user.id}">posts: ${user.getPostCount()}</a></li>
                    </ul>
                </div>
                <div class="aside">
                    <img src="/img/no-title.jpg" alt="no-title" class="icon">
                </div>
            </div>
            <div class="footer">
                <#assign index = c.getIndex(users, user)!>
                <div class="left-arrow">
                    <#if (index  > 0)>
                        <@c.userNameOnlyRef users[index - 1].handle "⟵"/>
                    <#else>
                        <p>⟵</p>
                    </#if>
                </div>
                <div class="right-arrow">
                    <#if (index < users?size - 1)>
                        <@c.userNameOnlyRef users[index + 1].handle "⟶"/>
                    <#else>
                        <p>⟶</p>
                    </#if>
                </div>
            </div>
        </div>
    <#else>
        No such user
    </#if>
</@c.page>
