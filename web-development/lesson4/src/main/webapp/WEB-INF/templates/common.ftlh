<#macro page>
    <!DOCTYPE html>
    <html lang="ru">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" href="/css/normalize.css">
        <link rel="stylesheet" href="/css/style.css">
        <link rel="icon" href="/favicon.ico">
    </head>
    <body>
    <@header/>
    <div class="middle">
        <@sidebar/>
        <main>
            <#nested/>
        </main>
    </div>
    <@footer/>
    </body>
    </html>
</#macro>

<#macro header>
    <header>
        <a href="#" class="logo"><img alt="Codeforces" title="Codeforces"
                                      src="/img/codeforces-logo-with-telegram.png"></a>
        <div class="languages">
            <a href="#"><img alt="In English" title="In English"
                             src="/img/gb.png"></a>
            <a href="#"><img alt="In Russian" title="In Russian"
                             src="/img/ru.png"></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                <@userlink user false/>
                |
                <a href="/logout">Logout</a>
            <#else>
                <a href="/enter">Enter</a>
                |
                <a href="/register">Register</a>
            </#if>
        </div>
        <nav>
            <#assign items=[
            {"href": "/index", "name": "Home"},
            {"href": "/misc/help", "name": "Help"},
            {"href": "/contests", "name": "Contests"},
            {"href": "/users", "name": "Users"}
            ]/>
            <ul>
                <#list items as item>
                    <li>
                        <@navMenuItem (item.href == URI), item!/>
                    </li>
                </#list>
            </ul>
        </nav>
    </header>
</#macro>

<#macro sidebar>
    <aside>
        <@printAsidePosts/>
    </aside>
</#macro>

<#macro footer>
    <footer>
        2010-2021 &copy; by Mike Mirzayanov
    </footer>
</#macro>

<#macro userlink user>
<#-- @ftlvariable name="user" type="ru.itmo.web.lesson4.model.User" -->
    <a href="/user?handle=${user.handle}">${user.name}</a>
</#macro>

<#function findBy items key value>
    <#list items as item>
        <#if item[key]?string == value?string>
            <#return item/>
        </#if>
    </#list>
</#function>

<#--task 1-->
<#macro userColoredRef handle color>
    <a class="${color}" href="/user?handle=${handle}">${handle}</a>
</#macro>

<#macro userNameOnlyRef handle name>
    <a href="/user?handle=${handle}">${name}</a>
</#macro>

<#macro userlink user colored>
    <#if colored>
        <@userColoredRef user.handle, user.color/>
    <#else>
        <@userNameOnlyRef user.handle user.handle/>
    </#if>
</#macro>

<#function getIndex items value>
    <#list items as item>
        <#if item == value>
            <#return item?index/>
        </#if>
    </#list>
</#function>

<#--task 2-->
<#macro navMenuItem isUnderlined item>
    <#if isUnderlined>
        <a class="current" href="${item.href}">${item.name}</a>
    <#else>
        <a href="${item.href}">${item.name}</a>
    </#if>
</#macro>

<#--task 4-->
<#macro printAllPosts>
    <#list posts?reverse as post>
        <@printPost post false/>
    </#list>
</#macro>

<#macro printPost post printFull>
    <article>
        <div class="title">${post.title}</div>
        <#assign user = findById(users, post.userId)!/>
        <div class="information">By
            <@userlink user true/>, 2 days ago, translation
        </div>
        <div class="body">
            <@limitText post.text printFull/>
        </div>
        <ul class="attachment">
            <li>Announcement of <a href="#">Codeforces Round #510 (Div. 1)</a></li>
            <li>Announcement of <a href="#">Codeforces Round #510 (Div. 2)</a></li>
        </ul>
        <div class="footer">
            <div class="left">
                <img src="/img/voteup.png" title="Vote Up" alt="Vote Up"/>
                <span class="positive-score">+173</span>
                <img src="/img/votedown.png" title="Vote Down" alt="Vote Down"/>
            </div>
            <div class="right">
                <img src="/img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                2 days ago
                <img src="/img/comments_16x16.png" title="Comments" alt="Comments"/>
                <a href="#">68</a>
            </div>
        </div>
    </article>
</#macro>

<#function findById items id>
    <#list items as item>
        <#if item.id?string == id?string>
            <#return item>
        </#if>
    </#list>
</#function>

<#macro limitText text printFull>
    <#if text?length <= 250 || printFull>
        <p>${text}</p>
    <#else>
        <p>${text?substring(0, 250) + "..."}</p>
    </#if>
</#macro>

<#--task 5-->
<#macro printAsidePosts>
    <#list posts?reverse as post>
        <section>
            <div class="header">
                Post #${post.id}
            </div>
            <div class="body">
                <@limitText post.text false/>
            </div>
            <div class="footer">
                <a href="/post?post_id=${post.id}">View all</a>
            </div>
        </section>
    </#list>
</#macro>

<#macro printUserPosts user>
    <#if user?hasContent>
        <#list user.getPosts()?reverse as post>
            <@printPost post false/>
        </#list>
    </#if>
</#macro>