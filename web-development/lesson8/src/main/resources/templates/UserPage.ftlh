<#import "commons.ftlh" as c>

<@c.page>
    <#if user??>
        <div class="user">
            <div class="data">
                <div class="info">
                    <ul>
                        <li>id: ${user.id}</li>
                        <li>login: ${user.login}</li>
                        <li>creation time: ${user.creationTime}</li>
                    </ul>
                </div>
            </div>
        </div>
    <#else>
        No such user
    </#if>
</@c.page>
