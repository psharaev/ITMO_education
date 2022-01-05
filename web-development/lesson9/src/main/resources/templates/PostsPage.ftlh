<#import "commons.ftlh" as c>

<@c.page>
<#list user.posts as post>
    <@c.post post true/>
</#list>
</@c.page>
