<#-- @ftlvariable name="user_id" type="java.lang.String" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#assign user = c.findById(users, user_id!)!>
    <#if user?has_content>
        <@c.printUserPosts user/>
    <#else>
        No such user
    </#if>
</@c.page>