<#-- @ftlvariable name="post_id" type="java.lang.String" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#assign post = c.findById(posts, post_id!)!>
    <#if post?has_content>
        <@c.printPost post true/>
    <#else>
        No such post
    </#if>
</@c.page>