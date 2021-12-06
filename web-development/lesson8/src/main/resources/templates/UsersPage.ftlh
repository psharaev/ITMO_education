<#-- @ftlvariable name="users" type="ru.itmo.wp.lesson8.domain.User[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th>Created</th>
                <th>Enable/Disable</th>
            </tr>
            </thead>
            <tbody>
            <#if users?size==0>
                <tr class="noData">
                    <td colspan="3">
                        No data
                    </td>
                </tr>
            <#else>
                <#list users as user>
                    <tr>
                        <td class="id">${user.id}</td>
                        <td class="login">${user.login}</td>
                        <td class="creationTime">${user.creationTime}</td>
                        <#if user.disabled>
                            <td class="disabled">
                                <form method="post" action="/users/enable/${user.id}">
                                    <input type="submit" value="enable">
                                </form>
                            </td>
                        <#else>
                            <td class="enabled">
                                <form method="post" action="/users/disable/${user.id}">
                                    <input type="submit" value="disable">
                                </form>
                            </td>
                        </#if>
                    </tr>
                </#list>
            </#if>
            </tbody>
        </table>
    </div>
</@c.page>
