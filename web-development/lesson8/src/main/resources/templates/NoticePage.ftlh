<#import "/spring.ftl" as spring>
<#import "commons.ftlh" as c>

<@c.page>
<div class="form-box">
    <div class="header">New Notice</div>
    <div class="body">
        <form method="post" action="/notice">
            <div class="field">
                <div class="name">
                    <label for="content">Text</label>
                </div>
                <div class="value">
                    <textarea id="content" rows="10" name="content"></textarea>
                </div>
                <@c.error "noticeForm.content"/>
            </div>
            <div class="button-field">
                <input type="submit" value="Add">
            </div>
        </form>
    </div>
</div>
</@c.page>
