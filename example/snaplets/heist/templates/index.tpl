<!DOCTYPE html>    
<html>
    <head>
    </head>
    
    <body>
        
        <h2>Test I18N</h2>
        
        <label><testSplice /></label>
        
        <label><i18n name="shanghai"></i18n></label>
        <p>
            <i18nSpan name="hello" class="test"></i18nSpan>
        </p>
        <p><i18n name="invalidekey"></i18n></p>
        
        <p>
            <i18n name="shanghai">
                <input type="submit" value="${i18nValue}" />
            </i18n>
        </p>
        
    </body>
</html>
