[![Build Status](https://secure.travis-ci.org/HaskellCNOrg/snaplet-i18n.png?branch=master)](http://travis-ci.org/HaskellCNOrg/snaplet-i18n)

## snaplet-i18n

  1. create config below into `data/message_en.cfg`

~~~
hello = "Hello"
shanghai = "ShangHai"
~~~

  2. use tag in heist template

~~~
<i18n name="shanghai"></i18n>
<i18nSpan name="shanghai"></i18n>
<i18n name="shanghai">
  <a><i18nValue /></a>
</i18n>
~~~

**see example at example dir**

## Snaplet

- [what is snaplet]

[what is snaplet]: http://snapframework.com/docs/tutorials/snaplets-tutorial

## TODO

- maybe multiple locale support at run time.
- use getDataDir to retrieve locale msg. [snaplet tutorial](http://snapframework.com/docs/tutorials/snaplets-tutorial)
  
