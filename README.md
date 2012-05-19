
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
~~~

**see test at test/snap.hs***

## Snaplet

- [what is snaplet]

[what is snaplet]: http://snapframework.com/docs/tutorials/snaplets-tutorial
