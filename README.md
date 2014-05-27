coverlay.el
================================

Test coverage overlay for Emacs


DESCRIPTION
---------------------------------------
`coverlay.el` is an emacs plugin to highlight untested lines.

![coverlay demo](https://raw.githubusercontent.com/twada/coverlay.el/master/img/coverlay_demo.png "coverlay demo")

`coverlay.el` supports widely used [LCOV format](http://ltp.sourceforge.net/coverage/lcov/geninfo.1.php). (ex. [Coveralls](https://coveralls.io/))

Please note that `coverlay.el` is a beta version product. Pull-requests, issue reports and patches are always welcomed.


HOW TO USE
---------------------------------------

### configure

Load coverlay.el in your .emacs

```lisp
(require 'coverlay)
```

### use

First, load lcov file in your project into coverlay buffer.

    M-x coverlay-load-file /path/to/lcov-file

Open target code then toggle overlay.

    M-x coverlay-toggle-overlays


AUTHOR
---------------------------------------
* [Takuto Wada](http://github.com/twada)


LICENSE
---------------------------------------
Licensed under the GPLv3 license.
