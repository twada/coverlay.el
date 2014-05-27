coverlay.el
================================

Code Coverage Overlay for Emacs


DESCRIPTION
---------------------------------------
`coverlay.el` is a emacs-lisp to highlight lines not covered by tests.

`coverlay.el` supports lcov format that is widely used among CI services. (ex. Coveralls)

![coverlay demo](https://raw.githubusercontent.com/twada/coverlay.el/master/img/coverlay_demo.png "coverlay demo")

Please note that `coverlay.el` is a beta version product. Pull-requests, issue reports and patches are always welcomed.


HOW TO USE
---------------------------------------

### configure

Load coverlay.el in your .emacs

     (require 'coverlay)


### use

Load lcov file into coverlay buffer

    M-x coverlay-load-file /path/to/lcov-file

Toggle overlay

    M-x coverlay-toggle-overlays


AUTHOR
---------------------------------------
* [Takuto Wada](http://github.com/twada)


LICENSE
---------------------------------------
Licensed under the GPLv3 license.
