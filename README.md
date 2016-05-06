coverlay.el
================================

Test coverage overlay for Emacs


DESCRIPTION
---------------------------------------
`coverlay.el` is an emacs plugin to highlight untested lines.

![coverlay demo](https://raw.githubusercontent.com/twada/coverlay.el/master/img/coverlay_demo.png "coverlay demo")

`coverlay.el` supports widely used [LCOV format](http://ltp.sourceforge.net/coverage/lcov/geninfo.1.php). (for example, [Coveralls](https://coveralls.io/) uses lcov format)

Please note that `coverlay.el` is a beta version product. Pull-requests, issue reports and patches are always welcomed.


HOW TO USE
---------------------------------------

### configure

Load coverlay.el in your .emacs

```lisp
(require 'coverlay)
```

### use

Coverlay contains a global mode that will highlight missed lines on every covered file.

    M-x coverlay-mode


Load a lcov file and keep watching it for changes.

    M-x coverlay-watch-file /path/to/lcov-file


Alternativly just read the coverage file once.

    M-x coverlay-load-file /path/to/lcov-file

This command is bound to "M-o f" by default.

And reread current coverage file

    M-x coverlay-reload-file

This command is bound to "M-o g" by default.


Coverage information about all files included in the coverage file can be displayed in a list.

    M-x coverlay-display-stats

This command is bound to "M-o s" by default.


You can also manually toggle the overlays in the current buffer.

    M-x coverlay-toggle-overlays

This command is bound to "M-o t" by default.


### customization

Some values can be customized via customize. This includes the possibility to set a base path if the filenames in the coverage file are relative.



AUTHOR
---------------------------------------
* [Takuto Wada](http://github.com/twada)


CONTRIBUTORS
---------------------------------------
* [syohex](https://github.com/syohex)
* [FossiFoo](https://github.com/fossifoo)


LICENSE
---------------------------------------
Licensed under the GPLv3 license.
