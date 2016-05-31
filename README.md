# newlisp-scripts [![Build Status][travis-badge]][travis]

*A collection of command line wrapers, written in newLISP*

[![][newlisp-logo]][newlisp-logo-large]


#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Build & Install](#build-install-)
* [Running](#running-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

This repo provides a collection of command line utilities that wrap various other
command line utilities that I use, though providing a much simpler (and
easier-to-remember) user experience.

So far, this repo provides the following executables:

* ``wifi`` - A wrapper for a ``nmcli`` (just the access points list and the connect command)
* ``bat`` - A wrapper for the combination of commands necessary to extract current laptop battery status
* ``backlight`` - A wrapper for ``xbacklight``


## Dependencies [&#x219F;](#contents)

All you need is newLISP and GNU make. You will not be able to build or run the scripts unless you have run ``make install`` in your newLISP build directory.

The scripts themselves use the following libraries which come with newLISP:

* [getopts](http://www.newlisp.org/code/modules/getopts.lsp.html)


## Build & Install [&#x219F;](#contents)

As long as you have ``newlisp`` installed (including its modules) and on your ``PATH``,
you may build with this command:

```
$ make
```

(or ``make compile``) and then install with this command:

```
$ sudo make install
```

**Be careful** with that one! If you have files in ``INSTALL_DIR`` with the same name as the
binaries created with ``make compile``, they will be **overwritten**.

Note if you are running on *BSD, you will need to use ``gmake`` instead of ``make``.


## Running [&#x219F;](#contents)

Once installed, running the scripts is just like any other *NIX executable. All scripts
support the ``-h`` option that you can examine for their simple usage.


## License [&#x219F;](#contents)

```
Copyright © 2016 Duncan McGreggor

Distributed under the BSD 2-Clause License.
```


<!-- Named page links below: /-->

[travis]: https://travis-ci.org/oubiwann/newlisp-scripts
[travis-badge]: https://travis-ci.org/oubiwann/newlisp-scripts.png?branch=master
[newlisp-logo]: resources/images/logo-white-small.png
[newlisp-logo-large]: resources/images/logo-white.png
