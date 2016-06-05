# newlisp-scripts [![Build Status][travis-badge]][travis][![GitHub tag][github-tag]]()[![newLISP versions][newlisp-v]]()

*A collection of command line wrapers, written in newLISP*

[![][newlisp-logo]][newlisp-logo-large]


#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Build & Install](#build--install-)
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

Here are some quick examples:

```
$ bat
```
```
Battery status:
    state:               fully-charged
    percentage:          99%
```

```
$ wifi scan
```
```
*  SSID                 MODE   CHAN  RATE       SIGNAL  BARS  SECURITY
   middleearth          Infra  9     54 Mbit/s  44      ▂▄__  WPA2
   middleearth-guest    Infra  9     54 Mbit/s  44      ▂▄__  WPA2
   middleearth-guest    Infra  149   54 Mbit/s  30      ▂___  WPA2
   Kim's Wi-Fi Network  Infra  11    54 Mbit/s  19      ▂___  WPA2
   NETGEAR67            Infra  11    54 Mbit/s  15      ▂___  WPA2
   Airlink101           Infra  11    54 Mbit/s  14      ▂___  WPA2
   middleearth          Infra  149   54 Mbit/s  30      ▂___  WPA2
*  middleearth          Infra  149   54 Mbit/s  49      ▂▄__  WPA2
```

```
$ wifi join hhonors
```
```
Connecting to SSID hhonors ...
Connection with UUID '50001b43-2b58-406a-a740-ea06d5dea584' created and activated on device 'wlan0'
```

```
$ backlight 95
```
```
94.953052
```
```
$ backlight inc
```
```
100.000000
```


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
[github-tag]: https://img.shields.io/github/tag/oubiwann/newlisp-scripts.svg?maxAge=2592000
[newlisp-v]: https://img.shields.io/badge/newlisp-10.7.0-blue.svg
