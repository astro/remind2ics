# Motivation

After eleven years of using [remind](https://www.roaringpenguin.com/products/remind) + [wyrd](http://pessimization.com/software/wyrd/) I'm switching to [Radicale](http://radicale.org/) + [DAVdroid](https://davdroid.bitfire.at/what-is-davdroid) for the following reasons:

* I want to share calendars with other users
* I fancy fancy GUIs
* I'm tired of operating ssh on mobile devices

# Installation

```shell
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal configure
cabal build
```

# Usage

```shell
./dist/build/remind2icalendar/remind2icalendar < .reminders > reminders.ics
```
