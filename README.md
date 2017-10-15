heavy-logger README
===================

This is Haskell logging library, which prefers functionality and extendability over light weight and simplicity.
It can use [fast-logger][1] as backend and is compatible with [monad-logger][2] interface, so it can be used in [WAI][3] projects.
heavy-logger is integrated with [text-format-heavy][4] string formatting library.

Most notable features of heavy-logger are:

* Several backends and possibility to write your own backends. The provided backends are:
  * Fast-logger backend. It allows to write messages to stdout, stderr or arbitrary file.
  * Syslog backend.
  * Chan backend. Writes messages to a Chan, so they can be read from the other side.
* Possibility to define log message format in the output file. For example, do you want to
  see event severity level first, and then time, or vice versa?
* Possibility to easily set up message filtering based on message source and severity level.
  For example, you may want to write only Info messages, but also Debug messages from one module.
* Text formatting library integration. Formatting of messages by `text-format-heavy` is done lazily;
  so, you can issue a lot of debug messages, that include data that take time to present as a string;
  the formatting will be executed only in the case when debug output for this module is actually enabled
  by the filter.

[1]: https://hackage.haskell.org/package/fast-logger
[2]: https://hackage.haskell.org/package/monad-logger
[3]: https://hackage.haskell.org/package/wai
[4]: https://hackage.haskell.org/package/text-format-heavy
