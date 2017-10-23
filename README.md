heavy-logger README
===================

This is Haskell logging library, which prefers functionality and extendability
over light weight and simplicity.
It can use [fast-logger][1] as backend and is compatible with [monad-logger][2]
interface, so it can be used in projects that already use monad-logger.
heavy-logger is integrated with [text-format-heavy][3] string formatting library.

Most notable features of heavy-logger are:

* Several backends and possibility to write your own backends. The provided
  backends are:
  * Fast-logger backend. It allows to write messages to stdout, stderr or
    arbitrary file.
  * Syslog backend.
  * Chan backend. Writes messages to a Chan, so they can be read from the other
    side.
* Possiblity to write messages to several backends in parallel.
* Sane default set of logging message severity levels and possibility to define
  custom severity levels.
* Logging context stacks support (aka mapped diagnostic contexts, MDC). Each
  logging context stack frame contains a set of named variables. These
  variables can be writen to the log.
* Possibility to define log message format in the output file. For example, do
  you want to see event severity level first, and then time, or vice versa? It
  is possible to use variables from logging context stack in the formatting
  string.
* Flexible events filtering mechanism. Messages are filtered based on message
  source and severity level.  For example, you may want to write only Info
  messages, but also Debug messages from one module.  Filtering can be
  performed on two stages:
  * Before event is passed to backend. This stage is context-sensitive; for
    example, you can enable logging only for events that happened during
    transaction. Context-level filters also support negation; for example, it
    is possible to explicitly forbid debug messages from one contexts, while
    debug is enabled for the whole system.
  * In the backend. For example, you can forbid to write any debug into file,
    but allow to write all debug into syslog.
* Text formatting library integration. Formatting of messages by
  `text-format-heavy` is done lazily; so, you can issue a lot of debug
  messages, that include data that take time to present as a string; the
  formatting will be executed only in the case when debug output for this
  module is actually enabled by the filter.

This package is mostly writen with ideas of "open architecture". It exposes all
internal logical pieces, so they can be combined in other order in specific
applications.

Please refer to Haddock documentation and examples in the `examples/` directory
for more detailed information.

[1]: https://hackage.haskell.org/package/fast-logger
[2]: https://hackage.haskell.org/package/monad-logger
[3]: https://hackage.haskell.org/package/text-format-heavy
