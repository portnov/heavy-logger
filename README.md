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
* Logging backend settings can be defined dynamically; it is not necessary to
  hardcode which backend you will use, you can load settings in runtime.
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

All functions provided by the package work within any monad, which should be an
instance of one of type classes defined by package: `HasLogger`, `HasLogBackend`,
`HasLogContext`. Each function's signature declares only specific constraint, so
if you do not need all functionality, you can implement instances only of that 
classes that you actually need.

There are, in general, following ways to use this package:

* Use `LoggingT` monad transformer. It can be the simplest, if you already have
  monadic transformers stack of 1-2 transformers and you do not mind to add yet
  another. With `LoggingT`, you do not need to write any adapter instances, since
  `LoggingT` is already an instance of all required classes. This implementation
  automatically solves all threading-related problems, since in fact it does not
  have any shared state.
* Use `System.Log.Heavy.IO` module. If you do not have monadic transformers at all,
  and your application works in pure IO, this may be the simplest way. However,
  this is a bit fragile, because you have to be sure that you always call logging
  functions only when logging state is initialized, i.e. within `withLoggingIO`
  call. This implementation stores required state in thread-local storage.
* Implement required class instances for monadic stack that you already use in
  your application. For example, if you already have something like
  `ReaderT StateT ExceptT IO`, it will be probably better to add a couple of 
  fields to StateT's state to track logging state, than change your stack to
  `ReaderT StateT LoggingT ExceptT IO`. If you wish to store logging state in some
  kind of shared storage (global IORef or whatever), then you should think about
  thread-safety by yourself.

Please refer to Haddock documentation and examples in the `examples/` directory
for more detailed information.

[1]: https://hackage.haskell.org/package/fast-logger
[2]: https://hackage.haskell.org/package/monad-logger
[3]: https://hackage.haskell.org/package/text-format-heavy

