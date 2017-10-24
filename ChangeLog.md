# Revision history for heavy-logger

## 0.1.0.0  -- 2017-10-15

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2017-10-16

* Major rewrite of internal interfaces. Incompatible API changes.
  Was not released to Hackage.
* Added the Shortcuts module.

## 0.3.0.0 -- 2017-10-24

* Introduced parallel logging backend.
* Introduced filtering logging backend.
* Logging context stacks support introduced.
* More flexible severity levels system. This is incompatible API change.
  Conversion functions are provided for integration with monad-logger's
  LogLevel type.
* Internal modules rearrangement.
* Added the TH module.
* Added the IO module.
* Introduced null logging backend.

