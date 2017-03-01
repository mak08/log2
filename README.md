# log2
Simple logging by package and function

Includes code from log4cl written by Max Mikhanosha. It lacks most of the features of log4cl but provides timestamps with millisecond resolution (via local-time). Supports SBCL and CCL.

## Installation

```
(ql:quickload 'bordeaux-threads)
(ql:quickload 'local-time)
(ql:quickload 'cl-utilities)

(asdf:load-system :log2)
```
