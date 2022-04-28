# clockodo-el

This emacs package provides a basic integration of the [clockodo](https://clockodo.com/)
service. It provides two ways to interact with the API. On the one hand, it provides
a basic minor-mode to see the current clock state and a command to interact with the clock.
On the other hand, it provides a bunch of commands to generate reports or show raw information
from the API. This is mostly written from the employee perspective and mostly uses reading 
requests to interact with the service. To change time information uses the webpage, at the moment. 

## Installation

### Melpa (TODO)

The simplest way is to install the package directly from melpa with
`package-install RET clockodo RET`. 

Or use `use-package` with:
```emacs-lisp
(use-package clockodo)
```

### Manual

This implementation depends on the libraries:
  * `request > 0.3.2` for simple http/s requests
  * `ts > 0.2.2` for simple timestamp parsing and formating
  * `org > 8` for table handling and date picking

Download this repository and add the `clockodo.el` file to you load-path.
Afterwards, run `(require clockodo)` to make it available.

## Usage

Use `M-x customize-group RET clockodo` to see all customization options.

### Minor mode

To enable the minor mode use `M-x clockodo-mode` this enables the background timer
to regulary request the clock state from the clockodo API. This enables the usage of other
applications to interact with clockodo alongside emacs.

When the minor mode is enabled the keys are bound to the prefix `C-c C-#`. 

### The clock

To active or deactivate the clock use `M-x clockodo-toggle-clock`.  
On activation this either starts the clock or gets the current running id.  
On deactivation this only stops the clock if it's currently running.

### Reports

The package provides the following time reports:

  * `clockodo-print-daily-report`
  * `clockodo-print-weekly-report`
  * `clockodo-print-monthly-report` (TODO)
  * `clockodo-print-overall-report` (TODO)

Each report provides the quick bindings:

  * `g` - refresh buffer
  * `c` - choose report time
  * `n` - next report
  * `p` - previous report

General requests which return raw results from the API can be triggered with
the function `clockodo-show-information`.

## Contribution

Feel free to contribute to this project. Pull requests with code improvements,
new features or something else are highly welcome. Since this is my first greater emacs
package feel free to give general hints on idiomatic emacs-lisp and style. 

## License

GPLv3 but TODO
