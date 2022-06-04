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

As credentials provide the user email and the api token which is located under `my data`.

### Minor mode

To enable the minor mode use `M-x clockodo-mode` this enables the background timer
to regulary request the clock state from the clockodo API. This enables the usage of other
applications to interact with clockodo alongside emacs.

When the minor mode is enabled the keys are bound to the prefix `C-c C-#`. 

### The clock

To active or deactivate the clock use `M-x clockodo-toggle-clock`.  
On activation this either starts the clock or gets the current running id.  
On deactivation this only stops the clock if it's currently running.
To select from the active services provided by the company use the prefix key
(`C-u`) before starting the clock.

### Configuration

This mode can be configured through the `customize` feature of emacs.
It includes changing faces, colors and other minor adjustments.

- Set your own default service id (which exists in clockodo) through `clockodo-service-id` 
instead of using the company defined one.

### Reports

The package provides the following time reports:

  * `clockodo-print-daily-report`
  * `clockodo-print-weekly-report`
  * `clockodo-print-monthly-report`
  * `clockodo-print-overall-report`

Each report provides the quick bindings:

  * `g` - refresh buffer
  * `c` - choose report time
  * `n` - next report
  * `p` - previous report
  
Special bindings:

  * `w` - jump to week from a daily report
  * `m` - jump to month from daily or weekly buffer
  * `y` - jump to year from daily, weekly or monthly buffer

General requests which return raw results from the API can be triggered with
the function `clockodo-show-information`.

### TODOS

This package models only the employee view and only the points necessary for me.
There are many more open tasks which can be nicely integrated into this package.
Feel free to open an issue with a missing point or a pull-requests with the code.

- [ ] ~~Define the default customer~~ or let the user choose the customer for time track
- [ ] Modify time tracks
- [ ] Show abscene as report
- [ ] Use `thing-at-point` to switch between reports
- [ ] Better error handling
- [ ] Show abscene in reports

## Contribution

Feel free to contribute to this project. Pull requests with code improvements,
new features or something else are highly welcome. Since this is my first greater emacs
package feel free to give general hints on idiomatic emacs-lisp and style. 

## License

Copyright (c) 2022 Henrik Jürges <ratzeputz@rtzptz.xyz>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
