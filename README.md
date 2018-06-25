# Whitespace Final Newline

The `whitespace-final-newline` Emacs package adds the
`whitespace-final-newline-mode` minor mode, which highlights when a buffer
does not end with a `newline`.

To use this package, enable the `whitespace-final-newline-mode` minor mode
in the buffers in which you wish to use it, or to enable it for all
buffers, customize `global-whitespace-final-newline-mode` to `t`.

The default configuration puts the string " <-- No final newline" in an
overlay at the end of a buffer when the buffer does not end with a
newline.  This can be changed by customizing
`whitespace-final-newline-message`.

The default configuration highlights the message with a magenta
background.  This can be changed by customizing the
`whitespace-final-newline` face.

The default configuration does not show the message when the point is at
the end of the buffer.  This can be changed by customizing
`whitespace-final-newline-no-message-when-point-at-eob`.

Changes to `whitespace-final-newline-message` take effect only when the
`whitespace-final-newline-mode` minor mode is being turned on.  Thus, you
may need to toggle the mode off and on again in affected buffers before
you see the effect of any configuration changes to
`whitespace-final-newline-message`.

In the long term, I would like to have this feature implemented by the
official `whitespace.el` file that comes with Emacs.

## Source

The source code for this package is available at:
  <https://bitbucket.org/adamsmd/whitespace-final-newline/>

## License

This package is licensed under the GPL 3.0 license.  See the LICENSE file
in the source code.

## Installation

- Install the package files

    - Option 1 (preferred): Install from MELPA

        - Enable installation of packages from MELPA (see
          <http://melpa.org/#/getting-started>).

        - Launch Emacs's package manager from the menu with `Options ->
          Manage Emacs Packages` or manually with `M-x
          package-list-packages`.

        - Select the `whitespace-final-newline` package for installation.

    - Option 2: Install manually

        - Download the source code with:
          `git clone git@bitbucket.org:adamsmd/whitespace-final-newline.git`

        - Copy `whitespace-final-newline.el` into a directory in your `load-path`.

        - Add `(require 'whitespace-final-newline)` to your `.emacs`.

- Enable the `whitespace-final-newline` minor mode in the buffers in which
  you wish to use it, or to enable it for all buffers, customize
  `global-whitespace-final-newline-mode` to `t`.

## Limitations

- This mode depends on the conditional formatting of overlays provided by
  Emacs.  When `whitespace-final-newline-no-message-when-point-at-eob` is
  `t` (the default), some cursor movements may not cause the visibility of
  the message to change when they should.  This is due to when Emacs
  decides to recompute the conditional controlling the visibility of the
  overlay.  I do not know how to work around it.

  NOTE: Inserting or deleting characters at the end of the buffer usually
  causes the correct update to happen.

## Contributing

I am looking for someone to take over maintenance of this package.

I am not an expert on Emacs Lisp programming, so suggestions about better
ways to write or package the code are welcome.

I wrote this software to solve a particular problem that I had, and it now
solves that problem.  Unfortunately, I am fairly busy and don't have much
time for feature improvements or on-going maintenance.  This has a few
consequences.

- Suggestions, improvements, and fixes are welcome, but I may not get to
  them for quite some time.

- The less work I have to do to incorporate your improvement or fix, the
  more quickly and more likely I will include it.  The best case is a pull
  request that has a good comment explaining what it does and why it
  should be done and that has a simple enough diff that it is obviously
  correct.

- I would be happy to turn over maintenance of this package to someone
  qualified who is in a better position to do on-going maintenance than I.
