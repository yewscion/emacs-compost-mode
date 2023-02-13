

# The `emacs-compost-mode` README

**A Note-taking mode for GNU Emacs.**

*Last Updated: 2023-02-13 (W07) 23:07:05 GMT*


# Installation


## GNU Guix

If You use [GNU Guix](https://guix.gnu.org/), this package 
is on [my channel](https://sr.ht/~yewscion/yewscion-guix-channel/).

Once You have it set up, You can just run:

    guix pull
    guix install emacs-compost-mode

If You just want to try it out, You can use Guix Shell instead:

    guix shell --pure --rebuild-cache -v4 emacs-compost-mode bash emacs

And if You'd rather just try it out without my channel, You can clone this
repo and then do:

    cd emacs-compost-mode
    ./cast.sh

Either of these will create a profile with **just** this project in it, to mess around with.


## Source

If You don't want to use [GNU Guix](https://guix.gnu.org/),
You can clone this repo and install it in the normal way (assuming You have
 an `~/.emacs.d/init.el`, and that `~/.emacs.d/lisp` is in Your load-path):

    git clone https://git.sr.ht/~yewscion/emacs-compost-mode
    cd emacs-compost-mode
    cp -v *.el ~/.emacs.d/lisp/
    cat >> ~/.emacs.d/init.el <<< (require 'compost)

Then, in Emacs, it's just a matter of restarting (or calling 
`M-x package-initialize` again!) and You should be good to go.

You can also just open the `emacs-compost-mode.el` file and run `M-x eval-buffer`,
but that only lasts for the current session.

If You don't want to use git, or would rather stick with an
actual release, then see the tagged releases for some tarballs
of the source.

You can also install using the standard `./configure && make && make check && make install`
sequence, as I use GNU Autotools for all of my projects.

The needed dependencies are tracked in the `DEPENDENCIES.txt` file
to support this use case.


# Usage

Full usage is documented in the `doc/emacs-compost-mode.info` file. Here are
only generic instructions.

You should be able to access all of
its exported functions in guile by using its modules:

    (use-modules (cdr255 core))

Any binaries or scripts will be available in Your `$PATH`. A list of these
is maintained in the info file. They all also have the `--help` flag, so
if You prefer learning that way, that is also available.


# Contributing

Pull Requests are welcome, as are bugfixes and extensions. Please open
issues as needed. If You contribute a feature, needs to be tests and
documentation.

Development is expected to be done using [GNU Guix](https://guix.gnu.org/).
If You have `guix` set up, You should be able to enter a development
environment with the following:

    cd emacs-compost-mode
    guix shell -D -f guix.scm bash --pure

If You've made changes without the above precautions, those changes will
need to be confirmed to work in the above environment before merge.


# License

The `emacs-compost-mode` project and all associated files are ©2022 Christopher
Rodriguez, but licensed to the public at large under the terms of the:

[GNU AGPL3.0+](https://www.gnu.org/licenses/agpl-3.0.html) license.

Please see the `LICENSE` file and the above link for more information.

