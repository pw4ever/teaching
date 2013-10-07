teaching data structure & algorithm with Common Lisp
===

how to run the code
---

If you already have Common Lisp set up (e.g., I use [SBCL][]), use the package like any [ASDF][]-defined system. [Quicklisp][] is recommended

Otherwise, an easy route to run these examples is to use [my Common Lisp setup helper for SBCL][dev-env cl helper] on a *vanilla* Linux machine with *sbcl*, *git*, *emacs*, *perl*, *curl* installed.

```bash
curl -L https://raw.github.com/pw4ever/dev-env/master/00helpers/setup-common-lisp.sh | sh
```

**NOTE: existing SBCL and Emacs configurations will be overwritten by the script. Read and adapt it before proceeding.**

This script will set up Emacs/[SLIME][] with Quicklisp that can load all project under `$HOME/hacking/common-lisp/` and its subdirectories (using ASDF's source registry facility `$HOME/.config/common-lisp/source-registry.conf.d/`).

Now, run the following Shell code:

```bash
cd $HOME/hacking/common-lisp/
git clone https://github.com/pw4ever/teaching
```

Now you are all set.

To test the code, start emacs and enter `M-x slime` to enter SLIME REPL. 

Tip: SLIME's documentation functions (with key map starting with `C-c C-d`) are extremely helpful in exploring new packages.

[sbcl]: http://www.sbcl.org "Steel Bank Common Lisp"
[asdf]: http://common-lisp.net/project/asdf/ "Another System Definition Facility"
[quicklisp]: http://www.quicklisp.org/beta/ "Quicklisp"
[dev-env cl helper]: https://github.com/pw4ever/dev-env/tree/master/00helpers
[slime]: http://common-lisp.net/project/slime/ "Superior Lisp Interaction Mode for Emacs"
