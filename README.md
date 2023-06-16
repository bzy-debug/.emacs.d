# My emacs config

bzy's emacs config. Manage packages using `git submodule`.

# How to use

1. Fetch source code and submodules

```bash
git clone --recurse-submodules --shallow-submodules https://github.com/bzy-debug/.emacs.d.git
```

2. Install package dependencies

  - [lsp-bridge](https://github.com/manateelazycat/lsp-bridge#installation)

    ```bash
    pip3 install epc orjson sexpdata six paramiko
    ```

  - [org-mode](https://orgmode.org/org.html#Installation)

    ```bash
    cd lisp/extensions/org
    make autoloads
    ```

  - [auctex](https://www.gnu.org/software/auctex/manual/auctex.html#Installation)

    ```bash
    cd lisp/extensions/auctex
    ./autogen.sh
    ./configure --with-texmf-dir=/usr/local/texlive/texmf-local/ --with-emacs=/Applications/Emacs.app/Contents/MacOS/bin/emacs --with-lispdir=/Applications/Emacs.app/Contents/Resources/site-lisp
    make
    sudo make install
    ```
