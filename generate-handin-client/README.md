# Use steps

0.  Choose which variant of the `handin` package to use. You can use the
    official variant, https://github.com/plt/handin; but in this document, I'll
    assume you want to use our variant: https://github.com/ps-tuebingen/handin,
    which contains various changes and this tool.

1.  Checkout the handin package in some directory of your choice:

    ```sh
    git clone https://github.com/ps-tuebingen/handin.git
    PATH_TO_HANDIN=$PWD/handin
    ```

    After this, `$PATH_TO_HANDIN` will be the path to the created repo.

2.  From the same directory, install the handin package:
    ```sh
    raco pkg install ./handin
    ```

3.  Create your `handin-config` directory --- for instance by taking ours:

    ```sh
    git clone https://github.com/ps-tuebingen/handin-config.git
    ```

    and setup `config.rktd` according to docs on [server setup][1]. Also include
    the correct `server-cert.pem`. You will also need `private-key.pem` for the
    server, but it's not needed for the client.

4.  Go to your `handin-config` directory, where you have the configuration file `config.rktd`:

    ```sh
    cd handin-config
    ```

5.  Don't adjust `${PATH_TO_HANDIN}/handin-client/info.rkt`. It will be
    regenerated with the information from `config.rktd`.
6.  Run this tool with:
    ```sh
    racket -t ${PATH_TO_HANDIN}/generate-handin-client/main.rkt
    ```
7.  This will produce a zip file and its checksum, for clients to install. The
    name will be the `client-name` from `config.rktd`.

[1]: http://pkg-build.racket-lang.org/doc/handin-server/server-setup.html
