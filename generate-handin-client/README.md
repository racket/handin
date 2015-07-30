# Use steps

1. Install the `handin` collection with `raco`. This tool will work with either
   version of that collection, but it will include code from the installed
   version, so you want to use your fork. For instance, if it is
   ps-tuebingen/handin, use:
   ```sh
   raco pkg install https://github.com/ps-tuebingen/handin.git
   ```
2. Install this script in some folder of your choice. We'll refer to it as `${PATH_TO_HANDIN}`.
3. Go to your `handin-config` directory, where you have the configuration file `config.rktd`.
4. Adjust `config.rktd` as needed, and include the correct `server-cert.pem`.
5. Run this tool with:
   ```sh
   racket -t ${PATH_TO_HANDIN}/generate-handin-client/main.rkt
   ```
6. This will produce a zip file and its checksum, for clients to install. The
   name will be the `client-name` from `config.rktd`.
