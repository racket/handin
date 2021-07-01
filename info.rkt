#lang setup/infotab

(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "drracket"
               "drracket-plugin-lib"
               "gui-lib"
               "htdp-lib"
               "net-lib"
               "pconvert-lib"
               ["sandbox-lib" #:version "1.2"]
               "rackunit-lib"
               "web-server-lib"))
(define build-deps '("gui-doc"
                     "racket-doc"
                     "scribble-lib"))
