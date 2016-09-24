;;; Commentary:
;;
;; Development environment for GNU Guix.
;;
;; To setup the development environment, run the following:
;;
;;    patch -p1 < guix-wiredtiger.diff
;;
;; To build dependencies use:
;;
;;    guix build -f guix.scm
;;
;; To install the dependencies, run:
;;
;;    guix install -f guix.scm
;;
;;; Code:

(use-modules (gnu packages autotools)
             (gnu packages databases)
             (guix packages)
             (guix git-download)
             (guix utils))

(define wiredtiger-next
  (package (inherit wiredtiger)
   (name "wiredtiger-next")
   (version "20160924.d85e189")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/wiredtiger/wiredtiger.git")
                  (commit "d85e189")))
            (sha256
             (base32
              "02ivw4lv4zg4nil7858iwbzhr4dhv7h1imhwvjfqm2wm7imx2qsr"))))
   (arguments
     (substitute-keyword-arguments (package-arguments wiredtiger)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'autogen
                      (lambda _
                        (zero? (system* "sh" "autogen.sh"))))))))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)
      ,@(package-native-inputs wiredtiger)))))


wiredtiger-next
