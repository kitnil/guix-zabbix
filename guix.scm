;; guix.scm --- Guix package for Zabbix-Guix
;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules ((guix licenses) #:prefix license:)
             (guix build-system guile)
             (guix build utils)
             (guix packages)
             (guix gexp)
             (guix git-download)
             (ice-9 popen)
             (ice-9 rdelim)
             (gnu packages bash)
             (gnu packages guile)
             (gnu packages package-management)
             (gnu packages version-control))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(let ((commit (git-output "log" "-n" "1" "--pretty=format:%H")))
  (package
    (name "zabbix-guix")
    (version (string-append "0.0.1" "-" (string-take commit 7)))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (inputs
     `(("bash" ,bash)
       ("git" ,git)))
    (native-inputs
     `(("guile" ,guile-2.2)
       ("guile-json" ,guile-json)
       ("guix" ,guix)
       ,@(package-propagated-inputs guix)))
    (arguments
     `(#:modules ((guix build guile-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 popen)
                  (ice-9 rdelim))
                 #:phases
                 (modify-phases %standard-phases
                   (replace 'unpack
                     (lambda* (#:key inputs #:allow-other-keys)
                       (for-each (lambda (file)
                                   (copy-file (string-append (assoc-ref inputs "source") "/" file)
                                              (string-append "./" file)))
                                 '("zabbix_guix" "zabbix.scm"))))
                   (add-after 'unpack 'setenv
                     (lambda _
                       (setenv "PATH"
                               (string-append (assoc-ref %build-inputs "bash") "/bin" ":"
                                              (getenv "PATH")))))
                   (add-after 'install 'install-script
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (bin (string-append out "/bin"))
                              (zabbix-guix (string-append bin "/zabbix_guix"))
                              (guile (assoc-ref inputs "guile"))
                              (guile-bin (string-append guile "/bin/guile"))
                              (git (assoc-ref inputs "guile-git"))
                              (bs (assoc-ref inputs "guile-bytestructures"))
                              (gcrypt (assoc-ref inputs "guile-gcrypt"))
                              (json (assoc-ref inputs "guile-json"))
                              (guix (assoc-ref inputs "guix"))
                              (deps (list out gcrypt json bs git guix))
                              (effective
                               (read-line
                                (open-pipe* OPEN_READ guile-bin
                                            "-c" "(display (effective-version))")))
                              (path   (string-join
                                       (map (cut string-append <>
                                                 "/share/guile/site/"
                                                 effective)
                                            deps)
                                       ":"))
                              (gopath (string-join
                                       (map (cut string-append <>
                                                 "/lib/guile/" effective
                                                 "/site-ccache")
                                            deps)
                                       ":")))
                         (mkdir-p bin)
                         (substitute* "./zabbix_guix"
                           (("\\$\\(which guile\\)") guile-bin))
                         (install-file "./zabbix_guix" bin)
                         (chmod zabbix-guix #o555)
                         (wrap-program zabbix-guix
                           `("GUILE_LOAD_PATH" ":" prefix (,path))
                           `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath))
                           `("PATH" ":" prefix (,(string-append (assoc-ref inputs "git") "/bin"))))
                         #t))))))
    (home-page "https://anongit.duckdns.org/guix/zabbix-guix")
    (description "This package provides a Guile script to monitor Guix
channels difference.")
    (synopsis "Monitor Guix in Zabbix")
    (license license:gpl3+)))
