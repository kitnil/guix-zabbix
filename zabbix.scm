;; zabbix.scm --- Monitor Guix in Zabbix.
;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(define-module (zabbix)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix profiles)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37))

(define (git . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (let* ((port   (apply open-pipe* OPEN_READ "git" args))
         (output (read-string port)))
    (close-port port)
    (string-trim-right output #\newline)))

(define (remote-channels channels)
  (map (lambda (c)
         (match (string-split (git "ls-remote" (channel-url c)
                                   (string-append "*/" (channel-branch c)))
                              #\tab)
           ((commit ref)
            (channel (name (channel-name c))
                     (url (channel-url c))
                     (commit commit)
                     (branch (channel-branch c))
                     (location #f)))))
       (eval (begin (use-modules (guix channels))
                    (with-input-from-file channels read))
             (current-module))))

(define (channels profile)
  (define number
    (generation-number profile))

  (map (lambda (entry)
         (match (assq 'source (manifest-entry-properties entry))
           (('source ('repository ('version 0)
                                  ('url url)
                                  ('branch branch)
                                  ('commit commit)
                                  _ ...))
            (channel (name (string->symbol (manifest-entry-name entry)))
                     (url url)
                     (commit commit)
                     (location #f)))

           ;; Pre-0.15.0 Guix does not provide that information,
           ;; so there's not much we can do in that case.
           (_ (channel (name 'guix)
                       (url "?")
                       (commit "?")))))

       ;; Show most recently installed packages last.
       (reverse
        (manifest-entries
         (profile-manifest
          (if (zero? number)
              profile
              (generation-file-name profile number)))))))

(define %options
  (let ((display-and-exit-proc (lambda (msg)
                                 (lambda (opt name arg loads)
                                   (display msg) (quit)))))
    (list (option '(#\v "version") #f #f
                  (display-and-exit-proc "Guiz version 0.0.1\n"))
          (option '(#\h "help") #f #f
                  (display-and-exit-proc
                   "Usage: foo scheme-file ..."))
          (option '(#\A "available") #f #t
                  (lambda (opt name arg loads)
                    (cons `(query list-available ,(or arg ""))
                          loads)))
          (option '(#\d "diff") #t #f
                  (lambda (opt name arg loads)
                    (cons `(query diff ,(or arg ""))
                          loads)))
          (option '(#\p "profile") #t #f
                  (lambda (opt name arg loads)
                    (alist-cons 'profile (canonicalize-profile arg)
                                loads)))
          (option '(#\r "remote") #t #f
                  (lambda (opt name arg loads)
                    (alist-cons 'remote arg loads))))))

(define %default-options
  '())

(define (main args)
  (define opts
    (args-fold (cdr (program-arguments))
               %options
               (lambda (opt name arg loads)
                 (error "Unrecognized option `~A'" name))
               (lambda (op loads)
                 (cons op loads))
               %default-options))

  (define (find-channel-name name channels)
    (fold (lambda (x xs)
            (if (string=? (symbol->string (channel-name x)) name)
                (cons x xs)
                xs))
          '()
          channels))

  (define remote (or (assq-ref opts 'remote)
                     (and=> (getenv "HOME")
                            (lambda (home)
                              (string-append home "/.config/guix/channels.scm")))))

  (define profile (or (assq-ref opts 'profile) (current-profile)))

  (match (assoc-ref opts 'query)
    (('list-available name)
     (scm->json
      `((data ,@(map (lambda (local remote)
                       `(("{#REMOTE_NAME}" . ,(channel-name remote))
                         ("{#REMOTE_URL}" . ,(channel-url remote))
                         ("{#REMOTE_COMMIT}" . ,(channel-commit remote))
                         ("{#LOCAL_NAME}" . ,(channel-name local))
                         ("{#LOCAL_URL}" . ,(channel-url local))
                         ("{#LOCAL_COMMIT}" . ,(channel-commit local))))
                     (channels profile)
                     (remote-channels remote))))))
    (('diff name)
     (map (lambda (local remote)
            (if (string=? name (symbol->string (channel-name local)))
                (begin (display 0) (exit 1))
                (begin (display 1) (exit 0))))
          (channels profile)
          (remote-channels remote)))))
