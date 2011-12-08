#lang racket/base

(require raco/command-name
         racket/cmdline
         racket/path
         compiler/compiler)

(define verbose #f)
(define (debug str . args)
  (when verbose
    (apply printf str args)))

;; (debug "auto command 1\n")
(define file
  (command-line
    #:program (short-program+command-name)
    #:args (source-or-bytecode-file)
    source-or-bytecode-file))

(define (is-zo? path)
  (string=? "zo" (format "~a" (filename-extension path))))

(define (last-modification-time file)
  (file-or-directory-modify-seconds file))

(define (file-newer? file1 file2)
  (> (last-modification-time file1)
     (last-modification-time file2)))

(define (from-racket/base? path)
  (regexp-match #px"racket/base" (format "~a" path)))

(define (do-load loader)
  (lambda (path something)
    (define (can-load path)
      (with-handlers ([exn? (lambda (fail) #f)])
        (loader path something)
        #t))

    (define (create-zo source zo-path compiled-path)
      (when (not (directory-exists? compiled-path))
        (make-directory compiled-path))
      (debug "compiling to zo ~a\n" path)
      (when (file-exists? zo-path)
        (delete-file zo-path))
      ((compile-zos #f #:module? #t) (list path) compiled-path))

    (if (and #false (from-racket/base? path))
      (loader path something)
      (let ()
        (define-values (parent self _) (split-path path))
        (parameterize ([current-load-relative-directory parent])
          (debug "compile/load ~a ~a\n" path something)
          (debug "load directory ~a\n" (current-load-relative-directory))
          (define load-path 
            (if (is-zo? path)
              path
              (let ()
                (define compiled-directory "compiled")
                (define compiled-path (build-path parent compiled-directory))
                (define zo-path (build-path compiled-path
                                            (path-add-suffix self #".zo")))
                (when (or (not (file-exists? zo-path))
                          (not (can-load zo-path))
                          (file-newer? path zo-path))
                  (create-zo path zo-path compiled-path))
                zo-path)))
          (debug "Loading file ~a\n" load-path)
          (loader load-path something))))))

(debug "got file ~a\n" file)
(let ([namespace (make-base-namespace)])
  (define loader (current-load/use-compiled))
  (parameterize ([current-namespace namespace]
                 [current-load/use-compiled (do-load loader)])
    #;
    (eval `(require ,file))
    (eval-syntax (with-syntax ([file file])
                   #'(require file)))))
