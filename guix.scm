(use-modules (guix packages)
             ((guix licenses)
              #:prefix license:)
             (guix build-system emacs)
             (gnu packages emacs-xyz)
             (gnu packages texinfo)
             (guix gexp))

(package
  (name "emacs-compost-mode")
  (version "0.1.0")
  (source (local-file (getcwd)
                      #:recursive? #t))
  (build-system emacs-build-system)
  (native-inputs (list texinfo))
  (propagated-inputs (list emacs-deadgrep emacs-pdf-tools))
  (arguments
   (list #:phases #~(modify-phases %standard-phases
                      (add-before 'install 'make-info
                        (lambda* (#:key inputs #:allow-other-keys)
                          (let ((makeinfo (search-input-file inputs
                                                             "/bin/makeinfo")))
                            (system (string-append makeinfo " "
                                     "doc/emacs-compost-mode.texi")))))
                      (add-before 'install 'set-home
                        (lambda _
                          (setenv "HOME" "/tmp"))))))
  (synopsis "Notetaking Mode for GNU Emacs")
  (description
   "An implementation of a variation on the Zettelkasten method of notetaking
in GNU Emacs, leveraging org-mode, plain-text, and pdf-tools to create a
directory of notes.")
  (home-page "https://cdr255.com/projects/compost-mode")
  (license license:agpl3+))

;; Local Variables:
;; mode: scheme
;; End:
