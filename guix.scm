;;; Variables: emacs-compost-mode project-homepage project-synopsis
(use-modules
 ;;; These are my commonly needed modules; remove unneeded ones.
 (guix packages)
 ((guix licenses) #:prefix license:)
 (guix download)
 (guix build-system emacs)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (guix gexp))

(package
  (name "emacs-compost-mode")
  (version "0.1.0")
  (source (local-file (string-append "./"))
  (build-system emacs-build-system)
  (native-inputs (list texinfo))                       
  (synopsis "Notetaking Mode for GNU Emacs")
  (description
   (string-append
    "An implementation of a variation on the Zettelkasten method of "
    "notetaking in GNU Emacs, leveraging org-mode, plain-text, and "
    "pdf-tools to create a directory of notes."))
  (home-page
   "https://cdr255.com/projects/compost-mode")
  (license license:agpl3+))

;; Local Variables:
;; mode: scheme
;; End:
