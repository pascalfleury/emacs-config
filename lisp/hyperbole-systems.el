;; These utilities help detecting things for internal systems.
;;
;; Examples:
;;     go/somewhere-else
;;     b/123456789
;;
;; Integrate it this way into your Emacs:
;;
;;    (use-package hyperbole
;;     :init
;;     (load-file (expanf-file-name "hyperbole-systems.el" my-lisp-directory)))
;;
(require 'thingatpt)
(require 'hyperbole)

(defun systems-url-bounds-of-thing-at-point ()
  "Find constructs that are identifiers for regular URLs"
  ;; http://www.pafsoft.ch https://pafsoft.ch http://cl/12345
  (save-excursion
    (skip-chars-backward "[:alnum:]_/.:")
    (if (looking-at "https?://[[:alnum:]_\\.-]+")
        (cons (point) (match-end 0))
      nil)))

(put 'systems-url 'bounds-of-thing-at-point
     'systems-url-bounds-of-thing-at-point)

(defun systems-link-bounds-of-thing-at-point ()
  "Find constructs that are identifiers for internal systems,
   e.g. b/12345 cr/12345678 guts/12345"
  ;; b/12345 cr/12345678 guts/12345 mdb/fleury
  (save-excursion
    (skip-chars-backward "[:alnum:]_/.-")
    (if (looking-at "[[:alpha:]_]+/[[:alnum:]_\\.-]+")
        (cons (point) (match-end 0))
      nil)))

(put 'systems-link 'bounds-of-thing-at-point
     'systems-link-bounds-of-thing-at-point)

(defun short-link-bounds-of-thing-at-point ()
  "Find constructs that are identifiers for short links,
   g/to-a.group or go/a-link or goto/a-link"
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z_/?#$=,;.&-")
    (if (looking-at "go?t?o?/[a-zA-Z0-9_\\.-]+\\([?/#][^ \t\n]*\\)?")
        (cons (point) (match-end 0))
      nil)))

(put 'short-link 'bounds-of-thing-at-point
     'short-link-bounds-of-thing-at-point)

(defun systems-ldap-bounds-of-thing-at-point ()
  "Find constructs that are people usernames like john@ or jack.y@"
  (save-excursion
    (skip-chars-backward "a-zA-Z\\.@")
    (if (looking-at "[a-zA-Z_\\.]+@")
        (cons (point) (1- (match-end 0)))
      nil)))

(put 'systems-ldap 'bounds-of-thing-at-point
     'systems-ldap-bounds-of-thing-at-point)

(defun mid-bounds-of-thing-at-point ()
  "Find constructs that are mids, like /g/126534 or /m/01yrx."
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z_/")
    (if (looking-at "/[mgtxpn]/[a-z0-9_]+")
        (cons (point) (match-end 0))
      nil)))

(put 'mid 'bounds-of-thing-at-point
     'mid-bounds-of-thing-at-point)

;; //nlp/generation/tippex/BUILD
(defun srcfs-path-of-thing-at-point ()
  "Find constructs that are paths to SrcFS."
  (save-excursion
    (skip-chars-backward "[:alnum:]_/\\.;-")
    (if (looking-at "//[[:alnum:]_/\\.-]+\\(;l=[0-9,-]+\\)?")
        (cons (+ (point) 2) (match-end 0)) ; remove leading slashes
      nil)))

(put 'srcfs 'bounds-of-thing-at-point
     'srcfs-path-of-thing-at-point)

;; /cns/ok-d/home/fleury/
(defun cns-path-of-thing-at-point ()
  "Find constructs that are paths to CNS."
  (save-excursion
    (skip-chars-backward "[:alnum:]_/%=\\.-")
    (if (looking-at "/cns/[[:alnum:]_/%=\\.-]+")
        (cons (point) (match-end 0))
      nil)))

(put 'cns 'bounds-of-thing-at-point
     'cns-path-of-thing-at-point)

;; mpm:ephemeral/gbash/diskless:1-b2d818a0_0cd22096_98ea5734_8e70527a_efb08073
(defun mpm-path-of-thing-at-point ()
  "Find constructs that are paths to CNS."
  (save-excursion
    (skip-chars-backward "[:alnum:]_/%=\\.:-")
    (if (looking-at "mpm:\\([[:alnum:]_/%=\\.:-]+\\)")
        (cons (match-beginning 1) (match-end 1))
      nil)))

(put 'mpm 'bounds-of-thing-at-point
     'mpm-path-of-thing-at-point)

;; https://cl-status.corp.google.com/#/summary/Boq%20genx-nlg-server/494699940
;; cl-status:Boq%20genx-nlg-server/494699940
(defun cl-status-of-thing-at-point ()
  "Find cl-status of project at given CL."
  (save-excursion
    (skip-chars-backward "[:alnum:]_/%=:.-")
    (if (looking-at "cl\\-status:[[:alnum:]_%\\.-]+/[0-9]+")
        ;(cons (match-beginning 1) (match-end 1))
        (cons (+ (point) 10) (match-end 0))
      nil)))

(put 'cl-status 'bounds-of-thing-at-point
     'cl-status-of-thing-at-point)

;; cds|C|6467954|BE|fr|9c34ff36699d6d37:fr_BE
(defun gl-path-of-thing-at-point ()
  "Find constructs that are CDS IDs."
  (save-excursion
    (skip-chars-backward "[:alnum:]|:_")
    (if (looking-at "\\(cds|[[:alnum:]|:_]+\\)")
        (cons (match-beginning 1) (match-end 1))
      nil)))

(put 'gl 'bounds-of-thing-at-point
     'gl-path-of-thing-at-point)

(defib systems-stuff ()
  "Hyperbole implicit button for systems style things."
  (save-excursion
    (let ((url (thing-at-point 'url))
          (mid (thing-at-point 'mid))
          (link (thing-at-point 'systems-link))
          (short (thing-at-point 'short-link))
          (ldap (thing-at-point 'systems-ldap))
          (mid (thing-at-point 'mid))
          (srcfs (thing-at-point 'srcfs))
          (cns (thing-at-point 'cns))
          (mpm (thing-at-point 'mpm))
          (gl (thing-at-point 'gl))
          (cl-status (thing-at-point 'cl-status))
          topic)
      (cond
       ;; this order should be the most specific first.
       (url (progn (ibut:label-set url)
                   (hact 'www-url url)))
       (srcfs (progn (ibut:label-set srcfs)
                     (hact 'www-url (concat "http://cs///depot/google3/" srcfs))))
       (cns (progn (ibut:label-set cns)
                   (hact 'www-url (concat "http://data.corp.google.com/cnsviewer/file?query=" cns))))
       (mpm (progn (ibut:label-set mpm)
                   (hact 'www-url (concat "http://mpmbrowse/package/" mpm "#1"))))
       (cl-status (progn (ibut:label-set cl-status)
                         (hact 'www-url (concat "https://cl-status.corp.google.com/#/summary/" cl-status))))
       (mid (progn (ibut:label-set mid)
                   (hact 'www-url (concat "http://lexistore" mid))))
       (short (progn (ibut:label-set short)
                     (hact 'www-url (concat "http://" short))))
       (link (progn (ibut:label-set link)
                    (hact 'www-url (concat "http://" link))))
       (ldap (progn (ibut:label-set ldap)
                    (hact 'www-url (concat "http://who/" ldap))))
       (mid (progn (ibut:label-set mid)
                   (hact 'www-url (concat "http://hume" mid))))
       (gl (progn (ibut:label-set gl)
                   (hact 'www-url (concat "http://go/gl-cdsid/" gl))))
       ))))
