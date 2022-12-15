;; These utilities help detecting things for internal systems.
;; Examples:
;;     go/somewhere-else
;;     b/123456789

(require 'thingatpt)
(require 'hyperbole)

(defun systems-link-bounds-of-thing-at-point ()
  "Find constructs that are identifiers for internal systems, e.g. b/12345 cr/12345678 guts/12345"
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z_/")
    (if (looking-at "[a-zA-Z_]+/[0-9a-zA-Z_-]+")
        (cons (point) (match-end 0))
      nil))) ; not a systems thing

(put 'systems-link 'bounds-of-thing-at-point
     'systems-link-bounds-of-thing-at-point)

(defun short-link-bounds-of-thing-at-point ()
  "Find constructs that are identifiers for short links, go/... or goto/..."
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z_/?#$=,;.&-")
    (if (looking-at "got?o?/[[:alnum:]_-]+\([?/#][^ \t\n]*\)?")
        (cons (point) (match-end 0))
      nil)))

(put 'short-link 'bounds-of-thing-at-point
     'short-link-bounds-of-thing-at-point)

(defun systems-ldap-bounds-of-thing-at-point ()
  "Find constructs that are people usernames like john@ or jack@"
  (save-excursion
    (skip-chars-backward "a-zA-Z\.@")
    (if (looking-at "[a-zA-Z_\.]+@")
        (cons (point) (1- (match-end 0)))
      nil))) ; not a systems username

(put 'systems-ldap 'bounds-of-thing-at-point
     'systems-ldap-bounds-of-thing-at-point)

(defun mid-bounds-of-thing-at-point ()
  "Find constructs that are mids, like /g/126534 or /m/01yrx."
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z_/")
    (if (looking-at "/[mgtxpn]/[a-z0-9_]+")
        (cons (point) (match-end 0))
      nil))) ; not a mid

(put 'mid 'bounds-of-thing-at-point
     'mid-bounds-of-thing-at-point)

(defun srcfs-path-of-thing-at-point ()
  "Find constructs that are paths to SrcFS."
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z_/.-")
    (if (looking-at "//[[:alnum:]_/.-]+")
        (cons (+ (point) 2) (match-end 0)) ; remove one leading slashes
      nil))) ; not a SrcFS path

(put 'srcfs 'bounds-of-thing-at-point
     'srcfs-path-of-thing-at-point)

(defun cns-path-of-thing-at-point ()
  "Find constructs that are paths to CNS."
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z_/%=.-")
    (if (looking-at "/cns/[[:alnum:]_/%=\.-]+")
        (cons (point) (match-end 0))
      nil))) ; not a CNS path

(put 'cns 'bounds-of-thing-at-point
     'cns-path-of-thing-at-point)

(defib systems-stuff ()
  "Hyperbole implicit button for systems style things."
  (save-excursion
    (let ((mid (thing-at-point 'mid))
          (link (thing-at-point 'systems-link))
          (short (thing-at-point 'short-link))
          (ldap (thing-at-point 'systems-ldap))
          (mid (thing-at-point 'mid))
          (srcfs (thing-at-point 'srcfs))
          (cns (thing-at-point 'cns))
          topic)
      (cond
       (cns (progn (ibut:label-set cns)
                   (hact 'www-url (concat "http://data.corp.google.com/cnsviewer/file?query=" cns))))
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
       (srcfs (progn (ibut:label-set srcfs)
                     (hact 'www-url (concat "http://cs///depot/google3/" srcfs))))
       ))))
