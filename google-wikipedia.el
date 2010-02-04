;;; google.el --- Google access functions... Plus wikipedia

;; Copyright (C) 2009  Filipe Cabecinhas

(defvar gsearch-url
  "http://www.google.com/search?q=%s"
  "URL that searches Google.")
(defvar gsearch-prompt
  "String to search: ")

(defvar glucky-search-url
  "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=%s"
  "URL that performs an ``I'm feeling lucky'' search in Google.")
(defvar glucky-prompt
  "String to do a \"lucky\" search: ")

(defvar gcodesearch-url
  "http://www.google.com/codesearch?hl=en&lr=&q=%s"
  "URL that performs a Google Code search.")
(defvar gcodesearch-prompt
  "String to search in Google Code: ")

(defvar gtranslate-url
  "http://translate.google.com/translate_t?prev=hp&hl=en&js=n&text=%s&sl=%s&tl=%s"
  "URL that performs a Google Translate.")
(defvar gtranslate-prompt
  "String to translate: ")

(defvar gmaps-url
  "http://maps.google.com/maps?q=%s"
  "URL that performs a Google Maps search.")
(defvar gmaps-prompt
  "Location to search: ")

(defvar wikipedia-url
  "http://%s.wikipedia.com/wiki/Special:Search?search=%s&go=Go"
  "URL that searches wikipedia and goes directly to the result page.")
(defvar wikipedia-prompt
  "Term to search wikipedia: ")


(defun google ()
  (interactive)
  (process-file "open" nil nil nil
                (format gsearch-url
                        (g-url-encode
                         (get-region-or-ask-string
                          gsearch-prompt)))))

(defun glucky ()
  (interactive)
  (process-file "open" nil nil nil
                (format glucky-search-url
                        (g-url-encode
                         (get-region-or-ask-string
                          glucky-prompt)))))

(defun gcode ()
  (interactive)
  (process-file "open" nil nil nil
                (format gcodesearch-url
                        (g-url-encode
                         (get-region-or-ask-string
                          gcodesearch-prompt)))))

(defun gtranslate ()
  (interactive)
  (process-file "open" nil nil nil
                (format gtranslate-url
                        (g-url-encode
                         (get-region-or-ask-string
                          gtranslate-prompt))
                        "auto"
                        "en")))

(defun gmaps ()
  (interactive)
  (process-file "open" nil nil nil
                (format gmaps-url
                        (g-url-encode
                         (get-region-or-ask-string
                          gmaps-prompt)))))


(defun wikipedia ()
  (interactive)
  (process-file "open" nil nil nil
                (format wikipedia-url
                        "en"
                        (g-url-encode
                         (get-region-or-ask-string
                          wikipedia-prompt)))))



(defsubst g-url-encode (str)
  "URL encode  string."
  (mapconcat '(lambda (c)
                (cond ((= c 32) "+")
                      ((or (and (>= c ?a) (<= c ?z))
                           (and (>= c ?A) (<= c ?Z))
                           (and (>= c ?0) (<= c ?9)))
                       (char-to-string c))
                      (t (upcase (format "%%%02x" c)))))
             str
             ""))


(provide 'google-wikipedia)

