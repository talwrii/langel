;;; Convenience functions help learn langauges -*- lexical-binding: t -*-

;;; Copyright Tal Wrii
;;; All rights reserved

(defvar lang-grep-history nil "History of words that we grepped for")
(defvar lang-grep-target "german" "Target language")

(defvar lang-corpus-dir "/home/tom/mine/not-code/german/corpus" "Directory containing a corpus of text")

(defvar lang-thing nil "Thing that we want to insert at point")


(defvar lang-grep-word-list-file "/usr/share/dict/ngerman")
(defvar lang-grammar-file "/home/tom/mine/not-code/notes/language/german-grammer-1.txt")
(defvar lang-phrase-file "/home/tom/mine/not-code/german/phrase.txt")

(defvar lang-irregular-verb-file "/home/tom/mine/not-code/german/irregular.csv")

(defvar lang-personal-vocab "/home/tom/mine/not-code/notes/language/german-personal-vocab")

(defvar lang-grep-translate-from-overrides "/home/tom/mine/not-code/notes/language/german-dictionary-overrides")

(defvar lang-regular-conjugations-file "/home/tom/mine/not-code/notes/language/german-rules")

(defvar lang-regular-pronunciation-file "/home/tom/mine/not-code/notes/language/german-pronuncation")

(defvar lang-ipa-command "espeak -q -v %S --ipa %S")
(defvar lang-say-command "espeak -v %S --ipa %S")
(defvar lang-phrase-command "trans -s %S -t %S %s  -brief")
(defvar lang--lookup-func 'lang--dict-translate "function used for translation.")
(setq lang--lookup-func 'lang--leo-translate)
(defvar lang--translation-buffer "*Lang translation*" )
(defvar lang--words-buffer "*Lang words*")

(defvar lang-iso-target "de" "Isocode of the target language")
(defvar lang-iso-source "en" "isocode of the source language")

(defvar lang-dict-target-source "german-english")
(defvar lang-dict-source-target "english-german")
(defvar lang-mode--grep-expression "")

(define-derived-mode lang-words-mode fundamental-mode "lang-words" "Mode for a list of words"
  (setq mode-line-format (list "%e" mode-line-front-space  mode-line-buffer-identification " " lang-mode--grep-expression )))

(defun lang-leo-conj (word)
  "Look up conjugation information with leo"
  (interactive (list (thing-at-point 'word)))
  (message
  (shell-command-to-string
   (format "deconj %s"
           (shell-quote-argument word)))))


(define-key lang-words-mode-map "g" nil)
(define-key lang-words-mode-map "gg" 'lang-grep-edit)
(define-key lang-words-mode-map "gh" 'lang-grep-start)
(define-key lang-words-mode-map "gl" 'lang-grep-end)
(define-key lang-words-mode-map "t" 'lang-translate-to-target-prompt)


(define-derived-mode lang-reading-mode fundamental-mode
  "lang-reading"
  "Mode for reading in a foreign language")

(defun lang-at-point (func)
  (lambda ()
    (interactive)
    (funcall func  (thing-at-point 'word))))

(defun lang-pronounce (word)
  (interactive (list (read-string "Word:" (thing-at-point 'symbol))))
  (message (lang-pronounce-raw word)))

(defun lang-pronounce-raw (word)
  (shell-command-to-string
   (format lang-ipa-command
           lang-iso-target
           word)))

(defun lang-say-raw (word)
  (shell-command-to-string
   (format lang-say-command
           lang-iso-target
           word)))

(defun lang-say-tap ()
  "Pronounce the word at point."
  (interactive)
  (message (lang-say-raw (thing-at-point 'word))))

(defun lang-pronounce-tap ()
  "Pronounce the word at point."
  (interactive)
  (lang-pronounce (thing-at-point 'word)))

(defun lang-pronounce-tap-clip ()
  "Pronounce the word at point."
  (interactive)
  (kill-new (lang-pronounce-raw (thing-at-point 'word))))

(define-derived-mode lang-translation-mode fundamental-mode "lang-words" "Mode for a list of words")

(defvar lang-keymap (make-sparse-keymap))

(defun lang--write-buffer (buffer-name string)
  "Write into BUFFER-NAME, STRING."
  (let (buffer)
    (setq buffer (get-buffer-create buffer-name))
    (with-current-buffer buffer
      (erase-buffer)
      (insert string)
      (goto-char (point-min)))
    buffer))

(defun lang--selected-region ()
  (when mark-active
    (buffer-substring (region-beginning) (region-end))))

(defun lang-grep-tap ()
  (interactive)
  (lang-grep (thing-at-point 'word)))


(defun lang-grep (regexp)
  "Search for words matching a regular expression."
  (interactive (list (read-string "Regular Expression:" (lang--selected-region))))
  (let (match-string word-buffer)
    (setq lang-grep-history (cons regexp lang-grep-history))
    (setq match-string (shell-command-to-string (format "grep -E -i %s %s" (shell-quote-argument regexp) (shell-quote-argument lang-grep-word-list-file))))
    (setq word-buffer (lang--write-buffer lang--words-buffer match-string))
    (setq lang-mode--grep-expression regexp )
    (with-current-buffer word-buffer
      (lang-words-mode))
      (pop-to-buffer word-buffer)))

(defun lang-grep-at-point ()
  (interactive)
  (lang-grep (read-string "Grep for thing:" (thing-at-point 'word))))

(defun lang-grep-edit ()
  "Edit the grep expression and research"
  (interactive)
  (lang-grep
   (read-string "Grep for thing" lang-mode--grep-expression)))



(defun lang-grep-start-region (regexp)
  (interactive (list (read-string "Regular Expression:" (s-concat "^" (lang--selected-region)))))
  (lang-grep regexp))

(defun lang-grep-end-region (regexp)
  (interactive (list (read-string "Regular Expression:" (s-concat (lang--selected-region) "$"))))
  (lang-grep regexp))


(setq lang-lookup-map
      (let ((map (make-sparse-keymap)))
        (define-key map "r" 'lang-open-conjugations)
        (define-key map "p" 'lang-open-pronunciations)
        (define-key map "g" 'lang-grammar-lookup)
        (define-key map "i" 'lang-irregular-lookup)
        (define-key map "v" 'lang-my-vocab)
        (define-key map "P" 'lang-phrase-lookup)

        (define-key map "I" (lang-at-point 'lang-irregular-lookup))
        map))

(setq lang-notes-map
      (let ((map (make-sparse-keymap)))
        (define-key map "v" 'lang-my-vocab)
        (define-key map "v" 'lang-phrase-lookup)
        map))



(evil-define-key 'normal lang-reading-mode-map
  (kbd "<SPC>") 'lang-translate-from-target
  (kbd "t") 'lang-translate-to-target
  (kbd "f") 'lang-trans-translate-from-target
  (kbd "T") 'lang-phrase-translate-to-target
  (kbd "F") 'lang-phrase-translate-to-source
  "l" nil
  "n" nil
  (kbd "nh") 'lang-grep-start
  "np" nil
  (kbd "n.p") 'lang-pronounce-tap
  "gs" 'lang-say-tap
  (kbd "ng") 'lang-grep-tap
  (kbd "nP") 'lang-pronounce-tap-clip
  (kbd "ni") 'lang-insert-thing
  (kbd "nl") 'lang-grep-end
  "n l" 'lang-translate-from-end
  "n.f" 'lang-trans-translate-from-target
  "np " 'lang-translate-from-target-prompt
  "npt" 'lang-translate-to-target-prompt
  "nrc" 'lang-leo-conj
  "nkn" 'lang-noun-to-leo
  "nka" 'lang-adj-to-leo
  "C" 'lang-search-corpus
  "nb" 'lang-bury-line
  "nc" 'lang-conjugate
  "r" lang-lookup-map
  "nn" lang-notes-map)



(defun lang-bury-line ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'line))
         (content (buffer-substring (car bounds) (cdr bounds))))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (goto-char (point-max))
      (insert "\n")
      (insert content))))



(defun lang-conjugate ()
  (interactive)
  (shell-command (concat "deconj " (shell-quote-argument (thing-at-point 'word)))))



(evil-define-key 'visual lang-reading-mode-map
  (kbd "<SPC>") 'lang-translate-from-target
  (kbd "gh") 'lang-grep-start
  (kbd "p") 'lang-pronounce-tap
  (kbd "gg") 'lang-grep
  (kbd "F") 'lang-phrase-translate-to-source
  (kbd "T") 'lang-phrase-translate-to-target
  (kbd "gl") 'lang-grep-end)





(evil-define-key 'visual lang-reading-mode-map
  (kbd "<SPC>") 'lang-translate-from-target)

(evil-define-key 'normal lang-words-mode-map
  (kbd "<SPC>") 'lang-translate-from-target)




(evil-make-overriding-map lang-reading-mode-map)
(evil-make-overriding-map lang-words-mode-map)



(defun lang-repeat-grep ()
  (interactive)
  (let (word)
    (setq word (completing-read "Regular expression:" lang-grep-history))
    (lang-grep word)))

(defun lang-grep-pop ()
  (interactive)
  (pop lang-grep-history)
  (lang-grep (pop lang-grep-history)))

(defun lang-grep-translate-from-override (word)
  "Lookup a from override (personal dictionary)"
  (let ((mappings (lang-grep-load-translate-from-overrides)))
    (cdr (assoc word mappings))))

(defun lang-grep-load-translate-from-overrides ()
  (mapcar (lambda (x) (apply 'cons (s-split-up-to " " x 1)))
          (mapcar (lambda (x) (s-replace "+" "\n" x))
                  (-filter (lambda (x) (not (equal (length x) 0)))
                          (s-split "\n" (f-read lang-grep-translate-from-overrides))))))


(defun lang-grep-translate-save-override (word definition)
  "Save a dictionary override."
  (f-append (s-concat word
                      (s-replace "\n" "+" definition))
            'utf-8
            lang-grep-translate-from-overrides))

(defun lang-translate-from-target ()
  (interactive)
  (let ((word-at-point (or (lang--selected-region) (thing-at-point 'word))))
    (or
     (lang-grep-translate-from-override word-at-point)
     (funcall lang--lookup-func lang-dict-target-source
                           word-at-point))))

(defun lang-translate-from-target-string (word)
  (funcall lang--lookup-func lang-dict-target-source
                           word))


(defun lang-translate-to-target ()
  (interactive)
  (funcall lang--lookup-func
           lang-dict-source-target
           (or (lang--selected-region) (thing-at-point 'word))))

(defun lang-translate-to-target-prompt (word)
  (interactive "sWord:")
  (funcall lang--lookup-func lang-dict-source-target word))




(defun lang-translate-from-target-prompt ()
  (interactive)
  (funcall lang--lookup-func lang-dict-target-source
                        (read-string "Word:")))

(defun lang-translate-from-target-prompt ()
  (interactive)
  (funcall lang--lookup-func lang-dict-target-source
                        (read-string "Word:")))


(defun lang-reading-layout ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer lang--words-buffer)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer lang--translation-buffer)
  (other-window 1))


(defun lang--leo-translate (lang word)
  (lang--display-translation
   (shell-command-to-string
    (format "leo %s" (shell-quote-argument word)))))

(defun lang--dict-translate (dictionary word)
  (let (translation)
    (setq translation (shell-command-to-string
           (format "dict -d %s %s"
                   (shell-quote-argument dictionary)
                   (shell-quote-argument word))))
    (lang--display-translation translation)))

(defun lang--trans-dict (source target word)
  (lang--display-translation
   (shell-command-to-string
    (format "trans -s %s -t %s %s" source target (shell-quote-argument word)))))

(defun lang-trans-translate-from-target ()
  (interactive)
  (lang--trans-dict
   lang-iso-target lang-iso-source
   (thing-at-point 'word)))

(defun lang--display-translation (translation)
  "Display a translation window"
  (let ((current-window (selected-window)))
    (pop-to-buffer (lang--write-buffer lang--translation-buffer translation))
    (select-window current-window)))


(defun lang-grammar-lookup ()
  "Search in the formal grammar."
  (interactive)
  (find-file-other-window lang-grammar-file)
  (helm-swoop :$query ""))

(defun lang-phrase-lookup ()
  "Search for phrases in the phrase book."
  (interactive)
  (find-file-other-window lang-phrase-file)
  (helm-swoop :$query ""))

(defun lang-irregular-lookup (&optional query)
  "Search for phrases in the phrase book."
  (interactive)
  (setq query (or query ""))
  (find-file-other-window lang-irregular-verb-file)
  (helm-swoop :$query query))

(defun lang-record-thing (x)
  (setq lang-thing x))

(defun lang-insert-thing ()
  (interactive)
  (insert lang-thing))


(defun lang-phrase-translate-to-source ()
  "Do a complete translation of a region."
  (interactive)
  (message "%S" (lang--phrase-translate
                 lang-iso-target
                 lang-iso-source
                 (or
                  (lang--selected-region)
                  (thing-at-point 'sentence)))))

(defun lang-phrase-translate-to-target ()
  "Do a complete translation of a region."
  (interactive)
  (message "%S" (lang--phrase-translate
                 lang-iso-source
                 lang-iso-target
                 (or
                  (lang--selected-region)
                  (thing-at-point 'sentence)))))

(defun lang--phrase-translate (source target string)
  (lang-record-thing
  (shell-command-to-string
   (format lang-phrase-command
           source
           target
           (shell-quote-argument
            string)))))

(defun lang-search-corpus ()
  (interactive)
  (let ((helm-grep-ag-command "ag --follow --ignore '*.pdf' --line-numbers -S --hidden --color --nogroup %s %s %s "))
    (delete-other-windows)
    (helm-grep-ag lang-corpus-dir nil)))



;;; Phrase book, versus dictionary, versus grammar
;; A dictionary deals only with words and their meaning
;; A phrase-book tends to function as a grammar cheat sheet, though may include some words. Often these words come with usage information
;; A formal grammar will tend to be more correct

;; The line between a phrase-book and a grammar can be
;;  a little blurry, a phrase-book tends to point you
;;   in the correct direction and if your uses are simple
;;   it is good enough


(define-key lang-keymap "g" nil)
(define-key lang-keymap "gg" 'lang-grep)
(define-key lang-keymap "g." 'lang-grep-at-point)
(define-key lang-keymap "gh" 'lang-grep-start)



(define-key lang-keymap "gl" 'lang-grep-end)

(define-key lang-keymap "gG" 'lang-repeat-grep)
(define-key lang-keymap "go" 'lang-grep-pop)

(define-key lang-keymap "p" 'lang-pronounce)

(define-key lang-keymap "f" 'lang-translate-from-target)
(define-key lang-keymap "F" 'lang-translate-from-target-prompt)
(define-key lang-keymap "t" 'lang-translate-to-target)
(define-key lang-keymap "T" 'lang-translate-to-target-prompt)

(define-key lang-keymap "lG" 'lang-grammar-lookup)
(define-key lang-keymap "lP" 'lang-phrase-lookup)
(define-key lang-keymap "lI" 'lang-irregular-lookup)



(defun lang-my-vocab ()
  "Open the file containing your own vocabulary."
  (interactive)
  (with-current-buffer (find-file lang-personal-vocab)
    (lang-reading-mode)))

(define-key evil-normal-state-map "gt" lang-keymap)


(define-key lang-words-mode-map (kbd "<SPC>") 'lang-translate-from-target)
(define-key lang-words-mode-map (kbd "<RET>") 'lang-grep)

(defun lang-leo-make-def (class thing)
  (let ((definition (completing-read "Definition" (cddr (s-split "\n" (shell-command-to-string (concat "PYTHONIOENCODING=utf8 leo -p " class " " (shell-quote-argument thing))))))))
    (-let (
          ((source target) (s-split "  +" definition)))
      (s-concat source " -> "
                (lang-leo-compress-plural
                 target) "\n"))))

(defun lang-line-to-leo (class)
  (let* (
         (line-bounds (bounds-of-thing-at-point 'line))
         (thing (thing-at-point 'line)))
    (delete-region (car line-bounds) (cdr line-bounds))
    (insert (lang-leo-make-def class thing))))


(defun lang-leo-compress-plural (def)
  (string-match "\\([^ ]+\\) \\(.*\\) (die \\(.*\\))" def)
  (let* (
         (article (match-string 1 def))
         (single (match-string 2 def))
         (plural (match-string 3 def))
         (differences (lang-string-diff single plural))
         (added-umlaut
          (and (equal (length differences) 1)
               (equal
                (lang-add-umlaut (cadar differences) )

                (caddar differences))))
         (crazy
          (and (not added-umlaut) (not (equal (length differences) 0))))
         (affix (if (not crazy) (substring plural
                                           (length single))))
         )
    (cond
     (crazy def)
     (t
      (s-concat article
                " "
                single
                "-"
                (if added-umlaut
                    "\""
                  "")
                affix)))))


(defun lang-add-umlaut (vowel)
  (cdr
  (assoc
   vowel
   '(
    ("a" . "ä")
    ("u" . "ü")
    ("o" . "ö")))))

(ert-deftest lang-add-umlaut-test ()
  (should (equal (lang-add-umlaut "u")
 "ü")))



(defun lang-string-diff (a b)
  (mapcar
   (lambda (x) (list (car x)
                     (char-to-string (cadr x))
                     (char-to-string (caddr x))))
     (lang-list-diff (string-to-list a) (string-to-list b))))

(ert-deftest lang-string-diff-test ()
  (should (equal (lang-string-diff "123" "143") '((1 "2" "4"))
)))





(defun lang-list-diff (a b &optional n)
  (setq n (or n 0 ))
  (if (or (null a) (null b))
      nil
    (append
     (if (equal (car a) (car b))
         nil
       (list (list n (car a) (car b))))
    (lang-list-diff (cdr a) (cdr b) (+ 1 n)))))



(ert-deftest lang-leo-compress-test ()
  (should (equal (lang-leo-compress-plural "die X (die Xen)" ) "die X-en"))
  (should (equal (lang-leo-compress-plural "der XXX (die XXXe)" ) "der XXX-e"))
  (should (equal (lang-leo-compress-plural "die XuX (die XüXe)" ) "die XuX-\"e")))







(defun lang-noun-to-leo ()
  (interactive)
  (lang-line-to-leo "n"))

(defun lang-adj-to-leo ()
  (interactive)
  (lang-line-to-leo "adj"))


(defun lang-grep-start ()
  (interactive)
  (let* (
        (word-start (car (bounds-of-thing-at-point 'word)))
        (region-end (and mark-active (region-end)))
        (region-beginning (and mark-active (region-beginning)))
        (start (or region-beginning word-start))
        (end (or region-end (point))))

    (lang-grep (s-concat "^"
                         (buffer-substring start (point))))))

(defun lang-grep-end ()
  (interactive)
  (lang-grep (s-concat (lang--get-end) "$")))

(defun lang--get-end ()
  (let* (
         (word-end (cdr (bounds-of-thing-at-point 'word)))
         (region-end (and mark-active (region-end)))
         (region-beginning (and mark-active (region-beginning)))
         (start (or region-beginning (point)))
         (end (or region-end word-end))
         )
    (buffer-substring start end)))

(defun lang-translate-from-end ()
  (interactive)
  (lang-translate-from-target-string (lang--get-end)))


(define-key lang-words-mode-map (kbd "o") 'lang-grep-pop)

(define-key lang-words-mode-map (kbd "p") 'lang-pronounce-tap)
(define-key lang-words-mode-map (kbd "T") 'lang-translate-to-target-prompt)

(defun lang-open-conjugations (&optional query)
  (interactive)
  (find-file-other-window lang-regular-conjugations-file)
  (helm-swoop :$query query))

(defun lang-open-pronunciations ()
  (interactive)
  (find-file lang-regular-pronunciation-file))



(provide 'lang)
