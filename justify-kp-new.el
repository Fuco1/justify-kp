;; WIP version
;; GString routines
(defun pj--buffer-subgstring (from to)
  "Return a gstring representing buffer text between FROM and TO.

This function assumes that the font between FROM and TO does not
change."
  (let ((font (font-at from))
        (str (string-to-multibyte (buffer-substring-no-properties from to))))
    (copy-tree (composition-get-gstring 0 (length str) font str) t)))

;; TODO: this is unused, remove?
(defun pj-map-gstring (fun gstring)
  "Call FUN on each glyph of GSTRING and return the list of
results."
  (let ((re nil)
        (ln (lgstring-char-len gstring)))
    (--dotimes ln
      (push (funcall fun (lgstring-glyph gstring it)) re))
    (nreverse re)))

;; TODO: make this more efficient and only traverse the list once
(defun -map-partition (n fun list)
  (-map fun (-partition n list)))

;; punctuation should always attach to preceding word, unless it's an opening quote: "adas

(defun pj-line-at-point ()
  "Like (thing-at-point 'line) but with initial whitespace trimmed."
  (s-trim (thing-at-point 'line)))

(defvar pj-punctuation-class '(?, ?. ?? ?! ?\" ?\'))
(defvar pj-splitpoint-class '(?- ?—))
(defvar pj-whitespace-class '(? )) ;; we allow splits on whitespace automatically
(defvar pj-tokenizer-states '(word white split))

(defun pj--get-string-tokens ()
  "Split the line in tokens for analysis."
  (flet ((push-char () (push char token))
         (push-tok-char () (push (reverse token) tokens) (setq token (list char))))
    (let ((line (string-to-list (pj-line-at-point)))
          (tokens nil)
          (token nil)
          (state 'word))
      (-each line
        (lambda (char)
          (cond
           ((eq state 'word)
            (cond
             ((memq char pj-whitespace-class)
              (push-tok-char)
              (setq state 'white))
             ((memq char pj-splitpoint-class)
              (push-tok-char)
              (setq state 'split))
             (t (push-char))))
           ((eq state 'white)
            (cond
             ((memq char pj-whitespace-class) (push-char))
             ((memq char pj-splitpoint-class)
              (push-tok-char)
              (setq state 'split))
             (t
              (push-tok-char)
              (setq state 'word))))
           ((eq state 'split)
            (push-tok-char)
            (cond
             ((memq char pj-whitespace-class) (setq setq 'white))
             ((memq char pj-splitpoint-class) (setq state 'split))
             (t (setq state 'word)))))))
      (push (reverse token) tokens)
      (list :length (length line)
            :tokens (--map (apply 'string it) (nreverse tokens))))))

(defun pj--get-tokens (string-tokens)
  "Assumes the point is at the first character of the first
string token in the buffer where these were produced."
  (-let* (((&plist :length length :tokens tokens) string-tokens)
          ;; TODO: assumes the entire line is the same font.  We
          ;; should have a more elaborate method to return the correct
          ;; glyph sizes if there are multiple fonts active
          ([_ _ &rest gstr] (pj--buffer-subgstring (point) (+ (point) length)))
          (gstr (append gstr nil))
          (total-width 0)
          (total-shrink 0)
          (total-stretch 0))
    (list :length length
          :tokens (-map
                   (lambda (token)
                     (-let* ((len (length token))
                             ((cur rest) (-split-at len gstr))
                             (widths (-map 'lglyph-width cur))
                             (is-whitespace (memq (elt token 0) pj-whitespace-class))
                             (width (if is-whitespace (car widths) (-sum widths)))
                             ;; TODO: add shrink tolerance setting here
                             (shrink (if is-whitespace (ceiling (* width 0.33)) 0))
                             ;; TODO: add stretch tolerance setting here
                             (stretch (if is-whitespace (ceiling (* width 1.0)) 0)))
                       (prog1 (list :type (cond
                                           (is-whitespace 'white)
                                           ((memq (elt token 0) pj-splitpoint-class) 'split)
                                           (t 'box))
                                    :value token
                                    :width width
                                    :total-width (setq total-width (+ total-width width))
                                    :shrink shrink
                                    :total-shrink (setq total-shrink (+ total-shrink shrink))
                                    :stretch stretch
                                    :total-stretch (setq total-stretch (+ total-stretch stretch))
                                    :widths widths)
                         (forward-char len)
                         (setq gstr rest))))
                   tokens))))

(defun pj--get-line ()
  "Get tokens for calculating break points of current paragraph."
  (save-excursion
    (pj--get-tokens (pj--get-string-tokens))))

(defun pj--get-token-diff-width (tokena tokenb)
  (- (plist-get tokenb :total-width) (plist-get tokena :total-width)))

(defun pj--get-token-diff-shrink (tokena tokenb)
  (- (plist-get tokenb :total-shrink) (plist-get tokena :total-shrink)))

(defun pj--get-token-diff-stretch (tokena tokenb)
  (- (plist-get tokenb :total-stretch) (plist-get tokena :total-stretch)))

;; Breakpoints are always either whitespace tokens or split tokens.
;; To calculate lenght of the "current" line wrt breakpoint, all we
;; need to do is to get the difference of precomputed total-* values

;; This function should be pure.
(defun pj--justify (tokens)
  (-let* (((&plist :length length :tokens tokens) tokens)
          (active-nodes nil))
    (while tokens
      (-let* (((cur next) tokens)
              ((&plist :type cur-type) cur)
              ((&plist :type next-type) next))
        (cond
         ;; Possible breakpoint.  The whitespace should disappear, its
         ;; width is not counted towards this line's width / shrink /
         ;; stretch.
         ((and (eq cur-type 'white)
               (eq next-type 'box)))
         ;; Possible breakpoint.  The split point's width is counted
         ;; towards this line's total width.
         ((and (eq cur-type 'split)
               (eq next-type 'box)))
         (t (!cdr tokens))))
      ))
  )
