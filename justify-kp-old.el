;; ver experimental, code has to be cleaned up & documented

;; This file is here only for historical significance.  Nothing from
;; here works anymore.

(require 'dash)
(require 'dash-functional)

;; TODO: - and — should always be treated as word delimiter (and also
;; other punctuation)
;; TODO: — should be hanging punctuation

(defgroup justify-kp ()
  "Text/paragraph justification using Knuth/Plass algorithm."
  :group 'convenience
  :prefix "pj-")

(defcustom pj-line-width 1000
  "Goal width of each line."
  :group 'justify-kp
  :type 'integer)
(make-variable-buffer-local 'pj-line-width)

(defcustom pj-demerits-line 10
  "Value which a linebreak contributes to break-point demerits."
  :group 'justify-kp
  :type 'integer)

(defcustom pj-hanging-punctuation '(("." 0.5) ("," 0.5) ("—" 0.5) ("-" 0.5))
  "Punctuation that should extend after the right margin."
  :group 'justify-kp
  :type '(repeat (list string float)))

(defun pj--get-gstring (from to &optional font-object string)
  "Return a gstring representation of buffer text between FROM
and TO.

If optional argument FONT-OBJECT is non-nil, use this font.
Defaults to (font-at FROM).

If optional argument STRING is non-nil, use this as the target
instead of current buffer."
  (setq font-object (or font-object (font-at from)))
  (if string
      (copy-tree (composition-get-gstring from to font-object (string-to-multibyte string)) t)
    (setq string (string-to-multibyte
                  (buffer-substring-no-properties from to)))
    (copy-tree (composition-get-gstring 0 (length string) font-object string) t)))

(defun pj-mapcar (fun gline)
  "Call FUN on each glyph of GLINE and return the list of
results."
  (let ((re nil)
        (ln (lgstring-char-len gline)))
    (--dotimes ln
      (push (funcall fun (lgstring-glyph gline it)) re))
    (nreverse re)))

(defun pj-mapc (fun gline)
  "Call FUN on each glyph of GLINE for side effect only."
  (let ((ln (lgstring-char-len gline)))
    (--dotimes ln
      (funcall fun (lgstring-glyph gline it)))))

(defun pj-reduce (fun init gline)
  "Reduce GLINE using FUN and initial value INIT.

FUN is binary function, with first argument the accumulated value
and second argument an element of the list.

The reduction is left-associative."
  (let ((acc init)
        (ln (lgstring-char-len gline)))
    (--dotimes ln (setq acc (funcall fun acc (lgstring-glyph gline it))))
    acc))

(font-lock-add-keywords 'emacs-lisp-mode `((,(concat "("
                                                     (regexp-opt '("pj-mapcar"
                                                                   "pj-mapc"
                                                                   "pj-reduce") t)
                                                     "\\>")
                                            (1 font-lock-keyword-face))))

(defun pj-get-line (&optional p)
  "Transform current line into a \"glyph\" line."
  (setq p (or p (point)))
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (font (font-at p))
         (gline (pj--get-gstring 0 (length line) font line)))
    gline))

(defun pj-get-line-width (gline)
  "Return the width of GLINE in pixels, ignoring whitespace."
  (pj-reduce (lambda (acc it)
               (if (eq (lglyph-char it) 32) acc (+ acc (lglyph-width it))))
             0 gline))

(defun pj-get-line-spaces ()
  "Return number of spaces in GLINE."
  (cl-destructuring-bind (b . e) (bounds-of-thing-at-point 'line)
    (count-matches " " b e)))

(defun pj--get-buffer-substring-width (from to)
  "Get width of text in current buffer between FROM and TO.

It is assumed the font does not change in this interval."
  (pj--get-string-width (buffer-substring-no-properties from to) from))

(defun pj--get-string-width (string &optional at)
  "Get width of STRING as if it were inserted at current point in current buffer.

If AT is non-nil and number, assume font at that buffer position."
  (let ((gstring (pj--get-gstring 0 (length string) (font-at (or at (point))) string)))
    (apply '+ (pj-mapcar 'lglyph-width gstring))))

;; TODO: make this faster by somehow detecting the 'display space
;; specs
(defun pj--next-font-change (&optional p limit)
  "Move point to next position where font changes.

If optional argument P is a number, start from that position,
defaults to current position.

If optional argument LIMIT is a number, do not extend the search
further than this position.  If no font change was found, the
point will be at the LIMIT position or end of file (if this is
smaller) after the function returns."
  (setq p (or p (point)))
  (setq limit (min (or limit (point-max)) (point-max)))
  (goto-char p)
  (flet ((get-next-font-name
          ()
          (let ((np (or (next-property-change p) limit)))
            (if (>= np limit)
                (setq p limit)
              (elt (font-info (font-at (setq p (goto-char np)))) 0)))))
    (let ((current-font (elt (font-info (font-at p)) 0)))
      (while (equal current-font (get-next-font-name)))
      (goto-char p))))

(defun pj--get-line-data ()
  (let ((limit (1- (cdr (bounds-of-thing-at-point 'line))))
        (last-change (point))
        next-change
        gline
        re)
    (while (< (point) limit)
      (setq next-change (pj--next-font-change (point) limit))
      (setq gline (pj--get-gstring last-change next-change))
      (setq last-change next-change)
      (pj-mapc
       (lambda (g)
         (push (list :char (lglyph-char g) :width (lglyph-width g)) re))
       gline))
    (nreverse re)))

(defun pj-get-space-width (gline)
  "Return width of space character using the font of GLINE.

This function assumes there is a space character in GLINE."
  (let ((header (lgstring-header gline))
        (i 1))
    (while (not (eq (aref header i) 32)) (cl-incf i))
    (lglyph-width (lgstring-glyph gline (1- i)))))

;; warning: word and whitespace come REVERSED!!!
(defun pj--make-token (word whitespace total-width total-shrink total-stretch)
  "Make a box data structure out of WORD, which is a list of glyphs."
  (let* ((word-width (--reduce-from (+ acc (pj-glyph-get it :width)) 0 word))
         (white-width (--reduce-from (+ acc (pj-glyph-get it :width)) 0 whitespace))
         (glue-width white-width)
         (word-value (apply 'string (nreverse (--map (pj-glyph-get it :char) word))))
         (white-value (apply 'string (--map (pj-glyph-get it :char) whitespace))))
    (when (and pj-hanging-punctuation
               (-any? (-cut s-ends-with-p <> word-value) (--map (car it) pj-hanging-punctuation)))
      (-when-let* ((punct (substring word-value (1- (length word-value))))
                   (punct-data (assoc punct pj-hanging-punctuation))
                   (extension (floor (* (cadr punct-data) (pj-glyph-get (car word) :width)))))
        (cl-incf glue-width extension)
        (cl-decf word-width extension)))
    (list :type :box
          :width word-width
          :value word-value
          :total-width (+ word-width total-width)
          :total-shrink total-shrink
          :total-stretch total-stretch
          :glue (when whitespace
                  (list :type :glue
                        :width glue-width
                        :real-width white-width
                        :shrink (/ white-width 3)
                        :stretch (* white-width 2)
                        :value white-value)))))


(defmacro pj-glyph-get (glyph-data prop)
  `(plist-get ,glyph-data ,prop))

;; this assumes one glue always follows one box -- I think any other
;; situation can be reduced to this anyway.
;;   - when we have foo-bar-baz we need to take this as multiple boxes
;;     with glue of zero width

;; TODO: above function needs to be reworked to take changes in
;; text-properties into account.  Input should be a point in buffer:
;; we take the line following the point, then walk over the words by
;; face (font!!)  property changes, take the glines of each segment,
;; and produce the tokens.  At most one glue token can follow each
;; non-glue token---this glue will be added as a property of the box,
;; not as a separate item in the token stream.  Each glue will have to
;; remember its apparent and real width (apparent width can extend
;; over surrounding boxes, real width is what we stretch/shring when
;; we actually render the glue)
(defun pj--get-line-tokens ()
  "Return a list of tokens on the current line.

Each token has a width, a value (the text it represents) and an
optional glue following it.  The glue has a width, a (usually
whitespace) value, and a possible stretchability and
shirnkability---when setting the line, glue represents the
stretching or shrinking of the space between this token and the
next.

We must also distinguish an apparent width of the glue and the
real width of the glue.  Apparent width can extend the glue into
the surrounding tokens and is used in the break-point
calculation.  Real width is the width of the whitespace this glue
represent and is used when the line is actually rendered.  This
distinction is necessary due to the rendering engine of Emacs."
  (save-excursion
    (flet ((add-token
            (token)
            (push token re)
            (setq word nil)
            (setq whitespace nil)
            (cl-incf total-width (+ (plist-get token :width) (or (pj--plist-get token :glue :width) 0)))
            (cl-incf total-shrink (or (pj--plist-get token :glue :shrink) 0))
            (cl-incf total-stretch (or (pj--plist-get token :glue :stretch) 0))))
      (let ((line-data (pj--get-line-data))
            (total-width 0) (total-shrink 0) (total-stretch 0)
            re
            word whitespace)
        (--each line-data
          (cond
           ;; read the word
           ((and (not (eq (pj-glyph-get it :char) 32))
                 (not whitespace))
            (push it word))
           ;; read the whitespace
           ((and (eq (pj-glyph-get it :char) 32)
                 word)
            (push it whitespace))
           ;; when we have both, we can process them into a token
           (t
            (let ((token (pj--make-token word whitespace total-width total-shrink total-stretch)))
              (add-token token))
            (push it word))))
        (add-token (pj--make-token word whitespace total-width total-shrink total-stretch))
        (nreverse re)))))

(defun pj--break-badness (break-point)
  "Calculate badness of BREAK-POINT.

BREAK-POINT is supposed to be a value compatible with return
value of `pj--get-node-difference'."
  (let* ((adjustment (- pj-line-width (plist-get break-point :distance)))
         (adj-ratio (cond
                     ((<= adjustment 0)
                      (/ (float adjustment) (plist-get break-point :shrink-diff)))
                     ((> adjustment 0)
                      (/ (float adjustment) (plist-get break-point :stretch-diff)))))
         (badness (+ (* (expt (abs adj-ratio) 3) 100) 0.5)))
    badness))

;; todo: introduce various penalties
(defun pj--break-demerits (badness)
  "Calculate demerits of line break."
  (expt (+ pj-demerits-line badness) 2))

;; TODO: this assumes each node has a glue
(defun pj--get-node-difference (node1 node2)
  "Get differences between NODE1 and NODE2."
  (list :distance (- (plist-get node2 :total-width)
                     (plist-get node1 :total-width)
                     (pj--plist-get node1 :glue :width))
        :shrink-diff  (- (plist-get node2 :total-shrink)
                         (plist-get node1 :total-shrink)
                         (pj--plist-get node1 :glue :shrink))
        :stretch-diff (- (plist-get node2 :total-stretch)
                         (plist-get node1 :total-stretch)
                         (pj--plist-get node1 :glue :stretch))))

(defun pj--too-distant-p (current-node active-node)
  "Return non-nil if CURRENT-NODE is too distant from ACTIVE-NODE to permit breaking.

This signifies that the active node should be disactivated."
  (let* ((diff (pj--get-node-difference active-node current-node))
         (dist (plist-get diff :distance))
         (shr (plist-get diff :shrink-diff)))
    (< pj-line-width (- dist shr))))

(defun pj--too-close-p (current-node active-node)
  "Return non-nil if CURRENT-NODE is too distant from ACTIVE-NODE to permit breaking.

This signifies that the active node should be disactivated."
  (let* ((diff (pj--get-node-difference active-node current-node))
         (dist (plist-get diff :distance))
         (str (plist-get diff :stretch-diff)))
    (< (+ dist str) pj-line-width)))

(defun pj--possible-break-point-p (current-node active-node)
  "Return non-nil if CURRENT-NODE is possible break point w.r.t. ACTIVE-NODE."
  (let* ((diff (pj--get-node-difference active-node current-node))
         (dist (plist-get diff :distance))
         (shr (plist-get diff :shrink-diff))
         (str (plist-get diff :stretch-diff)))
    (and (<= (- dist shr) pj-line-width)
         (<= pj-line-width (+ dist str)))))

(defun pj-justify (tokens)
  "Find the best possible justification of TOKENS."
  (let ((active-nodes (list (list :type :box :value "" :width 0 :total-width 0 :total-shrink 0 :total-stretch 0 :demerits 0
                                  :glue (list :width 0 :shrink 0 :stretch 0)))))
    (mapc
     ;; for each box node, look back to all active nodes and find the best
     ;; parent.
     (lambda (cn)
       (let ((break-points nil)
             (rem-ind nil)
             (cur-ind 0))
         ;; find possible break points for current node
         (mapc
          (lambda (an)
            (cond
             ((pj--too-distant-p cn an)
              (push cur-ind rem-ind))
             ((pj--too-close-p cn an))
             (t
              (when (pj--possible-break-point-p cn an)
                (push (-concat (list :node an) (pj--get-node-difference an cn)) break-points))))
            (cl-incf cur-ind))
          active-nodes)
         ;; pick the best one and save it as parent
         ;; beware the car/cdr hacking! It modifies the tokens
         ;; inplace, but I can live with that for now
         (when rem-ind
           (setq active-nodes (-remove-at-indices rem-ind active-nodes)))
         (when break-points
           (setq active-nodes (-concat active-nodes (list cn)))
           (let* ((best (-min-by (-on '> (lambda (x) (+ (plist-get x :demerits)
                                                        )))
                                 (--map (-concat (list :demerits
                                                       (+ (plist-get (plist-get it :node) :demerits)
                                                          (pj--break-demerits
                                                           (pj--break-badness it))))
                                                 it)
                                        break-points)))
                  (an (plist-get best :node))
                  (dem (plist-get best :demerits))
                  (line-width (plist-get best :distance)))
             (setcdr cn (-cons* an
                                :distance line-width
                                :demerits dem
                                :shrink-diff (plist-get best :shrink-diff)
                                :stretch-diff (plist-get best :stretch-diff)
                                (car cn) (cdr cn)))
             (setcar cn :parent)))))
     tokens)
    (let* ((x (-min-by (-on '> (lambda (x) (plist-get x :demerits))) active-nodes))
           (re (list x)))
      (while (setq x (plist-get x :parent))
        (push x re))
      (cdr re))))

(defun pj-breaklines (tokens)
  (save-excursion
    (flet ((recalc-adj-ratio
            ()
            (setq overflow 0)
            (setq line-width (plist-get (car where) :distance))
            (setq adjustment (- pj-line-width line-width))
            (setq adj-ratio (cond
                             ((<= adjustment 0)
                              (/ (float adjustment) (plist-get (car where) :shrink-diff)))
                             ((> adjustment 0)
                              (/ (float adjustment) (plist-get (car where) :stretch-diff)))))))
      (let* ((where (pj-justify tokens))
             line-width adjustment adj-ratio
             (overflow 0))
        (when where
          (recalc-adj-ratio)
          (mapc
           (lambda (ac)
             ;; skip over the box
             (forward-char (+ (length (plist-get ac :value))
                              (or (length (pj--plist-get ac :glue :value)) 0)))
             ;; modify the space display
             (when where
               (if (= (plist-get (car where) :total-width)
                      (plist-get ac :total-width))
                   (progn
                     ;; TODO: text could also break between boxes with
                     ;; no glue between them
                     (put-text-property (- (point) (length (pj--plist-get ac :glue :value)))
                                        (point) 'display "\n")
                     (pop where)
                     (when where (recalc-adj-ratio)))
                 (when (plist-get ac :glue)
                   (let* ((real-width (pj--plist-get ac :glue :real-width))
                          (disp-width (+ real-width (* (if (<= adj-ratio 0)
                                                           (pj--plist-get ac :glue :shrink)
                                                         (pj--plist-get ac :glue :stretch)) adj-ratio)))
                          (disp-width-whole (floor disp-width))
                          (disp-width-decimal (- disp-width disp-width-whole))
                          (current-width (if (progn
                                               (cl-incf overflow disp-width-decimal)
                                               (< overflow 1))
                                             disp-width-whole
                                           (cl-decf overflow 1)
                                           (+ 1 disp-width-whole))))
                     (put-text-property (- (point) (length (pj--plist-get ac :glue :value)))
                                    (point)
                                    'display `(space :width ,(list current-width))))))))
           tokens)))))
  t)

(defun pj-breaklines-and-justify (tokens)
  (let ((lines (pj-breaklines tokens)))
    (--each lines
      (pj-justify-line (plist-get it :distance))
      (forward-line))))

(defmacro pj--plist-get (ds key &rest keys)
  (if keys
      `(-> ,ds
         (plist-get ,key)
         ,@(mapcar (lambda (k) (list 'plist-get k)) keys))
    `(plist-get ,ds ,key)))

(defun pj-justify-line (&optional width)
  (interactive)
  (pj--justify-line (pj-get-line) width))

(defun pj--justify-line (gline &optional width)
  (let* ((line-width (or width (pj-get-line-width gline)))
         (line-spaces (pj-get-line-spaces))
         (space-width (pj-get-space-width gline))
         (extra (- pj-line-width line-width))
         (new-space-width (+ space-width (/ (float extra) line-spaces)))
         (new-space-width-whole (floor new-space-width))
         (new-space-width-decimal (- new-space-width new-space-width-whole))
         (space-stretch-ratio (/ new-space-width space-width))
         (overflow 0))
    (save-excursion
      (goto-char (line-beginning-position))
      (while (search-forward " " (line-end-position) t)
        (cl-incf overflow new-space-width-decimal)
        (let ((current-width (if (< overflow 1)
                                 new-space-width-whole
                               (cl-decf overflow 1)
                               (+ 1 new-space-width-whole))))
          (put-text-property (match-beginning 0) (match-end 0) 'display `(space :width ,(list current-width)))))
      (search-backward " " (line-beginning-position) t)
      (put-text-property (match-beginning 0) (match-end 0) 'display `(space :width ,(list (+ overflow new-space-width-whole)))))))

(defun pj-remove-tp (begin end)
  (interactive "r")
  (remove-text-properties begin end '(display)))

;; TODO: this is a quick hack... we'd need something faster.
(defun pj-unfill-paragraph ()
  "Take a multi-line paragrap and make it into a single line of text.
This is the opposite of fill-paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun pj-justify-paragraph ()
  (interactive)
  (save-excursion
    (pj-unjustify-paragraph)
    ;; (pj-unfill-paragraph)
    (let ((end (progn (forward-paragraph) (point)))
          (start (progn (backward-paragraph) (point))))
      (forward-line)
      ;; (save-excursion (replace-regexp " +" " " nil start end))
      (pj-breaklines-and-justify (pj--get-line-tokens)))))


(defun pj-unjustify-paragraph ()
  (interactive)
  (save-excursion
    (let ((end (progn (forward-paragraph) (point)))
          (start (progn (backward-paragraph) (point))))
      (pj-remove-tp start end))))

(defun pj-justify-paragraph-2 ()
  (interactive)
  (save-excursion
    (pj-unjustify-paragraph)
    (let ((beg-of-par (progn
                        (forward-paragraph)
                        (backward-paragraph)
                        (forward-line)
                        (point)))
          ;; (prev-relevant-bp (let ((visual-line-mode t)
          ;;                         (line-move-visual t))
          ;;                     (beginning-of-visual-line)
          ;;                     (previous-line)
          ;;                     (point)))
          )
      (goto-char (max beg-of-par;;  prev-relevant-bp
                      )))
    (pj-breaklines (pj--get-line-tokens))))

(defun pj-justify-paragraph-and-move ()
  (interactive)
  (pj-justify-paragraph-2)
  (forward-paragraph 2)
  (backward-paragraph)
  (forward-line))
