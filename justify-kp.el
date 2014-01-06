;; ver experimental, code has to be cleaned up & documented

(require 'dash)
(require 'dash-functional)

(defvar pj-line-width 1000
  "Goal width of each line.")

(defvar pj-demerits-line 10
  "Value which a linebreak contributes to break-point demerits.")

(defvar pj-hanging-punctuation '("." ",")
  "Punctuation that should extend after the right margin.")

(defun pj-get-gstring (from to font-object string)
  (setq string (string-to-multibyte string))
  (copy-tree (composition-get-gstring from to font-object string) t))

(defmacro pj-mapcar (fun gline)
  "Call FUN on each glyph of GLINE and return the list of
results."
  (declare (debug (function-form form)))
  (let ((re (make-symbol "re-list"))
        (ln (make-symbol "gline-len")))
    `(let ((,re nil)
           (,ln (lgstring-char-len ,gline)))
       (--dotimes ,ln
         (push (funcall ,fun (lgstring-glyph ,gline it)) ,re))
       (nreverse ,re))))

(defmacro pj-mapc (fun gline)
  "Call FUN on each glyph of GLINE for side effect only."
  (declare (debug (function-form form)))
  (let ((ln (make-symbol "gline-len")))
    `(let ((,ln (lgstring-char-len ,gline)))
       (--dotimes ,ln
         (funcall ,fun (lgstring-glyph ,gline it))))))

(defun pj-get-line (&optional p)
  "Transform current line into a \"glyph\" line."
  (setq p (or p (point)))
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (font (font-at p))
         (gline (pj-get-gstring 0 (length line) font line)))
    gline))

(defun pj-get-line-width (gline)
  "Return the width of GLINE in pixels."
  (let ((width 0))
    (pj-mapc
     (lambda (g)
       (unless (eq (lglyph-char g) 32)
         (cl-incf width (lglyph-width g))))
     gline)
    width))

(defun pj-get-line-spaces ()
  "Return number of spaces in GLINE."
  (cl-destructuring-bind (b . e) (bounds-of-thing-at-point 'line)
    (count-matches " " b e)))

(defun pj-get-space-width (gline)
  "Return width of space character using the font of GLINE.

This function assumes there is a space character in GLINE."
  (let ((header (lgstring-header gline))
        (i 1))
    (while (not (eq (aref header i) 32)) (cl-incf i))
    (lglyph-width (lgstring-glyph gline (1- i)))))

;; TODO: right now the constant '5' is randomly chosen and doesn't
;; corespond to font-width.  Find a generic way to get glyph widths
;; (the gline approach is quite shitty)
(defun pj--make-box (word total-width shrink stretch &optional glue)
  "Make a box data structure out of WORD, which is a list of glyphs."
  (let* ((width 0)
         (chars (mapcar (lambda (c)
                          (cl-incf width (lglyph-width c))
                          (lglyph-char c))
                        word))
         (value (apply 'string chars)))
    (when (and pj-hanging-punctuation
               glue
               (-any? (-cut s-ends-with-p <> value) pj-hanging-punctuation))
      (cl-decf width 5)
      (setq glue (plist-put glue :width
                            (+ (plist-get glue :width) 5))))
    (list :type :box
          :width width
          :value value
          :total-width (+ width total-width)
          :total-shrink shrink
          :total-stretch stretch
          :glue glue)))

;; TODO: handle the underflow boxes better (right now huge stretch
;; fixes most of the issues)
(defun pj--make-glue (width)
  "Make a glue token with default WIDTH, shrinkability 1/3 of
width and stretchability 1/2 of width."
  (list :type :glue :width width :shrink (/ width 3) :stretch (/ (* width 6) 3) :value " "))

;; this assumes one glue always follows one box -- I think any other
;; situation can be reduced to this anyway
(defun pj-get-line-tokens (gline)
  "Return a list of tokens for the current line.

Each token is either \"box\" or \"glue\"---that is inter-word
spacing.  Each token has a width and possible shrink and stretch
values.  Boxes also has a value, the text they represent."
  (let ((total-width 0)
        (shrink 0)
        (stretch 0)
        (space-width (pj-get-space-width (copy-sequence gline)))
        re
        word)
    (pj-mapc
     (lambda (g)
       (if (not (eq (lglyph-char g) 32))
           (push g word)
         (let* ((glue (pj--make-glue space-width))
                (box (pj--make-box (nreverse word) total-width shrink stretch glue)))
           (setq glue (plist-get box :glue))
           (push box re)
           (push glue re)
           (setq word nil)
           (cl-incf total-width (+ (plist-get box :width) (plist-get glue :width)))
           (cl-incf shrink (plist-get glue :shrink))
           (cl-incf stretch (plist-get glue :stretch)))))
     gline)
    (push (pj--make-box (nreverse word) total-width shrink stretch) re)
    (nreverse re)))

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
           (mapc
            (lambda (ind)
              (setq active-nodes (remove-if (lambda (_) t) active-nodes :start ind :end (1+ ind))))
            rem-ind))
         ;; TODO: fix `-remove-at-indices' (-flatten breaks things)
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
             (setcdr cn (cons an (cons :distance
                                       (cons line-width
                                             (cons :demerits
                                                   (cons dem (cons (car cn) (cdr cn))))))))
             (setcar cn :parent)))))
     ;; we can only consider boxes at this point
     (--filter (eq :box (plist-get it :type)) tokens))
    (let* ((x (-min-by (-on '> (lambda (x) (plist-get x :demerits))) active-nodes))
           (re (list x)))
      (while (setq x (plist-get x :parent))
        (push x re))
      (cdr re))
    ))

(defun pj-breaklines (tokens)
  (save-excursion
    (let* ((where (pj-justify tokens))
           (where2 where))
      (mapc
       (lambda (ac)
         (forward-char (+ (length (plist-get ac :value))
                          (length (pj--plist-get ac :glue :value))))
         (when where
           (if (= (plist-get (car where) :total-width)
                  (plist-get ac :total-width))
               (progn
                 (delete-char (- (length (pj--plist-get ac :glue :value))))
                 (newline)
                 (pop where)))))
       (--filter (eq :box (plist-get it :type)) tokens))
      where2)))

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
    (pj-unfill-paragraph)
    (let ((end (progn (forward-paragraph) (point)))
          (start (progn (backward-paragraph) (point))))
      (forward-line)
      (save-excursion (replace-regexp " +" " " nil start end))
      (pj-breaklines-and-justify (pj-get-line-tokens (pj-get-line))))))


(defun pj-unjustify-paragraph ()
  (interactive)
  (save-excursion
    (let ((end (progn (forward-paragraph) (point)))
          (start (progn (backward-paragraph) (point))))
      (pj-remove-tp start end))))
