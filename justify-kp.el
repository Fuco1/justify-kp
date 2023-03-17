;;; justify-kp.el --- Justify paragraphs using Knuth/Plass algorithm

;; Copyright (C) 2014 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 23th November 2014
;; Package-requires: ((dash "2.18.0") (s "0"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'dash)
(require 's)

(defgroup justify-kp ()
  "Justify paragraphs using Knuth/Plass algorithm."
  :group 'convenience
  :prefix "pj-")

(defcustom pj-demerits-line 10
  "Value which a linebreak contributes to break-point demerits."
  :group 'justify-kp
  :type 'integer)

(defcustom pj-shrink-ratio 0.33
  "Whitespace shrink ratio.

A whitespace token can be shrinked at most this multiple of its
real width."
  :group 'justify-kp
  :type 'float)

(defcustom pj-stretch-ratio 0.8
  "Whitespace stretch ratio.

A whitespace token can be stretched at most this multiple of its
real width."
  :group 'justify-kp
  :type 'float)

(defcustom pj-hanging-punctuation '(("." 0.5) ("," 0.5) ("—" 0.2) ("-" 0.5))
  "Punctuation that should extend after the right margin.

The numeric value specifies multiple of the regular width that
can overlap the margin."
  :group 'justify-kp
  :type '(repeat (list string float)))


;; Window routines
(defun pj--get-window-width ()
  "Get usable window width in pixels."
  (-let* (((left _ right) (window-pixel-edges))
          ((fleft fright) (window-fringes)))
    (- (- right fright) (+ left fleft))))

(defun pj--get-working-window-width ()
  "Get usable window width minus a working buffer in pixels."
  ;; make the 10 customizable
  (- (pj--get-window-width) (* 10 (frame-char-width))))

(defun pj-line-width ()
  "Return preferred line width."
  ;; TODO: make this overridable by a defcustom: use a constant, use working ww
  (pj--get-working-window-width))


;; GString routines
(defun pj--mapc-gstring (fun gstring)
  "Call FUN on each glyph of GSTRING for side effect only."
  (let ((ln (lgstring-char-len gstring)))
    (--dotimes ln (funcall fun (lgstring-glyph gstring it)))))

(defun pj--buffer-subgstring (from to)
  "Return a gstring representing buffer text between FROM and TO.

This function assumes that the font between FROM and TO does not
change."
  (let ((font (font-at from))
        (str (string-to-multibyte (buffer-substring-no-properties from to))))
    (copy-tree (composition-get-gstring 0 (length str) font str) t)))

;; doesn't handle font changes properly
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
  (cl-flet ((get-next-font-name
             ()
             (let ((np (or (next-property-change p) limit)))
               (if (>= np limit)
                   (setq p limit)
                 (elt (font-info (font-at (setq p (goto-char np)))) 0)))))
    (let ((current-font (elt (font-info (font-at p)) 0)))
      (while (equal current-font (get-next-font-name)))
      (goto-char p))))

(defun pj--get-line-data ()
  "Get characters and their widths on current line.

Respects font changes."
  (let ((limit (1- (cdr (bounds-of-thing-at-point 'line))))
        (last-change (point))
        next-change gline re)
    (while (< (point) limit)
      (setq next-change (pj--next-font-change (point) limit))
      (setq gline (pj--buffer-subgstring last-change next-change))
      (setq last-change next-change)
      (pj--mapc-gstring
       (lambda (g)
         (push (list :char (lglyph-char g) :width (lglyph-width g)) re))
       gline))
    (nreverse re)))

(defun pj-line-at-point ()
  "Like (thing-at-point 'line) but with initial whitespace trimmed."
  (s-trim (thing-at-point 'line)))

(defvar pj--punctuation-class '(?, ?. ?? ?! ?\" ?\'))
(defvar pj--splitpoint-class '(?- ?—))
(defvar pj--whitespace-class '(? )) ;; we allow splits on whitespace automatically

(defun pj--get-string-tokens ()
  "Split the current line in string tokens."
  (cl-flet ((push-char () (push char token))
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
             ((memq char pj--whitespace-class)
              (push-tok-char)
              (setq state 'white))
             ((memq char pj--splitpoint-class)
              (push-tok-char)
              (setq state 'split))
             (t (push-char))))
           ((eq state 'white)
            (cond
             ((memq char pj--whitespace-class) (push-char))
             ((memq char pj--splitpoint-class)
              (push-tok-char)
              (setq state 'split))
             (t
              (push-tok-char)
              (setq state 'word))))
           ((eq state 'split)
            (push-tok-char)
            (cond
             ((memq char pj--whitespace-class) (setq setq 'white))
             ((memq char pj--splitpoint-class) (setq state 'split))
             (t (setq state 'word)))))))
      (push (reverse token) tokens)
      (list :length (length line)
            :tokens (--map (apply 'string it) (nreverse tokens))))))

(defun pj--get-tokens ()
  "Construct list of tokens for analysis.

Assumes the point is at the first character of the first string
token in the buffer where these were produced."
  (-let* (((&plist :length length :tokens tokens) (pj--get-string-tokens))
          (line-data (save-excursion (pj--get-line-data)))
          (total-width 0)
          (total-shrink 0)
          (total-stretch 0)
          (index 0))
    (list :length length
          :tokens (-map
                   (lambda (token)
                     (-let* ((len (length token))
                             ((cur rest) (-split-at len line-data))
                             (widths (--map (plist-get it :width) cur))
                             (is-whitespace (memq (elt token 0) pj--whitespace-class))
                             (width (if is-whitespace (car widths) (-sum widths)))
                             (shrink (if is-whitespace (ceiling (* width pj-shrink-ratio)) 0))
                             (stretch (if is-whitespace (ceiling (* width pj-stretch-ratio)) 0)))
                       (prog1 (list :type (cond
                                           (is-whitespace 'white)
                                           ((memq (elt token 0) pj--splitpoint-class) 'split)
                                           (t 'box))
                                    :value token
                                    :index (prog1 index
                                             (setq index (1+ index)))
                                    :width width
                                    :total-width (setq total-width (+ total-width width))
                                    :shrink shrink
                                    :total-shrink (setq total-shrink (+ total-shrink shrink))
                                    :stretch stretch
                                    :total-stretch (setq total-stretch (+ total-stretch stretch))
                                    :widths widths)
                         (setq line-data rest))))
                   tokens))))

(defun pj--get-token-diff-width (tokena tokenb)
  "Return total width difference between TOKENA and TOKENB.

TOKENB should be the more advanced one."
  (- (plist-get tokenb :total-width) (plist-get tokena :total-width)))

(defun pj--get-token-diff-width-with-hp (tokena tokenb)
  "Return total width difference between TOKENA and TOKENB, taking hanging punctuation into account.

TOKENB should be the more advanced one."
  (-let* ((real-diff (pj--get-token-diff-width tokena tokenb))
          ((&plist :value value :widths widths) tokenb)
          (last-char (-last-item (string-to-list value)))
          (last-char-width (-last-item widths)))
    (-when-let (ratio (cadr (assoc (char-to-string last-char) pj-hanging-punctuation)))
      (setq real-diff (- real-diff (* ratio last-char-width))))
    real-diff))

(defun pj--get-token-diff-shrink (tokena tokenb)
  "Return total shrink difference between TOKENA and TOKENB.

TOKENB should be the more advanced one."
  (- (plist-get tokenb :total-shrink) (plist-get tokena :total-shrink)))

(defun pj--get-token-diff-stretch (tokena tokenb)
  "Return total stretch difference between TOKENA and TOKENB.

TOKENB should be the more advanced one."
  (- (plist-get tokenb :total-stretch) (plist-get tokena :total-stretch)))

(defun pj--break-badness (active-node current-node)
  "Calculate badness for a line from ACTIVE-NODE to CURRENT-NODE."
  (let* ((diff-width (pj--get-token-diff-width-with-hp active-node current-node))
         (diff-shrink (pj--get-token-diff-shrink active-node current-node))
         (diff-stretch (pj--get-token-diff-stretch active-node current-node))
         (adjustment (- (pj-line-width) diff-width))
         (adj-ratio (cond
                     ((<= adjustment 0)
                      (/ (float adjustment) diff-shrink))
                     ((> adjustment 0)
                      (/ (float adjustment) diff-stretch)))))
    (+ (* (expt (abs adj-ratio) 3) 100) 0.5)))

(defun pj--break-demerits (active-node current-node)
  "Calculate demerits for a line from ACTIVE-NODE to CURRENT-NODE."
  (let ((badness (pj--break-badness active-node current-node)))
    (expt (+ pj-demerits-line badness) 2)))

(defun pj--too-close-p (active-node current-node)
  "Return non-nil if ACTIVE-NODE and CURRENT-NODE are too close for a breakpoint."
  (< (+ (pj--get-token-diff-width-with-hp active-node current-node)
        (pj--get-token-diff-stretch active-node current-node))
     (pj-line-width)))

(defun pj--too-distant-p (active-node current-node)
  "Return non-nil if ACTIVE-NODE and CURRENT-NODE are too distant for a breakpoint."
  (< (pj-line-width)
     (- (pj--get-token-diff-width-with-hp active-node current-node)
        (pj--get-token-diff-shrink active-node current-node))))

(defun pj--possible-break-point-p (active-node current-node)
  "Return non-nil if a breakpoint for line between ACTIVE-NODE and CURRENT-NODE is possible."
  (and (not (pj--too-close-p active-node current-node))
       (not (pj--too-distant-p active-node current-node))))

;; This function should be kept pure.
(defun pj--justify (tokens)
  "Find all possible justifications of TOKENS."
  (-let* (((&plist :length length :tokens tokens) tokens)
          (active-nodes (list (list :type 'init :value "" :width 0 :total-width 0 :shrink 0
                                    :total-shrink 0 :stretch 0 :total-stretch 0 :demerits 0 :widths nil))))
    (while tokens
      (-let* (((prev cur next) tokens)
              ((&plist :type prev-type) prev)
              ((&plist :type cur-type) cur)
              ((&plist :type next-type :value next-value) next)
              (possible-break-points nil)
              (rem-ind nil))
        (cond
         ;; TODO: check if box isn't punctuation, single letter
         ;; preposition etc...
         ((and (or
                ;; Possible breakpoint.  The whitespace should disappear, its
                ;; width is not counted towards this line's width / shrink /
                ;; stretch.
                (eq cur-type 'white)
                ;; Possible breakpoint.  The split point's width is counted
                ;; towards this line's total width.
                (eq cur-type 'split))
               (eq next-type 'box)
               (not (memq (elt next-value 0) pj--punctuation-class))
               ;; In some languages, single-letter words can not start a line.
               ;; (not (= (length next-value) 1))
               ;; TODO: add more line-breaking conditions here
               )
          (let ((comp (if (eq cur-type 'white) prev cur)))
            (-each-indexed active-nodes
              (lambda (it-index an)
                (when (pj--too-distant-p an comp)
                  (push it-index rem-ind))
                (when (pj--possible-break-point-p an comp)
                  (let ((bp (-concat
                             (list :parent an
                                   :demerits (+ (plist-get an :demerits)
                                                (pj--break-demerits an comp)))
                             cur)))
                    (push bp possible-break-points))))))
          (let ((new-active-nodes (if rem-ind
                                      (-remove-at-indices rem-ind active-nodes)
                                    active-nodes)))
            (if possible-break-points
                (let ((best (-min-by (-on '> (lambda (x) (plist-get x :demerits))) possible-break-points)))
                  (setq active-nodes (-concat new-active-nodes (list best))))
              (if new-active-nodes
                  (setq active-nodes new-active-nodes)
                ;; If we have an overly long line, we'd still rather
                ;; break it here than error out.  So if no active nodes
                ;; are left, we pick the best of the old ones and start
                ;; from zero, breaking at current position.
                (let ((best-active-node (pj--get-best-active-node active-nodes)))
                  (setq active-nodes (list (-concat
                                            (list :parent best-active-node
                                                  :demerits 0)
                                            cur)))))))
          (!cdr tokens)
          (!cdr tokens))
         (t (!cdr tokens)))))
    active-nodes))

(defun pj--get-best-active-node (active-nodes)
  "Get the best justification from ACTIVE-NODES.

ACTIVE-NODES should be compatible with output of `pj--justify'."
  (-min-by (-on '> (lambda (x) (plist-get x :demerits))) active-nodes))

(defun pj-justify ()
  "Justify current line using Knuth/Plass algorithm.

This is a low-level interactive function.  It must be called at
the beginning of *unfilled* line.

For more high-level interactive use, see: `pj-justify-paragraph',
`pj-unjustify-paragraph', `pj-justify-paragraph-and-move'."
  (interactive)
  (save-excursion
    (let* ((line (pj--get-tokens))
           (active-nodes (pj--justify line))
           (line (plist-get line :tokens))
           (raw-break-points (pj--get-best-active-node active-nodes))
           (break-points (let ((re (list raw-break-points)))
                           (while (setq raw-break-points
                                        (plist-get raw-break-points :parent))
                             (push raw-break-points re))
                           (cdr re)))
           (lbp (plist-get (car break-points) :parent)))
      (-each break-points
        (lambda (bp)
          (-let* (((cur-line rest) (--split-with (/= (plist-get it :index) (plist-get bp :index)) line))
                  (last-token (if (eq (plist-get bp :type) 'split) bp (-last-item cur-line)))
                  ;; Add hanging punctuation support.  We shorten the
                  ;; apparent width of the line but leave the
                  ;; stretch/shrink as it is, that means the
                  ;; punctuation will get pushed out of the margin
                  ((&plist :value lt-value :widths lt-widths) last-token)
                  (last-char (-last-item (string-to-list lt-value)))
                  (last-char-width (-last-item lt-widths))
                  (last-token (-if-let (ratio (cadr (assoc (char-to-string last-char) pj-hanging-punctuation)))
                                  (plist-put (-copy last-token) :total-width
                                             (- (plist-get last-token :total-width)
                                                (* ratio last-char-width)))
                                last-token))
                  (width (pj--get-token-diff-width lbp last-token))
                  (stretch (pj--get-token-diff-stretch lbp last-token))
                  (shrink (pj--get-token-diff-shrink lbp last-token))
                  (adjustment (- (pj-line-width) width))
                  (adj-ratio (cond
                              ((<= adjustment 0)
                               (max -1 (/ (float adjustment) shrink)))
                              ((> adjustment 0)
                               (/ (float adjustment) stretch))))
                  (overflow 0.0))
            (-each cur-line
              (lambda (lt)
                (let ((len (length (plist-get lt :value))))
                  (forward-char len)
                  (when (eq (plist-get lt :type) 'white)
                    (let* ((width (plist-get lt :width))
                           (disp-width (+ width (* (if (<= adj-ratio 0)
                                                       (plist-get lt :shrink)
                                                     (plist-get lt :stretch))
                                                   adj-ratio)))
                           (disp-width-whole (floor disp-width))
                           (disp-width-decimal (- disp-width disp-width-whole))
                           (current-width (if (progn
                                                (setq overflow (+ overflow disp-width-decimal))
                                                (< overflow 1))
                                              disp-width-whole
                                            (setq overflow (1- overflow))
                                            (1+ disp-width-whole))))
                      (put-text-property (- (point) len) (point)
                                         'display `(space :width (,current-width))))))))
            (setq lbp (car rest))
            (let ((type (plist-get lbp :type))
                  (len (length (plist-get lbp :value))))
              (forward-char len)
              (cond
               ((eq type 'white)
                (put-text-property (- (point) len)
                                   (point) 'display "\n"))
               ((eq type 'split)
                (put-text-property (- (point) len)
                                   (point) 'display (concat (plist-get lbp :value) "\n")))))
            (!cdr rest)
            (setq line rest)))))))

(defun pj-remove-display-property (begin end)
  "Remove display text property in a region between BEGIN and END."
  (interactive "r")
  (remove-text-properties begin end '(display)))

(defun pj-unfill-paragraph ()
  "Take a multi-line paragrap and make it into a single line of text.
This is the opposite of fill-paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun pj-unjustify-paragraph ()
  "Unjustify current paragraph.

Remove display properties and unfill."
  (interactive)
  (save-excursion
    (let ((end (progn (forward-paragraph) (point)))
          (start (progn (backward-paragraph) (point))))
      (pj-remove-display-property start end)
      (pj-unfill-paragraph))))

(defun pj-justify-paragraph ()
  "Justify current paragraph using Knuth/Plass algorithm.

If called on an already justified paragraph, it will be
re-filled."
  (interactive)
  (save-excursion
    (let ((trail-whitespace nil)
          (end-position nil))
      (-when-let* (((start . end) (bounds-of-thing-at-point 'paragraph)))
        ;; keep the whitespace suffix
        (goto-char end)
        (when (looking-at-p "^$")
          (backward-char 1))
        (when (and (looking-at-p "$")
                   (looking-back " +" (line-beginning-position) 'greedy))
          (setq end-position (point-marker))
          (setq trail-whitespace (match-string 0)))
        (pj-unjustify-paragraph)
        (goto-char start)
        (when (looking-at-p "$")
          (forward-line))
        (pj-justify)
        (when trail-whitespace
          (goto-char end-position)
          (insert trail-whitespace))))))

(define-minor-mode pj-auto-justify-mode
  "Automatically justify current paragraph."
  :lighter " PJ"
  (if pj-auto-justify-mode
      (add-hook 'post-self-insert-hook 'pj-justify-paragraph nil 'local)
    (remove-hook 'post-self-insert-hook 'pj-justify-paragraph 'local)))

(provide 'justify-kp)
;;; justify-kp.el ends here
