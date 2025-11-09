; (require  "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.components. "helix/components.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require (prefix-in helix.keymaps. "helix/keymaps.scm"))
(require (prefix-in helix.misc. "helix/misc.scm"))
(require (prefix-in pphx.selection. "./core/selection.scm"))

 (provide ;write-divider-string
          test)
    
(define (do-n-times action n)
  (when (> n 0)
    (action)
    (do-n-times action (- n 1))))

(define (delete-line-noyank!)
   (helix.static.extend_to_line_bounds)
   (helix.static.delete_selection))
  
(define (block-comment-line)
   (helix.static.extend_to_line_bounds)
   (helix.static.toggle_block_comments))

(define (get-selection-absolute-length)
         (string-length (helix.static.current-highlighted-text!)))

(define (select-line-content!)
  (helix.static.extend_to_line_bounds)
  (helix.static.trim_selections))

(define (get-line-content!)
         (select-line-content!)
         (helix.static.current-highlighted-text!))

(define (get-block-delim-length)
   (helix.static.open_above)
   (helix.static.insert_char #\o)
   (helix.static.normal_mode)
   (helix.static.extend_to_line_bounds)
   (helix.static.trim_selections)
   (helix.static.toggle_block_comments)
    (let* ([total-length (get-selection-absolute-length)]
           [test-char-length 1]
           [block-delim-length (- total-length test-char-length)])
             (helix.static.delete_selection_noyank)
             block-delim-length))


(define (delete_till_first_nonwhitespace!)
         (helix.static.goto_line_start)
         (helix.static.extend_to_first_nonwhitespace)
         (helix.static.extend_char_left)
         (helix.static.delete_selection_noyank))

; (define (get-fill-string-len line-length heading)
;          (let* ([heading-length (string-length heading)]
;                 [padding-length 2]
;                 [to-fill-length (- line-length heading-length padding-length)])
;                   (quotient to-fill-length 2)))
                 
; (define (get-heading-contents line-length heading outside-delim-length)
;          (let* ([heading-length (string-length heading)]
;                 [diff-linelen-headinglen (- line-length heading-length)]
;                 [minimum-padding 4])
;                   (if (< diff-linelen-headinglen minimum-padding)
;                       heading
;                       (make-enclosed-heading line-length))

;                 ))

; (define (make-inside-string line-length line-contents fill-char outside-delim-length is-header?)
;          (if is-header?
;              (make-header line-length line-contents outside-delim-length)
;              (make-string (- line-length outside-delim-length) fill-char)))

(define (insert-string)
  #t)

(define (insert-line line-length fill-pattern is-header? [padding-pattern " "])
         ;; empty heading & fill string
         (let ([heading ""]
               [fill-string ""]
               [padding ""])
                  ;; if constructing header, cut out line contents
                  (when is-header?
                        (string-append heading (get-line-content!))
                        (delete-line-noyank!)
                        (string-append padding padding-pattern))
                  ;; calculate length to fill
                   (let* ([heading-length (string-length heading)]
                          [delim-length (get-block-delim-length)]
                          [padding-length (string-length padding)]
                          [fill-pattern-length (string-length fill-pattern)]
                          [minimum-enclosure-length (* (+ padding-length fill-pattern-length) 2)]
                          [to-fill-length (max 0 (- line-length heading-length delim-length))])
                           ; if we're filling out the area between comment block delims
                           (when (<= minimum-enclosure-length to-fill-length)
                                 (let* ([fill-string-length (- (/ to-fill-length 2) padding-length)]
                                        [pattern-repetitions (quotient fill-string-length fill-pattern-length)])
                                          (do-n-times (string-append fill-string fill-pattern) pattern-repetitions)))
                           (string-append fill-string padding heading padding (reverse fill-string))
                           ; (insert-string)
                           )))
                           
         ;           (let* ([line-contents]
         ; [block-delim-length (get-block-delim-length)]
         ; )
         ;   )
         ;   ()


         ; [inside-string (make-inside-string line-length line-contents fill-char block-delim-length is-header?)])
         ;   (helix.static.insert_string inside-string)
         ;   (select-line-content!)
         ;   (helix.static.toggle_block_comments)
         ;   (helix.static.flip_selections)
         ;   (when (not (= (helix.static.get-current-column-number) 0))
         ;         (delete_till_first_nonwhitespace!))
         ;   (when is-header?
         ;         (helix.static.move_line_down)
         ;         (delete-line-noyank!))))

 
 
         


(define (test)
        (insert-line 80 "a" #t))
