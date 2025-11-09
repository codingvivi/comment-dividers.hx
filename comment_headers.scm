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


(define (string-repeat s n)
  (let loop ((count n) (result ""))
    (if (<= count 0)
        result
        (loop (- count 1) (string-append result s)))))

(define (insert-line line-length fill-pattern is-header? [padding-pattern " "])
  ;; determine heading and padding
  (let* ([actual-heading (if is-header? (get-line-content!) "")]
         [actual-padding (if is-header? padding-pattern "")])

    ;; delete original line if heading was yoinked
    (when is-header?
      (delete-line-noyank!))

    ;; calculate lengths
    (let* ([heading-length (string-length actual-heading)]
           [delim-length (get-block-delim-length)]
           [padding-length (string-length actual-padding)]
           [fill-pattern-length (string-length fill-pattern)]
           [minimum-enclosure-length (* (+ padding-length fill-pattern-length) 2)]
           [to-fill-length (max 0 (- line-length heading-length delim-length))])

      ;; build fill string
      (let ((fill-string (if (<= minimum-enclosure-length to-fill-length)
                             (let* ([fill-string-length (floor (- (/ to-fill-length 2) padding-length))]
                                    [pattern-repetitions (quotient fill-string-length fill-pattern-length)])
                               (string-repeat fill-pattern pattern-repetitions))
                             "")))

        ;; construct the final line..
        (let ([final-line (string-append fill-string actual-padding actual-heading actual-padding fill-string)])
          ;; ...and paste her...
          (helix.static.insert_string final-line)
          ;; shes beautiful she deserves a snuggly comment block
          (select-line-content!)
          (helix.static.toggle_block_comments)
          ;; remove possible whitespace
          (helix.static.flip_selections)
          (when (not (= (helix.static.get-current-column-number) 0))
                (delete_till_first_nonwhitespace!)))))))
        ;;donezo garbonzo
                           
     
         


(define (test)
        (insert-line 80 "a" #t))
