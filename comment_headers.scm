(require (prefix-in helix.static. "helix/static.scm"))

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


(define (split-list-even list)
(let* ([list-length (length list)]
      [midpoint (/ list-length 2)])
  (values (take list midpoint)
          (drop list midpoint))))

(define (remove-element list element-to-remove)
  (filter (lambda (item)
                  (not (equal? item element-to-remove)))
          list))

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

(define (get-block-delims)
   (helix.static.open_above)
   (helix.static.insert_string "test_string")
   (helix.static.normal_mode)
   (block-comment-line)
   (let* ([commented-test-string (helix.static.current-highlighted-text!)]
         [strings-without-whitespace (split-whitespace commented-test-string)])
     (delete-line-noyank!)
     (values (first strings-without-whitespace)
             (last strings-without-whitespace))))

    ; (let* ([total-length (get-selection-absolute-length)]
    ;        [test-char-length 1]
    ;        [block-delim-length (- total-length test-char-length)])
    ;          
    ;          block-delim-length))

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
          ; (select-line-content!)
          ; (helix.static.toggle_block_comments)
          ;; remove possible whitespace
          ; (helix.static.flip_selections)
          ; (when (not (= (helix.static.get-current-column-number) 0))
                ; (delete_till_first_nonwhitespace!))
        ;;donezo garbonzo
                           
)))))     
         

(define (insert-single-line-heading line-length fill-pattern [padding-pattern " "])
  (insert-line line-length fill-pattern #t padding-pattern))

(define (insert-divider-line line-length fill-pattern)
  (insert-line line-length fill-pattern #f ""))

(define (insert-multi-line-heading line-length [padding-pattern " "])
  (insert-line line-length " " #t padding-pattern))                                                                         
                                                                                                                                                  

(define (test)  
(get-block-delims))
