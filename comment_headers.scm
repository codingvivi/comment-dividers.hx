(require (prefix-in helix.static. "helix/static.scm"))

 (provide ;write-divider-string
          test)
    
(define (do-n-times action n)
  (when (> n 0)
    (action)
    (do-n-times action (- n 1))))

(define (repeat val times)
  (cond
	[(and (integer? times) (> times 0))
		(map (λ (_) val) (range 0 times))]
	[else
	  (error 'invalid-arg "'times' must be a positive nonzero integer.")]))

(define (intersperse lst char)
  (let* ([num-in-between-spaces (- (length lst) 1)]
         [filler-list (repeat char num-in-between-spaces)])
  (interleaving lst filler-list)))

(define (move-current-line-down!)
    (helix.static.open_above)
    (helix.static.normal_mode))

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

(define (make-line line-length fill-pattern is-header? [padding " "])
  ;; determine heading and padding
  (let* ([heading (if is-header? (get-line-content!) "")]
         [padding-heading (if is-header? padding "")])  
      ;; get delims
      (let* ([block-delims (get-block-delims)]
             [full-start-delim (string-append (car block-delims) padding)]
             [full-end-delim (string-append padding (cadr block-delims))]
             ;;calculate lengths
             [padding-heading-length (string-length padding-heading)]
             [heading-length (string-length heading)] 
             [padded-heading-length (string-append )]
             [full-delim-length (+ (string-length full-start-delim)
                               (string-length full-end-delim))]
             [fill-pattern-length (string-length fill-pattern)]
             [minimum-enclosure-length (* (+ padding-heading-length fill-pattern-length)2)]
             [to-fill-length (max 0 (- line-length heading-length full-delim-length))])
  
        ;; build fill string
        (let ([fill-string (if (<= minimum-enclosure-length to-fill-length)
                               (let* ([fill-string-length (floor (- (/ to-fill-length 2) padding-heading-length))]
                                      [pattern-repetitions (quotient fill-string-length fill-pattern-length)])
                                 (string-repeat fill-pattern pattern-repetitions))
                               "")])
          ;; construct the final line..
          (string-append full-start-delim fill-string padding-heading heading padding-heading fill-string full-end-delim)))))


 (define (make-heading-list line-length fill-pattern [padding-pattern " "] [heading-level 1] [max-level 3])

  (let* ([distance-from-lowest-level (- max-level heading-level)]
         [headline-string (if (= distance-from-lowest-level 0)
                              (make-line line-length fill-pattern #t padding-pattern)
                              (make-line line-length " " #t padding-pattern))]
         [divider-line-string (if (>= distance-from-lowest-level 1)
                                  (make-line line-length fill-pattern #f padding-pattern)
                                  "")]
         [padding-line-string (if (>= distance-from-lowest-level 2)
                                  (make-line line-length fill-pattern #f padding-pattern)
                                  "")]
         [max-line-above-heading (- max-level 1)])
    
    (define (construct-lines line-above-heading)
      (if (= line-above-heading 0)
          '()
          (if (< line-above-heading max-line-above-heading)      
              (cons padding-line-string (construct-lines (- line-above-heading 1)))
              (cons divider-line-string (construct-lines (- line-above-heading 1))))))

    (let* ([encl-lines-top (construct-lines max-line-above-heading)]
          [encl-lines-bottom (reverse encl-lines-top)])
      (string-append encl-lines-top headline-string encl-lines-bottom))))                            

(define (insert-header! line-length fill-pattern line-length fill-pattern [padding-pattern " "] [heading-level 1] [max-level 3])
  (let* ([header-items (make-heading-list line-length fill-pattern padding-pattern heading-level max-level)]
         [header-string (list->string (intersperse header-items #\newline))])
    (delete-line-noyank!)
    (helix.static.insert_string header-string)))


(define (insert-divider-line line-length fill-pattern [padding-pattern " "])
  (let ([divider-string (make-line line-length fill-pattern #f padding-pattern)])
    (move-current-line-down!)
    (helix.static.insert_string divider-string)))

  ; (let ([single-line-header-string 
  ;       [multi-line-header-string (make-line line-length " " #t padding-pattern)]
  ;       [divider-line-string (make-line line-length fill-pattern #f padding-pattern)]
  ;       [padding-line-string (make-line line-length " " #f padding-pattern)])
  ;   (delete-line-noyank!)))



; (define (test)
  ; (insert-multi-line-heading 80))
