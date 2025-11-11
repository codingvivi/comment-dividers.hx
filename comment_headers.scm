(require (prefix-in helix.static. "helix/static.scm"))

 (provide insert-header!
          insert-divider!
          insert-h1!
          insert-h2!
          insert-h3!)
    
(define (string-not-empty? str)
  (> (string-length str) 0))

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
     (list (first strings-without-whitespace)
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

(define (only-whitespace? str)
  (for-each (list->string str)))

; (define (is-header? content-str)
;   (if (and (string-not-empty? content-str))
;       #t)
;   #t)


(define (make-line line-length line-content fill-pattern [padding " "])
  ;; determine heading and padding
  (let* ([is-content-header?  (if (string-not-empty? line-content) #t #f)]
         [padding-heading (if is-content-header? padding "")])  
      ;; get delims
      (let* ([block-delims (get-block-delims)]
             [full-start-delim (string-append (car block-delims) padding)]
             [full-end-delim (string-append padding (cadr block-delims))]
             ;;calculate lengths
             [padding-heading-length (string-length padding-heading)]
             [heading-length (string-length line-content)] 
             [full-heading-length (+ padding-heading-length
                                     heading-length
                                     padding-heading-length)]
             [full-delim-length (+ (string-length full-start-delim)
                                   (string-length full-end-delim))]
             [fill-pattern-length (string-length fill-pattern)]
             [minimum-enclosure-length (* (+ padding-heading-length fill-pattern-length) 2)]
             [to-fill-length (max 0 (- line-length heading-length full-delim-length))])
  
        ;; build fill string
        (let ([fill-string (if (and (string-not-empty? fill-pattern) (<= minimum-enclosure-length to-fill-length))
                               (let* ([fill-string-length (floor (- (/ to-fill-length 2) padding-heading-length))]
                                      [pattern-repetitions (quotient fill-string-length fill-pattern-length)])
                                 (string-repeat fill-pattern pattern-repetitions))
                               "")])
          ;; construct the final line..
          (string-append full-start-delim fill-string padding-heading line-content padding-heading fill-string full-end-delim)))))


(define (make-heading-list line-length fill-pattern [heading-level 1] [max-level 3] [padding-pattern " "])
  ;; let statements
  (let* ([distance-from-lowest-level (- max-level heading-level)]
         ;; contruct strings
         [line-content (get-line-content!)]
         [headline-string (if (= distance-from-lowest-level 0)
                              (make-line line-length line-content fill-pattern padding-pattern)
                              (make-line line-length line-content " " padding-pattern))]
         [divider-line-string (if (>= distance-from-lowest-level 1)
                                  (make-line line-length "" fill-pattern padding-pattern)
                                  "")]
         [padding-line-string (if (>= distance-from-lowest-level 2)
                                  (make-line line-length "" " " padding-pattern)
                                  "")]
         ;; number of lines to insert above heading
         [max-line-above-heading (- max-level 1)])
    
    (define (construct-lines curr-line-above-heading)
      (if (= curr-line-above-heading 0)
          '()
          (if (< curr-line-above-heading max-line-above-heading)      
              (cons padding-line-string (construct-lines (- curr-line-above-heading 1)))
              (cons divider-line-string (construct-lines (- curr-line-above-heading 1))))))
    
    (let* ([encl-lines-top (construct-lines max-line-above-heading)]
           [encl-lines-bottom (reverse encl-lines-top)])
      (append encl-lines-top (list headline-string) encl-lines-bottom))))                            

(define (insert-header! line-length fill-pattern [heading-level 1] [max-level 3] [padding-pattern " "])
  (let ([header-lines (make-heading-list line-length fill-pattern heading-level max-level padding-pattern)])
    (delete-line-noyank!)
    (for-each (lambda (line-str)
                      (helix.static.insert_string line-str)
                      (helix.static.insert_newline))
              header-lines)))

(define (insert-h1!)
  (insert-header! 80 "~" 1 3))

(define (insert-h2!)
  (insert-header! 80 "~" 2 3))

(define (insert-h3!)
  (insert-header! 80 "~" 3 3))

(define (insert-divider! line-length fill-pattern [padding-pattern " "])
  (let ([divider-string (make-line line-length fill-pattern #f padding-pattern)])
    (move-current-line-down!)
    (helix.static.insert_string divider-string)))



; (define (test number)
  ; (when (zero? (string->int number))
        ; (insert-header! 80 "~" " " 1 1)))
