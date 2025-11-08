; (require  "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.components. "helix/components.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require (prefix-in helix.keymaps. "helix/keymaps.scm"))
(require (prefix-in helix.misc. "helix/misc.scm"))
(require (prefix-in pphx.selection. "./core/selection.scm"))

 (provide ;write-divider-string
          test)
    
 
  
(define (block-comment-line)
   (helix.static.extend_to_line_bounds)
   (helix.static.toggle_block_comments))

(define (get-selection-absolute-length)
         (string-length (helix.static.current-highlighted-text!)))

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
      total-length))

(define (select-line-content-only)
  (helix.static.extend_to_line_bounds)
  (helix.static.trim_selections))

(define (make-header line-length outside-delim-length)
         (let* ([heading (helix.static.current-highlighted-text!)]
                [heading-length (string-length heading)]
                [padding " "]
                [padded-heading (string-append padding heading padding)])
                  (if (>= (+ heading outside-delim-length) line-length)
                  heading
                  padded-heading)))

(define (make-inside-string line-length fill-char outside-delim-length is-header?)
         (if (not is-header?)
             (make-string (- line-length 2) fill-char)
             (make-header line-length outside-delim-length)))

(define (insert-line line-length filler-char is-header?)
  (select-line-content-only)
  (let* ([block-delim-length (get-block-delim-length)]
         [inside-string (make-inside-string)])
        [non-filler-length (+ heading-length heading-padding-length block-delim-length)]
        [fill-string-length (quotient (- line-length non-filler-length) 2 )]
        [fill-string (make-string fill-string-length filler-char)]
        [full-string (string-append fill-string padding heading padding fill-string)])
    (helix.static.add_newline_below)
    ; (helix.static.extend_to_line_end)
    ; (helix.static.delete_selection_noyank)
    ; (helix.static.insert_string full-string)
    ; (select-line-content-only)
    ; (helix.static.toggle_block_comments)
    ))

(define (test)
         (insert-line 80 #\a))
