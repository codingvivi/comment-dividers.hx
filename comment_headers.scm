; (require  "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.components. "helix/components.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require (prefix-in helix.keymaps. "helix/keymaps.scm"))
(require (prefix-in helix.misc. "helix/misc.scm"))
(require (prefix-in pphx.selection. "./core/selection.scm"))

 (provide ;write-divider-string
          test)
    
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

(define (make-header line-length heading outside-delim-length)
         
         (let* ([heading-length (string-length heading)]
                [padding " "]
                [padded-heading (string-append padding heading padding)])
                  (if (>= (+ heading-length outside-delim-length) line-length)
                  heading
                  padded-heading)))

(define (make-inside-string line-length line-contents fill-char outside-delim-length is-header?)
         (if is-header?
             (make-header line-length line-contents outside-delim-length)
             (make-string (- line-length outside-delim-length) fill-char)))

(define (insert-line line-length fill-char is-header?)
  (let* ([line-contents (get-line-content!)]
         [block-delim-length (get-block-delim-length)]
         [inside-string (make-inside-string line-length line-contents fill-char block-delim-length is-header?)])
           (helix.static.insert_string inside-string)
           (select-line-content!)
           (helix.static.toggle_block_comments)
           (helix.static.flip_selections)
           (when (not (= (helix.static.get-current-column-number) 0))
                 (delete_till_first_nonwhitespace!))
           (when is-header?
                 (helix.static.move_line_down)
                 (delete-line-noyank!))
    ))

 
 



(define (test)
        (insert-line 80 #\a #t))
