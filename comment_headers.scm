(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static "helix/static.scm"))
(require (prefix-in helix. "helix/editor.scm"))

(define (make-language-header-settings line-p block-p filler)
  (hash 'line-prefix line-p
        'block-prefix block-p
        'filler_char filler))

(define language-settings-db
  (let* ([standard-filler "~"]
         [c-family-settings (make-language-header-settings  "//" "/*" standard-filler)])
    (hash
     "c" c-family-settings
     "cpp" c-family-settings
     "hpp" c-family-settings
     "scm" (make-language-header-settings ";;" "#|" standard-filler))))


(define (get-selection-length)
  (string-length (helix.current_selection)))

(define (reverse-string str)
  (list->string (reverse (string->list str))))


(define (get-delims language-settings)
  (let* ([line-prefix (hash-ref language-settings 'line-prefix)]
         [block-prefix (hash-ref language-settings 'block-prefix)])
    (if (= (string-length block-prefix) 0)
        (values block-prefix (reverse-string block-prefix))
        (values line-prefix line-prefix))))

(define (get-current-lang)
     (let* ([focus (helix.editor-focus)]
            [doc-id (helix.editor->doc-id focus)])
       (helix.editor-document->language doc-id)))

; (define (repeat n action)
;   (let loop ([i 0])
;     (when (< i n)
;           (action)
;           (loop (+ i 1)))))

use the inbuilt block comment function! 


; (define (get-fill-string-length line-length heading prefix-delim postfix-delim)
;   (let* ([heading-length (string-length heading)]
;          [prefix-length (+ (string-length prefix-delim) 1)]
;          [postfix-length (+ (string-length postfix-delim) 1)])
;     (/ (- line-length heading-length prefix-length postfix-length) 2)))

; (define (concat-line line-length heading prefix-delim postfix-delim filler-char)
;   (let* ([fill-string-length (get-fill-string-length line-length heading prefix-delim postfix-delim)]
;          [fill-string (make-string fill-string-length filler-char)])
;     (make-string prefix-delim " " fill-string heading fill-string postfix-delim)))

(define (make-line language heading)
  (define (language-settings (hash-ref language-settings-db language))))

;  (define (make-header)
;    ;; get language
   
;    ;;get what chars to use
   
;    (define settings (hash-ref language-settings-db current-language "c"))
;    (let-values (((start-delim end-delim)(get-delims settings)
;             ))))
     
   


; ; (provide make-header)
