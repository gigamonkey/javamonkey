
; xemacs doesn't have match-string-no-properties
(if (not (fboundp 'match-string-no-properties))
    (defalias 'match-string-no-properties 'match-string))


(provide 'lisp-compatibility)
