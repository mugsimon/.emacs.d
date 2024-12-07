;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ timu-macos-theme custom                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (equal custom-enabled-themes '(timu-macos))
  (let ((timu-macos-custom-file
         "~/.emacs.d/themes/timu-macos-theme-custom.el"))
    (load timu-macos-custom-file 'noerror 'nomessage)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ common custom                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (custom-set-faces
;;    '(highlight-symbol-face
;;      ((t (:background "#414141" :weight bold)))))
