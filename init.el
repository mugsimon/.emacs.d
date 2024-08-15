;;; emacs setting file
;;; mugsimon

;;;;;;;;;;;;;;;;;;;;;;;
;;; package manager ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screen - start up message ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screen - mode line ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show line num
(line-number-mode t)
;; show column num
(column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; screen - isearch ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete search string with C-d
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)
;; Edit search string with C-e
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)
;; Completion search string with TAB
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;;;;;;;;;;;;;;;;;;;;;;
;;; screen - theme ;;;
;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'wombat t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screen - window transparency ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'default-frame-alist '(alpha . (0.90 0.85)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; screen - line number ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))
(setq display-line-numbers-width-start t)
(setq display-line-numbers-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Japanese input (for Japanese users) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'mozc)
  (package-refresh-contents)
  (package-install 'mozc))
(require 'mozc)
(setq default-input-method "japanese-mozc")
;; 半角/全角キーで切り替え
(global-set-key [zenkaku-hankaku] 'toggle-input-method)
;; 変換キーでmozcオン
(defun ime-on ()
  (interactive)
  (unless current-input-method
    (toggle-input-method)))
(global-set-key [henkan] 'ime-on)
;; 無変換キーでmozcオフ
(defun ime-off ()
  (interactive)
  (if current-input-method
      (toggle-input-method)))
(global-set-key [muhenkan] 'ime-off)
;; フォント
(set-fontset-font t 'japanese-jisx0208 "Migu 1M")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(mozc)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
