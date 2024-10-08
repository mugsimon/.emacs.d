;;; emacs setting file
;;; mugsimon

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; default 
(set-default-coding-systems 'utf-8-unix)
;; text file, new buffer
(prefer-coding-system 'utf-8-unix)
;; file name
(set-file-name-coding-system 'utf-8-unix)
;; keyboard input
(set-keyboard-coding-system 'utf-8-unix)
;; subprocess
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - start up message                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq inhibit-startup-message t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; show line num
(line-number-mode t)
;; show column num
(column-number-mode t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Delete search string with C-d
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)
;; Edit search string with C-e
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)
;; Completion search string with TAB
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - theme                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(load-theme 'wombat t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - window transparency                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(add-to-list 'default-frame-alist '(alpha . (0.90 0.85)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - line number                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))
(setq display-line-numbers-width-start t)
(setq display-line-numbers-width 4)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - backup                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; Backup(~)
(setq make-backup-files   t)  ;; auto backup
(setq version-control     t)  ;; add version to backup file
(setq kept-new-versions   3)  ;; number of latest backup files
(setq kept-old-versions   0)  ;; number of oldest backup files
(setq delete-old-versions t)  ;; delete backup file
;; backup directory
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
            backup-directory-alist))
;;; Auto Save
;; auto backup while editing
(setq backup-inhibited nil)
;; not delete auto backup file when close
(setq delete-auto-save-files nil)
;; not add specific file name and prefix for auto backup
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
;; backup interval (second)
(setq auto-save-timeout 3)
;; backup interval key stroke
(setq auto-save-interval 100)
;; editing file directory(##)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; make no lockfile
(setq create-lockfiles nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scroll                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; keep kersol when scrolling
(setq scroll-preserve-screen-position t)
;; small scroll
(setq scroll-conservatively 10000)
;; line overlay when scrolling
(setq next-screen-context-lines 1)
;; keep redisplay
(setq redisplay-dont-pause t)
;; horizontal scroll margin
(setq hscroll-margin 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto complete                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(unless (package-installed-p 'auto-complete)
  (package-refresh-contents)
  (package-install 'auto-complete))
;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; add complete mode
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'nxml-mode)
;; complete with TAB key
(ac-set-trigger-key "TAB")
;; complete menu
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ symbol highlight                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(unless (package-installed-p 'highlight-symbol)
  (package-refresh-contents)
  (package-install 'highlight-symbol))
(require 'highlight-symbol)
;; highlight delay
(setq highlight-symbol-idle-delay 0.5)
;; auto highlight
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
;; M-p/M-n move kersol between symbols
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tree-sitter                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (unless (package-installed-p 'tree-sitter)
;;   (package-refresh-contents)
;;   (package-install 'tree-sitter))

;; ;; (use-package tree-sitter)
;; ;;   :config
;; ;;   (setq treesit-font-lock-level 4))

;; (unless (package-installed-p 'tree-sitter-langs)
;;   (package-refresh-contents)
;;   (package-install 'tree-sitter-langs))


;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)

;; (setq treesit-font-lock-level 4)

;; (add-hook 'c-mode-hook #'tree-sitter-mode)
;; (add-hook 'c-mode-hook #'tree-sitter-hl-mode)



;; (use-package tree-sitter-hl
;;   :ensure t
;;   :hook (tree-sitter-after-on . tree-sitter-hl-mode))


;; (unless (package-installed-p 'treesit-auto)
;;   (package-refresh-contents)
;;   (package-install 'treesit-auto))
;; (require 'treesit-auto)
;; (global-treesit-auto-mode)

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))

;; (setq treesit-language-source-alist
;;   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;     (c "https://github.com/tree-sitter/tree-sitter-c")
;;     (cmake "https://github.com/uyha/tree-sitter-cmake")
;;     (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
;;     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;     (css "https://github.com/tree-sitter/tree-sitter-css")
;;     (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
;;     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;     (go "https://github.com/tree-sitter/tree-sitter-go")
;;     (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
;;     (html "https://github.com/tree-sitter/tree-sitter-html")
;;     (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;     (json "https://github.com/tree-sitter/tree-sitter-json")
;;     (lua "https://github.com/Azganoth/tree-sitter-lua")
;;     (make "https://github.com/alemuller/tree-sitter-make")
;;     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;     (python "https://github.com/tree-sitter/tree-sitter-python")
;;     (r "https://github.com/r-lib/tree-sitter-r")
;;     (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;     (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
;;     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-font-lock-level 4)
;;   :config
;;   (setq treesit-auto-install 'prompt)
;;   (global-treesit-auto-mode))

 (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(c-or-c++-mode . c-or-c++-ts-mode))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tabbar mode                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(tab-bar-mode 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto reload                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(global-auto-revert-mode t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto paring                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ undo tree                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(unless (package-installed-p 'undo-tree)
  (package-refresh-contents)
  (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode t)
;; save history file in specified directory
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history/")))
;; if the directory no exist, then make it
(unless (file-exists-p "~/.emacs.d/undo-tree-history/")
  (make-directory "~/.emacs.d/undo-tree-history/" t))
; Undo C-/, Redo C-S-/

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ shortcut                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; C-TAB, C-Shift-TAB buffer switch
(global-set-key (kbd "C-<tab>") '(lambda() (interactive) (bury-buffer)))
(global-set-key (kbd "C-S-<iso-lefttab>") '(lambda() (interactive) (unbury-buffer)))
;; C-; comment out/in
(defun one-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))
(global-set-key (kbd "C-;") 'one-line-comment)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ edit mode                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; markdown
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Japanese input (for Japanese users)                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
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
  (when current-input-method
    (toggle-input-method)))
(global-set-key [muhenkan] 'ime-off)
(add-hook 'mozc-mode-hook
	  (lambda ()
	    (define-key mozc-mode-map [muhenkan] 'ime-off)))
;; フォント
(set-fontset-font t 'japanese-jisx0208 "Migu 1M")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(display-theme mozc)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
