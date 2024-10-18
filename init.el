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
;; (unless (package-installed-p 'auto-complete)
;;   (package-refresh-contents)
;;   (package-install 'auto-complete))
;; ;; auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; ;; add complete mode
;; (add-to-list 'ac-modes 'text-mode)
;; (add-to-list 'ac-modes 'org-mode)
;; (add-to-list 'ac-modes 'fundamental-mode)
;; (add-to-list 'ac-modes 'nxml-mode)
;; ;; complete with TAB key
;; (ac-set-trigger-key "TAB")
;; ;; complete menu
;; (setq ac-use-menu-map t)
;; (setq ac-use-fuzzy t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ company mode                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; company
(unless (package-installed-p 'company)
  (package-refresh-contents)
  (package-install 'company))
(require 'company)
;; enable company-mode globally
(global-company-mode)
;; set delay before completion suggestions appear
(setq company-idle-delay 0.0)
;; minimum prefix length before suggestions are shown
(setq company-minimum-prefix-length 1)
;; enable wrap-around selection in completion candidates
(setq company-selection-wrap-around t)
;; non exact match
(setq company-require-match 'never)
;; automatic expand
(setq company-auto-expand t)
;; show frequently used word
(setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
;; enable tab-and-go
;; (company-tng-configure-default)
;; Use Enter/Return to complete the current selection
(define-key company-active-map (kbd "RET") 'company-complete-selection)
(define-key company-active-map (kbd "<return>") 'company-complete-selection)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ lsp mode                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; lsp-mode
(unless (package-installed-p 'lsp-mode)
  (package-refresh-contents)
  (package-install 'lsp-mode))
(require 'lsp-mode)

;; Enable lsp-mode for C and C++
;; sudo apt install clangd
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
;; Set clangd as the LSP server
(setq lsp-clients-clangd-executable "clangd")

;; Enable lsp-mode for Python
;; sudo apt install -y nodejs npm
;; sudo npm install -g pyright
(unless (package-installed-p 'lsp-pyright)
  (package-refresh-contents)
  (package-install 'lsp-pyright))
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
;; ;; Use conda environment
;; mkdir -p ~/miniconda3
;; wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda3/miniconda.sh
;; bash ~/miniconda3/miniconda.sh -b -u -p ~/miniconda3
;; rm ~/miniconda3/miniconda.sh
(setq lsp-pyright-python-executable-cmd "~/miniconda3/bin/python")

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
;; https://emacs-tree-sitter.github.io/
(unless (package-installed-p 'tree-sitter)
  (package-refresh-contents)
  (package-install 'tree-sitter))
(require 'tree-sitter)

(unless (package-installed-p 'tree-sitter-langs)
  (package-refresh-contents)
  (package-install 'tree-sitter-langs))
(require 'tree-sitter-langs)
;; use tree-sitter all mode
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;; Some syntax highlights are incorrect without this setting(Cpp, js)
(setq tree-sitter-hl-use-font-lock-keywords nil)

;; Define custom grammar mapping for Scheme files with racket
(add-to-list 'tree-sitter-major-mode-language-alist '(scheme-mode . racket))


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
;;; @ multiple-cursors                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(unless (package-installed-p 'multiple-cursors)
  (package-refresh-contents)
  (package-install 'multiple-cursors))
(require 'multiple-cursors)
;; C-u C-M-SPC
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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
