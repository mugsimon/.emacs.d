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
;; (setq default-process-coding-system '(undecided-dos . utf-8-unix))
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ fonts setting                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package nerd-icons
  :ensure t)

(use-package all-the-icons
  :ensure t)

(defvar all-the-icons-setup-done
  (expand-file-name "~/.emacs.d/.fonts-setup-done"))

(unless (file-exists-p all-the-icons-setup-done)
  ;; Run nerd-icons and all-the-icons setup
  (nerd-icons-install-fonts t)
  (all-the-icons-install-fonts t)
  ;; Update font cache
  (start-process-shell-command "fc-cache" "*Messages*" "fc-cache -f -v")
  ;; Create the flag file
  (write-region "" nil all-the-icons-setup-done))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - start up message                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq inhibit-startup-message t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; show line num
;; (line-number-mode t)
;; show column num
;; (column-number-mode t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  )

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
(use-package timu-macos-theme
  :ensure t
  :config
  (load-theme 'timu-macos t)
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - window transparency                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(add-to-list 'default-frame-alist '(alpha . (0.90 0.85)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - line number                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-width-start nil) ;; Disable dynamic width adjustment
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
;;; @ session                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; save desktop session
(desktop-save-mode)

;; cursor memory
(save-place-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scroll                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; keep kersol when scrolling
(setq scroll-preserve-screen-position nil)
;; small scroll
(setq scroll-conservatively 10000)
;; line overlay when scrolling
(setq next-screen-context-lines 1)
;; keep redisplay
(setq redisplay-dont-pause t)

;; mouse scroll
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed t)

;; horizontal scroll margin
(setq hscroll-margin 1)
(setq hscroll-step 1)

;; horizontal mouse scroll
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ minibuffers                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(fido-vertical-mode t)

;; Enable a persistent minibuffer history 
(savehist-mode 1)
(setq savehist-additional-variables '(search-ring regexp-search-ring))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia)
  :init
  (all-the-icons-completion-mode))

;; Show the depth of minibuffer recursion when using nested commands.
(minibuffer-depth-indicate-mode 1)

;; To make the minibuffer appear centered
;; (setq resize-mini-windows 'grow-only)
;; (setq max-mini-window-height 0.3)

;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-." . embark-act) ;; Actions for the selected candidate
;;    ("C-;" . embark-dwim))) ;; Do what I mean

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ beep off                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq ring-bell-function 'ignore)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rainbow-mode                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package rainbow-mode
  :ensure t
  :init
  (rainbow-mode nil)
  (defun rainbow-mode-limited ()
    "Customize rainbow-mode to only highlight hex color codes."
    (interactive)
    (setq rainbow-html-colors nil) ;; HTMLの色名を無効化
    (setq rainbow-x-colors nil)    ;; CSS色名を無効化
    (setq rainbow-hexadecimal-colors t) ;; 16進数カラーコードのみ
    (when (bound-and-true-p rainbow-mode)
      (rainbow-mode -1)
      (rainbow-mode 1))
    )
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ highlight symbol                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package highlight-symbol
  :ensure t
  :custom
  (highlight-symbol-idle-delay 0.3)
  :bind
  (("M-n" . highlight-symbol-nxext)
   ("M-p" . highlight-symbol-prev))
  :hook
  (prog-mode . highlight-symbol-mode)
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ consult                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Improve evaluation and command completion
;; (use-package consult
;;   :ensure t
;;   :bind
;;   (
;;    ;; ("C-s" . consult-line)
;;    ("M-s l" . consult-line)
;;    ("M-y" . consult-yank-pop)
;;    )
;;   )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Auto Completion                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (use-package company
;;   :ensure t
;;   :init
;;   ;; Enable company-mode globally
;;   (global-company-mode)
;;   :custom
;;   ;; Set delay before completion suggestions appear
;;   (company-idle-delay 0.0)
;;   ;; Minimum prefix length before suggestions are shown
;;   (company-minimum-prefix-length 1)
;;   ;; Enable wrap-around selection in completion candidates
;;   (company-selection-wrap-around t)
;;   ;; Non-exact match
;;   (company-require-match 'never)
;;   ;; Automatic expand
;;   (company-auto-expand t)
;;   ;; Set backends
;;   (company-backends '((company-capf)))
;;   ;; Sort candidates
;;   (company-transformers '(company-sort-by-occurrence
;;                           company-sort-by-backend-importance
;;                           company-sort-prefer-same-case-prefix))
;;   :bind
;;   ;; Use Enter/Return to complete the current selection
;;   (:map company-active-map
;;         ("RET" . company-complete-selection)
;;         ("<return>" . company-complete-selection)
;;         ("<tab>" . company-complete)
;;         )
;;   )

;; (use-package company-statistics
;;   :ensure t
;;   :config
;;   (company-statistics-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (defun ms-corfu-expand ()
    (interactive)
    (unless (corfu-expand)
      (corfu-complete)))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  :bind
  (:map corfu-map
        ("RET" . corfu-complete)
        ("<return>" . corfu-complete)
        ("<tab>" . ms-corfu-expand)
        ("<backtab>" . corfu-reset)
        )
  )

(use-package corfu-prescient
  :ensure t
  :after corfu
  :config
  (corfu-prescient-mode t))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Language Server Protocol                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;; lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (setq read-process-output-max (* 1024 1024))
;;   :custom
;;   ;; clangd
;;   ;; sudo apt install clangd
;;   (lsp-clients-clangd-executable "clangd")
;;   :config
;;   ;; navigation
;;   (define-key lsp-mode-map (kbd "M-n") 'lsp-ui-find-next-reference)
;;   (define-key lsp-mode-map (kbd "M-p") 'lsp-ui-find-prev-reference)
;;   :hook (;; Enable lsp-mode for C, C++
;;          ((c-mode c++-mode) . lsp)
;;          ((racket-mode) . lsp)
;;          )
;;   :commands lsp)

;; ;; lsp-ui
;; (use-package lsp-ui
;;   :ensure t
;;   :custom
;;   ;; lsp-ui-side-line
;;   ;; (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   ;; lsp-ui-peek
;;   (lsp-ui-peek-enable t)
;;   :config
;;   ;; M-. show definitions
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions  )
;;   ;; M-? show referances
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   :commands lsp-ui-mode)

;; Enable lsp-mode for Python
;; Use conda environment
;; mkdir -p ~/miniconda3
;; wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda3/miniconda.sh
;; bash ~/miniconda3/miniconda.sh -b -u -p ~/miniconda3
;; rm ~/miniconda3/miniconda.sh
;; conda activate
;; conda install pyright
;; sudo apt install -y nodejs npm
;; sudo npm install -g pyright
;; (use-package lsp-pyright
;;   :ensure t
;;   :custom
;;   (lsp-pyright-langserver-command "pyright") ;; or basedpyright
;;   (lsp-pyright-python-executable-cmd "~/miniconda3/bin/python")
;;   :hook ((python-mode python-ts-mode) . (lambda ()
;;                                           (require 'lsp-pyright)
;;                                           ;; avoid python-flymake diagnostics, can't detect import
;;                                           (setq flymake-diagnostic-functions nil)
;;                                           (lsp))))  ; or lsp-deferred

;; ;; Enable lsp-mode for scheme
;; ;; https://github.com/emacsmirror/lsp-scheme
;; ;; sudo apt install guile-3.0 guile-3.0-dev
;; (use-package lsp-scheme
;;   :ensure t
;;   :custom
;;   (lsp-scheme-implementation "guile")
;;   :hook (scheme-mode . (lambda ()
;; 			 (require 'lsp-scheme)
;; 			 (lsp-scheme))))


;; Eglot
(use-package eglot
  :after treesit
  :config
  (add-to-list 'eglot-server-programs
               ;; curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
               ;; bash Miniforge3-$(uname)-$(uname -m).sh
               ;; mamba install -c conda-forge python-lsp-server
               ;; '((python-mode python-ts-mode) . ("pylsp")))
               ;; '((python-mode python-ts-mode) . ("~/miniforge3/bin/pylsp")))
               ;; # pyright
               ;; mamba install -c conda-forge nodejs=18 pyright
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; sudo apt install clangd
               '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               ;; git clone https://codeberg.org/rgherdt/scheme-lsp-server.git
               ;; cd scheme-lsp-server
               ;; sudo apt install guix
               ;; guix package -f guix.scm
               ;; GUIX_PROFILE="/home/simon/.guix-profile" # write this line to .bashrc
               ;; source "$GUIX_PROFILE/etc/profile" # write this line to .bashrc
               '(scheme-mode . ("guile-lsp-server")))
  (add-to-list 'eglot-server-programs
               ;; sudo snap install racket
               ;; raco pkg install racket-langserver
               '(racket-mode . ("racket" "-l" "racket-langserver")))
  :custom
  (eglot-ignored-server-capabilities
   ;; disable eglot symbol highlight
   '(:documentHighlightProvider))
  :config
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  :hook
  (
   ((python-mode python-ts-mode) . eglot-ensure)
   ((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure)
   (scheme-mode . eglot-ensure)
   (racket-mode . eglot-ensure)
   )
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tree-sitter                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; M-x treesit-install-language-grammar
(use-package treesit
  :init
  (let ((tree-sitter-dir
         (expand-file-name "tree-sitter/"
                           user-emacs-directory)))
    (dolist (lang '(c cpp python))
      (let ((lib-file (format "%slibtree-sitter-%s.so" tree-sitter-dir lang)))
        (unless (file-exists-p lib-file)
          (treesit-install-language-grammar lang)
          ))))
  :config
  (setq treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '(
          (python-mode . python-ts-mode)          
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          )
        )
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tramp                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 1)
  (tramp-auto-save-directory "/tmp")
  (tramp-connection-timeout 30)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :hook (tramp-cleanup-hook . (lambda ()
                                (message
                                 "Tramp connection lost, trying to reconnect..."))
                            )
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ projectile                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package projectile
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ treemacs                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package treemacs
  :ensure t
  :defer t
  :bind (:map global-map
              ("C-x t t" . treemacs)
              ("C-x t a" . treemacs-add-and-display-current-project)
              ("C-x t e" . treemacs-add-and-display-current-project-exclusively)
              )
  :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  )

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; treemacs theme
(use-package treemacs-all-the-icons
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ flymake                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (add-hook 'prog-mode-hook 'flymake-mode) ; use flymake in program-mode

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ GC Threshold                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq gc-cons-threshold (* 100 1024 1024)) ;100Mb ; default (* 800 1024)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tab mode                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(global-tab-line-mode t)
;; (tab-bar-mode nil)
;; (global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-next-tab)
;; (global-set-key (kbd "C-S-<iso-lefttab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<tab>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-x t k") 'tab-line-close-tab)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto reload                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(global-auto-revert-mode t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto paring                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(electric-pair-mode t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ truncate-lines                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

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
;; (global-set-key (kbd "C-<tab>") #'(lambda() (interactive) (bury-buffer)))
;; (global-set-key (kbd "C-S-<iso-lefttab>") #'(lambda() (interactive) (unbury-buffer)))
;; (global-set-key (kbd "C-<tab>") 'next-buffer)
;; (global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)

;; C-; comment out/in
(defun one-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))
(global-set-key (kbd "C-;") 'one-line-comment)

;; 
(defun custom-move-beginning-of-line ()
  (interactive)
  (if (= (point) (line-beginning-position))
      (back-to-indentation)
  (when (<= (point)
	    (progn (back-to-indentation)
                   (point)))
    (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'custom-move-beginning-of-line)

;; code jump history
;; M-,
(global-set-key (kbd "<mouse-8>") 'xref-go-back)
;; M-C-,
(global-set-key (kbd "<mouse-9>") 'xref-go-forward)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tab space                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq-default indent-tabs-mode nil)

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

;; cmake
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  )

;; racket
(use-package racket-mode
  :ensure t
  )

;; yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  )

;; python
(setq python-indent-offset 4)
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home "~/miniforge3")
  (setq conda-env-home-directory "~/miniforge3")
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-mode-line-setup)
  :hook
  ((conda-postactivate . (lambda ()
                           (eglot-reconnect
                            (eglot-current-server))))
   (conda-postdeactivate . (lambda ()
                           (eglot-reconnect
                            (eglot-current-server)))))
  )
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ multiple-cursors                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(unless (package-installed-p 'multiple-cursors)
  (package-refresh-contents)
  (package-install 'multiple-cursors))
(require 'multiple-cursors)
;; C-u C-M-SPC
(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-all-dwim)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/unmark-next-like-this)

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ which key                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ custom file load                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror 'nomessage)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ custom theme                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq custom-theme-file "~/.emacs.d/themes/custom-theme.el")
(load custom-theme-file 'noerror 'nomessage)
