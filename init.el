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
;; text file, new buffer
(prefer-coding-system 'utf-8-unix)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ fonts setting                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package nerd-icons
  :ensure t
  :init
  (defvar icons-fonts-setup-done
    (expand-file-name ".fonts-setup-done" user-emacs-directory))
  (unless (file-exists-p icons-fonts-setup-done)
    ;; Run nerd-icons setup
    (nerd-icons-install-fonts t)
    ;; Update font cache
    (start-process-shell-command "fc-cache" "*Messages*" "fc-cache -f -v")
    ;; Create the flag file
    (write-region "" nil icons-fonts-setup-done)))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ basic                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq use-short-answers t)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - start up message                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setopt inhibit-startup-message t)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
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
  (load-theme 'timu-macos t))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - window transparency                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(add-to-list 'default-frame-alist '(alpha . (0.90 0.85)))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - line number                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t))
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
(setq auto-save-default t) ;; enable auto save
(setq auto-save-timeout 3) ;; backup interval (second)
(setq auto-save-interval 100) ;; backup interval key stroke
;; editing file directory(##)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))
(setq auto-save-list-file-prefix "/tmp/emacsbk/.saves-")
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; make no lockfile
(setq create-lockfiles nil)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ session                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; save desktop session file to .emacs.d
(setopt desktop-dirname (expand-file-name "desktop" user-emacs-directory))
(setopt desktop-path (list desktop-dirname))
(unless (file-exists-p desktop-dirname)
  (make-directory desktop-dirname t))
;; save desktop session
(desktop-save-mode)
;; cursor memory
(save-place-mode)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scroll                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; keep kersol when scrolling
(setopt scroll-preserve-screen-position t)
;; small scroll
;; (setopt scroll-conservatively 10000)
;; line overlay when scrolling
;; (setopt next-screen-context-lines 1)
;; disable redisplay
(setopt redisplay-dont-pause nil)

;; disable progressive speed
(setopt mouse-wheel-progressive-speed nil)

;; scroll margin
(setopt scroll-margin 0)

;; horizontal scroll margin
(setopt hscroll-margin 0)

;; horizontal mouse scroll
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; pixel scroll
(pixel-scroll-precision-mode t)
(setopt pixel-scroll-precision-interpolation-factor 1.0) ;; default 2.0
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ minibuffers                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(fido-vertical-mode)

;; Enable a persistent minibuffer history
(savehist-mode)
(setopt savehist-additional-variables '(search-ring
                                      regexp-search-ring))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode))

;; Show the depth of minibuffer recursion when using nested commands.
(minibuffer-depth-indicate-mode)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((eglot (styles . (orderless flex)))))
  ;; (completion-category-overrides '((file (styles basic))))
  )
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ beep off                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setopt ring-bell-function 'ignore)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rainbow-mode                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode nil)
  (defvar rainbow-mode-hex-only-p nil
    "If non-nil, rainbow-mode will hightlight only hexadecimal color codes.")
  (defun rainbow-mode-toggle-hex-only ()
    "Toggle rainbow-mode between highlighting only hexadecimal colors and all colors."
    (interactive)
    (if rainbow-mode-hex-only-p
	(progn (setopt rainbow-html-colors t) ;; enable HTML color
	       (setopt rainbow-x-colors t)    ;; enable CSS color
	       (message "Rainbow mode: All colors highlighted"))
      (progn (setopt rainbow-html-colors nil) ;; enable HTML color
	     (setopt rainbow-x-colors nil)    ;; enable CSS color
	     (setopt rainbow-hexadecimal-colors t)
	     (message "Rainbow mode: Highlighting only hexadecimal colors")))
    (setopt rainbow-mode-hex-only-p (not rainbow-mode-hex-only-p))
    (rainbow-mode t)))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ highlight symbol                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package highlight-symbol
  :ensure t
  :custom
  (highlight-symbol-idle-delay 0.3)
  :bind
  (("M-n" . highlight-symbol-next)
   ("M-p" . highlight-symbol-prev))
  :hook
  (prog-mode . highlight-symbol-mode))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ consult embark                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Improve evaluation and command completion
(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)     ;; enhance switch-to-buffer
   ("M-g g". consult-goto-line)   ;; enhance goto-line
   ("M-y" . consult-yank-pop)     ;; enhance yank-pop
   ;; `search-map' M-s bindings
   ("M-s d" . consult-find)       ;; search files with find in dir
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s l" . consult-line)       ;; enhance isearch-forward
   ("M-s L" . consult-line-multi)
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history) ;; enhance isearch-edit-string
   ("M-s l" . consult-line)       ;; enhance isearch-forward
   ("M-s L" . consult-line-multi)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)    ;; Actions for the selected candidate
   ("C-;" . embark-dwim))) ;; Do what I mean

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ github copilot                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package copilot
  :ensure t
  :hook
  (prg-mode . copilot-mode)
  :config
  ;; disable tab key bind
  (define-key copilot-completion-map (kbd "<tab>") nil)
  (define-key copilot-completion-map (kbd "TAB") nil)
  (define-key copilot-completion-map (kbd "C-TAB") nil)
  (define-key copilot-completion-map (kbd "C-<tab>") nil)
  :bind
  (:map copilot-completion-map
        ("M-<return>" . 'copilot-accept-completion)
        ("C-<return>" . 'copilot-accept-completion-by-word)))

(use-package copilot-chat
  :ensure t
  :bind
  (:map global-map
        ("C-c C-y" . copilot-chat-yank)
        ("C-c M-y" . copilot-chat-yank-pop)
        ("C-c C-M-y" . (lambda() (interactive)
                         (copilot-chat-yank-pop -1)))))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Language Server Protocol                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Eglot
(use-package eglot
  :ensure t
  :after treesit
  :config
  ;; # npm install: https://nodejs.org/ja/download
  (add-to-list 'eglot-server-programs
               ;; npm install -g pyright
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
               ;; npm install -g basedpyright
               ;; '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))
               )
  (add-to-list 'eglot-server-programs
               ;; sudo apt install clangd
               '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               ;; npm install -g dockerfile-language-server-nodejs
               '((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; npm i -g vscode-langservers-extracted
               '((mhtml-mode html-mode html-ts-mode) . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; sudo snap install racket
               ;; raco pkg install racket-langserver
               '(racket-mode . ("racket" "-l" "racket-langserver")))
  :custom
  (eglot-ignored-server-capabilities
   ;; disable eglot symbol highlight to avoid conflict with highlight-symbol
   '(:documentHighlightProvider
     :inlayHintProvider))
  (eglot-send-changes-idle-time 2.0)
  ;; disable event logging
  (eglot-events-buffer-config '(:size 0 :format null))
  (read-process-output-max (* 4 1024 1024)) ;; 4MB
  :hook
  (((python-mode python-ts-mode) . eglot-ensure)
   ((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure)
   (racket-mode . eglot-ensure)
   ((dockerfile-mode dockerfile-ts-mode) . eglot-ensure)
   ((html-mode html-ts-mode) . eglot-ensure)))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Auto Completion                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package corfu
  :ensure t
  :hook
  (eglot-managed-mode . global-corfu-mode)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (defun ms:corfu-expand ()
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
        ("<tab>" . ms:corfu-expand)
        ("<backtab>" . corfu-reset)))
(use-package prescient
  :ensure t
  :custom
  (completion-category-overrides '((file (styles basic))))
  :config
  (prescient-persist-mode))
(use-package corfu-prescient
  :ensure t
  :after corfu
  :custom
  (completion-category-overrides '((file (styles basic))))
  (corfu-prescient-mode))
(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ flymake                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (add-hook 'prog-mode-hook 'flymake-mode) ; use flymake in program-mode
(use-package flymake
  :ensure t
  :hook
  (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 2.0))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tree-sitter                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; M-x treesit-install-language-grammar
(use-package treesit
  :hook (find-file . ms:treesit-setup)
  :functions (ms:maybe-install-treesit-grammar ms:treesit-setup)
  :init
  (defvar treesit-mode-pair-alist
    '((python-mode . python-ts-mode)
      (c-mode . c-ts-mode)
      (c++-mode . c++-ts-mode)
      (dockerfile-mode . dockerfile-ts-mode)
      (javascript-mode . js-ts-mode)
      (css-mode . css-ts-mode)
      (php-mode . php-ts-mode)
      (js-json-mode . json-ts-mode)
      (matlab-mode . matlab-ts-mode))
    "Alist mapping classic major modes to Tree-sitter-based modes.")
  (setopt treesit-language-source-alist
          '((python     "https://github.com/tree-sitter/tree-sitter-python")
            (c          "https://github.com/tree-sitter/tree-sitter-c")
            (c++        "https://github.com/tree-sitter/tree-sitter-cpp")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
            (js         "https://github.com/tree-sitter/node-tree-sitter")
            (css        "https://github.com/tree-sitter/tree-sitter-css")
            (php        "https://github.com/tree-sitter/tree-sitter-php")
            (json       "https://github.com/tree-sitter/tree-sitter-json")
            (matlab     "https://github.com/acristoffers/tree-sitter-matlab")
            (scheme     "https://github.com/6cdh/tree-sitter-scheme")))
  (dolist (treesit-mode-pair treesit-mode-pair-alist)
    (let ((lang (intern
                 (string-remove-suffix "-ts-mode"
                                       (symbol-name (cdr treesit-mode-pair))))))
      (when (treesit-language-available-p lang)
        (add-to-list 'major-mode-remap-alist treesit-mode-pair))))
  (defun ms:maybe-install-treesit-grammar (lang)
    "If the grammar for current major-mode exists in treesit-language-source-alist, install it."
    (when (and (assoc lang treesit-language-source-alist)
               (y-or-n-p (format
                          "Treesit grammar for '%s' not found.  Install? "
                          lang)))
      (treesit-install-language-grammar lang)))
  (defun ms:treesit-setup ()
    "Set up Tree-sitter grammars and remap major modes"
    (let ((treesit-mode-pair (assoc major-mode treesit-mode-pair-alist)))
      (when treesit-mode-pair
        (let ((lang (intern
                     (string-remove-suffix "-ts-mode"
                                           (symbol-name (cdr treesit-mode-pair))))))
          (ms:maybe-install-treesit-grammar lang)
          (when (treesit-language-available-p lang)
            (add-to-list 'major-mode-remap-alist treesit-mode-pair)
            (funcall (cdr treesit-mode-pair)))))))
  :custom
  (treesit-font-lock-level 4))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ edit mode                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; scheme
(use-package scheme
  :hook
  (scheme-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'exec-path (expand-file-name "~/.guix-profile/bin"))
    ;; git clone https://codeberg.org/rgherdt/scheme-lsp-server.git
    ;; cd scheme-lsp-server
    ;; sudo apt install guix
    ;; guix package -f guix.scm
    (add-to-list 'eglot-server-programs
                 '(scheme-mode . ("guile-lsp-server" "--stdio")))))
;; markdown
(use-package markdown-mode
  :ensure t
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-split-window-direction 'right)
  (markdown-live-preview-delete-export 'delete-on-export)
  :hook
  (gfm-mode . (lambda () (setopt truncate-lines t))))
;; cmake
(use-package cmake-mode
  :ensure t
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))
;; racket
(use-package racket-mode
  :ensure t
  :mode
  ("\\.rkt\\'"))
;; yaml
(use-package yaml-mode
  :ensure t
  :mode
  ("\\.yml\\'" "\\.yaml\\'"))
;; PHP
(use-package php-mode
  :ensure t
  :mode
  ("\\.php\\'"))
;; SSH config
(use-package ssh-config-mode
  :ensure t)
;; javascript
(use-package js
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2))
;; python
(use-package python-mode
  :ensure t
  :mode
  ("\\.py\\'")
  :hook
  ((python-mode . ms:update-doom-modeline-python-version)
   (python-ts-mode . ms:update-doom-modeline-python-version))
  :functions
  (ms:get-user-from-ssh-config
   ms:guess-user-from-buffer
   ms:find-venv-path
   pyright-env)
  :init
  ;; (defun ms:get-user-from-ssh-config (host)
  ;;   "Get the User for HOST from ~/.ssh/config ."
  ;;   (with-temp-buffer
  ;;     (insert-file-contents (expand-file-name "~/.ssh/config"))
  ;;     (ssh-config-mode)
  ;;     (let ((alist (ssh-config-parse-buffer)))
  ;;       (cdr (assoc 'User (assoc host alist))))))
  (defun ms:get-user-from-ssh-config (host)
    "Get the User for HOST from ~/.ssh/config safely."
    (with-temp-buffer
      (insert-file-contents (expand-file-name "~/.ssh/config"))
      (let ((case-fold-search t)
            (user nil)
            (blocks '()))
        ;; Separate file as Host unit
        (goto-char (point-min))
        (while (re-search-forward "^Host[ \t]+\\(.+\\)" nil t)
          (let ((start (line-beginning-position))
                (hosts (split-string (match-string 1)))
                (end (if (re-search-forward "^Host[ \t]+" nil t)
                         (line-beginning-position)
                       (point-max))))
            (push (list :hosts hosts :start start :end end) blocks)
            (goto-char end)))
        ;; Search each block
        (dolist (block blocks)
          (when (member host (plist-get block :hosts))
            (goto-char (plist-get block :start))
            (while (re-search-forward "^[ \t]*\\([^ \t\n]+\\)[ \t]+\\(.+\\)" (plist-get block :end) t)
              (when (string= (match-string 1) "User")
                (setq user (string-trim (match-string 2)))))))
        user)))
  (defun ms:guess-user-from-buffer ()
    "Guess the username from the buffer's TRAMP file path."
    (let ((file buffer-file-name))
      (cond
       ;; Local path with /home/USER
       ((and file (string-match "/home/\\([^/]+\\)/" file))
        (match-string 1 file))
       ;; Remote TRAMP path
       ((and file (file-remote-p file))
        (let ((host (tramp-file-name-host (tramp-dissect-file-name file))))
          (ms:get-user-from-ssh-config host))))))
  (defun ms:find-venv-path ()
    "Find venv path"
    (let* ((base-paths '(".anaconda"
                         ".anaconda3"
                         ".miniconda"
                         ".miniconda3"
                         ".miniforge3"
                         ".mambaforge"
                         "anaconda3"
                         "miniconda3"
                         "miniforge3"
                         "mambaforge"
                         "opt/miniconda3"
                         "/opt/miniconda3"
                         "/usr/bin/anaconda3"
                         "/usr/local/anaconda3"
                         "/usr/local/miniconda3"
                         "/usr/local/Caskroom/miniconda/base"
                         ".conda"))
           (remote (file-remote-p default-directory))
           (remote-user (when remote
                          (or (tramp-file-name-user
                               (tramp-dissect-file-name default-directory))
                              (ms:guess-user-from-buffer))))
           (candidates (mapcar (lambda (path)
                                 (let ((path-with-envs (format "%s/envs" path)))
                                   (if (string-prefix-p "/" path-with-envs)
                                       path-with-envs
                                     (if remote-user
                                         (format "/home/%s/%s" remote-user path-with-envs)
                                       (expand-file-name (format "~/%s" path-with-envs))))))
                               base-paths))
           (venv-path (seq-find (lambda (candidate)
                                  (let ((venv-path (if remote
                                                       (format "%s%s" remote candidate)
                                                     candidate)))
                                    (file-directory-p venv-path)))
                                candidates)))
      (if venv-path
          venv-path
        (user-error "Error: No Conda directory found"))))
  (require 'json)
  (defun pyright-env ()
    "Change conda env for pyright"
    ;; Set pyrightconfig.json based on local or remote Python env for the current buffer
    (interactive)
    (let* ((project (eglot--project (eglot-current-server)))
           (project-root (if project
                             (project-root project)
                           default-directory))
           (config-path (expand-file-name "pyrightconfig.json" project-root))
           (gitignore-path (expand-file-name ".gitignore" project-root))
           (remote (file-remote-p default-directory))
           (venv-path (ms:find-venv-path))
           (pyright-venv-path (if remote
                                  (format "%s%s" remote venv-path)
                                venv-path))
           (venv-list (directory-files pyright-venv-path nil "^[^.]"))
           (venv (completing-read "Choose Python environment: " venv-list))
           (config `(("venvPath" . ,venv-path)
                     ("venv" . ,venv)))
           (json-content (let ((json-object-type 'alist)
                               (json-array-type 'list)
                               (json-key-type 'string))
                           (json-encode config))))
      ;; Write
      (with-temp-file config-path
        (insert json-content))
      (message "Saved pyrightconfig.json with environment: %s (under %s/%s)"
               venv venv-path venv)
      ;; Recconect Eglot
      (eglot-reconnect (eglot-current-server))
      ;; Add pyrightconfig.json to .gitignore
      (when (file-writable-p gitignore-path)
        (with-temp-buffer
          (when (file-exists-p gitignore-path)
            (insert-file-contents gitignore-path))
          (goto-char (point-min))
          (unless (re-search-forward "^pyrightconfig\\.json$" nil t)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "pyrightconfig.json\n")
            (write-region (point-min) (point-max) gitignore-path)
            (message "Added pyrightconfig.json to %s" gitignore-path))))
      ;; Update doom-modeline
      (ms:update-doom-modeline-python-version)
      (revert-buffer t t)))
  (require 'project)
  (defun ms:update-doom-modeline-python-version ()
    "Set venv from pyrightconfig.json if available."
    (let* ((project (project-current))
           (project-root (if project
                             (project-root project)
                           default-directory))
           (config-path (expand-file-name "pyrightconfig.json" project-root))
           (remote (file-remote-p default-directory 'host)))
      (if (file-exists-p config-path)
          (with-temp-buffer
            (insert-file-contents config-path)
            (let* ((json-object-type 'hash-table)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (data (json-parse-buffer))
                   (venv-path (gethash "venvPath" data))
                   (venv-name (gethash "venv" data)))
              (when (and venv-path venv-name)
                (let* ((python-path (format "%s/%s/bin/python" venv-path venv-name))
                       (python-version-command
                        (if remote
                            (format "ssh %s %s --version | sed 's/$/(%s)/'" remote python-path venv-name)
                          (format "%s --version | sed 's/$/(%s)/'" python-path venv-name)))
                       (script-dir (expand-file-name ".cache/pyright-env" user-emacs-directory))
                       (script-path (expand-file-name (format "modeline-python-version-%s-%s.sh"
                                                              (or remote "local")
                                                              venv-name)
                                                      script-dir)))
                  (unless (file-exists-p script-dir)
                    (make-directory script-dir t))
                  (with-temp-file script-path
                    (insert "#!/bin/bash\n\n")
                    (insert (format "%s\n" python-version-command)))
                  (set-file-modes script-path #o755)
                  (setopt doom-modeline-env-python-executable script-path)))))
        (setopt doom-modeline-env-python-executable nil)))))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tramp                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package tramp
  ;; :functions (ms:tramp-reconnect)
  ;; :init
  ;; (defun ms:tramp-reconnect ()
  ;;   "Reconect tramp access"
  ;;   (interactive)
  ;;   (when (file-remote-p default-directory)
  ;;     (let ((current-file (buffer-file-name)))
  ;;       (when current-file
  ;;         (message "Reconnecting to remote file: %s" current-file)
  ;;         (find-alternate-file current-file)))))
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 1)
  (tramp-auto-save-directory "/tmp")
  (tramp-connection-timeout 30)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; :hook (tramp-cleanup-hook . (lambda ()
  ;;                               (message
  ;;                                "Tramp connection lost, trying to reconnect...")
  ;;                               (ms:tramp-reconnect)))
  )
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ project management                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package projectile
  :ensure t
  :config
  (projectile-mode))
(use-package treemacs
  :ensure t
  :defer t
  :bind (:map global-map
              ("C-x t t" . treemacs)
              ("C-x t a" . treemacs-add-and-display-current-project)
              ("C-x t e" . treemacs-add-and-display-current-project-exclusively))
  :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1))))
(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  :config
  (treemacs-projectile))
;; treemacs theme
(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:weight bold))))
  (git-gutter:added    ((t (:weight bold))))
  (git-gutter:deleted  ((t (:weight bold))))
  ;; (git-gutter:modified ((t (:background "#f1fa8c"))))
  ;; (git-gutter:added    ((t (:background "#50fa7b"))))
  ;; (git-gutter:deleted  ((t (:background "#ff79c6"))))
  )
(use-package nerd-icons-dired
  :ensure t
  :after (nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ GC Threshold                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setopt gc-cons-threshold (* 128 1024 1024)) ; 128Mb ; default (* 800 1024)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 32 1024 1024))))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tab line mode                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(global-tab-line-mode t)
;; (tab-bar-mode nil)
;; (global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-next-tab)
;; (global-set-key (kbd "C-S-<iso-lefttab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<tab>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-x t k") 'tab-line-close-tab)
(use-package tab-line-nerd-icons
  :ensure t
  :config
  (tab-line-nerd-icons-global-mode))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tool bar mode                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(tool-bar-mode -1) ;; disable tool bar
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
(add-hook 'prog-mode-hook (lambda () (setopt truncate-lines t)))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ undo tree                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package undo-tree
  :ensure t
  :init
  ;; if the directory no exist, then make it
  (defvar undo-tree-dir
    (expand-file-name "undo-tree-history" user-emacs-directory))
  (unless (file-exists-p undo-tree-dir)
    (make-directory undo-tree-dir t))
  :custom
  ;; Enable persistent history
  (undo-tree-auto-save-history t)
  ;; save history file in specified directory
  (undo-tree-history-directory-alist `(("." . ,undo-tree-dir)))
  (undo-tree-limit 1000)
  :config
  (global-undo-tree-mode)
  ;; Undo C-/, Redo C-S-/
  )
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ shortcut                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; C-TAB, C-Shift-TAB buffer switch
;; (global-set-key (kbd "C-<tab>") #'(lambda() (interactive) (bury-buffer)))
;; (global-set-key (kbd "C-S-<iso-lefttab>") #'(lambda() (interactive) (unbury-buffer)))
;; (global-set-key (kbd "C-<tab>") 'next-buffer)
;; (global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)

;; C-; comment out/in
;; (defun one-line-comment ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (set-mark (point))
;;     (end-of-line)
;;     (comment-or-uncomment-region (region-beginning) (region-end))))
;; (global-set-key (kbd "C-;") 'one-line-comment)

(defun ms:move-beginning-of-line ()
  "Toggle beginning of line and indent."
  (interactive)
  (let ((origin (point)))
    (back-to-indentation)
    (when (= origin (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'ms:move-beginning-of-line)

;; code jump history
;; M-,
(global-set-key (kbd "<mouse-8>") 'xref-go-back)
;; M-C-,
(global-set-key (kbd "<mouse-9>") 'xref-go-forward)

;; Backspace
(global-set-key (kbd "C-h") 'delete-backward-char)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tab space                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq-default indent-tabs-mode nil)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ multiple-cursors                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package multiple-cursors
  :ensure t
  :bind
  ;; C-u C-M-SPC
  (("C-S-c C-S-c" . mc/mark-all-dwim)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/unmark-next-like-this)))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Japanese input (for Japanese users)                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package mozc
  :ensure t
  :init
  (defun ime-on ()
    (interactive)
    (unless current-input-method
      (toggle-input-method)))
  (defun ime-off ()
    (interactive)
    (when current-input-method
      (toggle-input-method)))
  :custom
  (default-input-method "japanese-mozc")
  (mozc-leim-title "あ")
  :bind
  (;; 半角/全角キーで切り替え
   ([zenkaku-hankaku] . toggle-input-method)
   ;; 変換キーでmozcオン
   ([henkan] . ime-on)
   ;; 無変換キーでmozcオフ
   ([muhenkan] . ime-off)
   :map mozc-mode-map
   ([muhenkan] . ime-off))
  :hook
  ;; フォント
  (after-init . (lambda () (set-fontset-font t 'japanese-jisx0208 "Migu 1M"))))
  
(use-package mozc-popup
  :ensure t
  :after (mozc)
  :custom
  (mozc-candidate-style 'popup))
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
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file
      'noerror
      'nomessage)
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ custom theme                                                  ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(load (expand-file-name "themes/custom-theme.el" user-emacs-directory)
      'noerror
      'nomessage)
;; ;;; init.el ends here
