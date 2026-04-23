;;; init.el --- Cross-platform Emacs config for macOS and Ubuntu -*- lexical-binding: t; -*-

;; --------------------------------------------------
;; Basic OS detection
;; --------------------------------------------------
(defconst ms/os-mac-p (eq system-type 'darwin))
(defconst ms/os-linux-p (eq system-type 'gnu/linux))

;; --------------------------------------------------
;; Package.el + leaf bootstrap
;; --------------------------------------------------
(eval-and-compile
  (require 'package)

  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("org"   . "https://orgmode.org/elpa/")))

  (package-initialize)

  (unless (package-installed-p 'leaf)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'leaf))

  (require 'leaf)
  (require 'leaf-keywords)
  (leaf-keywords-init))

;; --------------------------------------------------
;; basic settings
;; --------------------------------------------------
(leaf emacs
  :preface
  (defconst ms/backup-dir
    (expand-file-name "var/backup/" user-emacs-directory))
  (defconst ms/auto-save-dir
    (expand-file-name "var/auto-save/" user-emacs-directory))
  (defun ms/truncate-on ()
    (setq-local truncate-lines t))
  (defun ms/move-beginning-of-line ()
    "Toggle between indentation and beginning of line."
    (interactive)
    (let ((origin (point)))
      (back-to-indentation)
      (when (= origin (point))
        (move-beginning-of-line 1))))
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (prog-mode-hook . ms/truncate-on)
  :bind
  (("C-<tab>" . tab-line-switch-to-next-tab)
   ("C-S-<iso-lefttab>" . tab-line-switch-to-prev-tab)
   ("C-x t k" . tab-line-close-tab)
   ("C-a" . ms/move-beginning-of-line))
  :custom
  (;; basic
   (use-short-answers . t)
   (ring-bell-function . 'ignore)
   ;; startup
   (inhibit-startup-message . t)
   ;; backup
   (make-backup-files . t)
   (version-control . t)
   (kept-new-versions . 3)
   (kept-old-versions . 0)
   (delete-old-versions . t)
   (auto-save-timeout . 10)
   (auto-save-interval . 100)
   ;; lockfile
   (create-lockfiles . nil)
   ;; scroll
   (scroll-preserve-screen-position . t)
   (mouse-wheel-tilt-scroll . t)
   (mouse-wheel-flip-direction . t)
   ;; line number
   (display-line-numbers-grow-only . t)
   ;; edit
   (indent-tabs-mode . nil)
   (tab-width . 4))
  :config
  ;; backup
  (make-directory ms/backup-dir t)
  (make-directory ms/auto-save-dir t)
  (setq backup-directory-alist `((".*" . ,ms/backup-dir))
        auto-save-file-name-transforms `((".*" ,ms/auto-save-dir t))
        auto-save-list-file-prefix
        (expand-file-name ".saves-" ms/auto-save-dir))
  ;; 
  (add-to-list 'default-frame-alist '(alpha . (90 . 85)))
  ;; scroll
  (pixel-scroll-precision-mode 1)
  ;; minibuffer
  (minibuffer-depth-indicate-mode 1)
  ;; tab
  (global-tab-line-mode 1)
  ;; auto paring
  (electric-pair-mode 1)
  ;; scheme language server path
  (add-to-list 'exec-path (expand-file-name "~/.guix-profile/bin"))
  :setq
  ;; basic
  `((read-process-output-max . ,(* 4 1024 1024))
    (gc-cons-threshold . ,(* 16 1024 1024))))

(leaf emacs
  :when ms/os-linux-p
  :custom
  ((redisplay-dont-pause . nil)
   (mouse-wheel-progressive-speed . nil)
   (scroll-margin . 0)
   (hscroll-margin . 0)
   (pixel-scroll-precision-interpolation-factor . 1.0))
  :bind
  (("<mouse-8>" . xref-go-back)
   ("<mouse-9>" . xref-go-forward))
  :hook
  (after-init-hook . (lambda ()
                       (set-fontset-font t 'japanese-jisx0208 "Migu 1M"))))

;; --------------------------------------------------
;; mode line
;; --------------------------------------------------
(leaf doom-modeline
  :ensure t
  :hook
  (after-init-hook . doom-modeline-mode))

;; --------------------------------------------------
;; theme
;; --------------------------------------------------
(leaf timu-macos-theme
  :ensure t
  :config
  (load-theme 'timu-macos t))

;; --------------------------------------------------
;; nerd-icons
;; --------------------------------------------------
(leaf nerd-icons
  :ensure t
  :config
  (defvar ms/icons-fonts-setup-done
    (expand-file-name ".fonts-setup-done" user-emacs-directory))

  (unless (file-exists-p ms/icons-fonts-setup-done)
    (when (fboundp 'nerd-icons-install-fonts)
      (nerd-icons-install-fonts t))

    ;; Linuxのみ fc-cache
    (when (and ms/os-linux-p
               (executable-find "fc-cache"))
      (call-process "fc-cache" nil "*Messages*" t "-f" "-v"))

    ;; フラグ作成
    (write-region "" nil ms/icons-fonts-setup-done)))

(leaf tab-line-nerd-icons
  :ensure t
  :config
  (tab-line-nerd-icons-global-mode 1))

(leaf treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-nerd-icons-config))

(leaf nerd-icons-dired
  :ensure t
  :after (nerd-icons)
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

;; --------------------------------------------------
;; search / navigation
;; --------------------------------------------------
(leaf isearch
  :bind
  (:isearch-mode-map
   ("C-d" . isearch-delete-char)
   ("C-e" . isearch-edit-string)
   ("TAB" . isearch-yank-word)))

(leaf consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("M-g g" . consult-goto-line)
   ("M-y" . consult-yank-pop)
   ("M-s d" . consult-find)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi))
  :bind
  (:isearch-mode-map
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)))

;; --------------------------------------------------
;; minibuffer
;; --------------------------------------------------
(leaf savehist
  :custom
  ((savehist-additional-variables . '(search-ring regexp-search-ring)))
  :config
  (savehist-mode 1))

;; --------------------------------------------------
;; actions
;; --------------------------------------------------
(leaf embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)))

(leaf embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;; --------------------------------------------------
;; completion UI
;; --------------------------------------------------
(leaf nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode 1))

(leaf vertico
  :ensure t
  :init
  (vertico-mode))

(leaf marginalia
  :ensure t
  :init
  (marginalia-mode))

(leaf orderless
  :ensure t
  :custom
  ((completion-styles . '(orderless basic))
   (completion-category-defaults . nil)
   (completion-category-overrides
    . '((file (styles basic partial-completion))))))

(leaf corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  :preface
  (defun ms/corfu-expand ()
    (interactive)
    (unless (corfu-expand)
      (corfu-complete)))
  :custom
  ((corfu-cycle . t)
   (corfu-auto . t)
   (corfu-auto-delay . 0.0)
   (corfu-auto-prefix . 1))
  :bind
  (:corfu-map
   ("RET" . corfu-complete)
   ("<return>" . corfu-complete)
   ("<tab>" . ms/corfu-expand)
   ("<backtab>" . corfu-reset)))

(leaf nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; --------------------------------------------------
;; session
;; --------------------------------------------------
(leaf saveplace
  :config
  (save-place-mode 1))

;; --------------------------------------------------
;; symbol highlight
;; --------------------------------------------------
(leaf highlight-symbol
  :ensure t
  :custom
  ((highlight-symbol-idle-delay . 0.3))
  :bind
  (("M-n" . highlight-symbol-next)
   ("M-p" . highlight-symbol-prev))
  :hook
  (prog-mode-hook . highlight-symbol-mode))

;; --------------------------------------------------
;; rainbow
;; --------------------------------------------------
(leaf rainbow-mode
  :ensure t
  :preface
  (defvar ms/rainbow-hex-only-p nil
    "If non-nil, highlight only hexadecimal color codes in rainbow-mode.")
  (defun ms/rainbow-mode-toggle-hex-only ()
    "Toggle rainbow-mode between hex-only and all-color highlighting."
    (interactive)
    (setq ms/rainbow-hex-only-p (not ms/rainbow-hex-only-p))
    (if ms/rainbow-hex-only-p
        (progn
          (setopt rainbow-html-colors nil)
          (setopt rainbow-x-colors nil)
          (setopt rainbow-hexadecimal-colors t)
          (message "Rainbow mode: Highlighting only hexadecimal colors"))
      (progn
        (setopt rainbow-html-colors t)
        (setopt rainbow-x-colors t)
        (setopt rainbow-hexadecimal-colors t)
        (message "Rainbow mode: All colors highlighted")))
    (when (bound-and-true-p rainbow-mode)
      (rainbow-mode -1)
      (rainbow-mode 1))))

;; --------------------------------------------------
;; github copilot
;; --------------------------------------------------
(leaf copilot
  :ensure t
  :preface
  (defun ms/copilot-server-installed-p ()
    "Return non-nil if Copilot language server seems available."
    (or (executable-find "copilot-language-server")
        (ignore-errors
          (when (fboundp 'copilot-server-executable)
            (let ((exe (copilot-server-executable)))
              (and exe (file-exists-p exe)))))))

  (defun ms/copilot-mode-enable ()
    "Enable `copilot-mode', installing the server first if needed."
    (interactive)
    (cond
     ((ms/copilot-server-installed-p)
      (copilot-mode 1))
     ((not (fboundp 'copilot-install-server))
      (user-error "copilot-install-server is not available"))
     ((y-or-n-p "Copilot language server is missing. Install it now? ")
      (call-interactively #'copilot-install-server)
      (message "Run `M-x copilot-login` after installation if needed."))
     (t
      (message "Copilot was not enabled"))))

  (defun ms/copilot-mode-toggle ()
    "Toggle Copilot mode safely."
    (interactive)
    (if (bound-and-true-p copilot-mode)
        (copilot-mode -1)
      (ms/copilot-mode-enable)))

  :bind
  (("C-c C-o" . ms/copilot-mode-toggle))
  :config
  ;; Keep TAB free
  (define-key copilot-completion-map (kbd "<tab>") nil)
  (define-key copilot-completion-map (kbd "TAB") nil)
  (define-key copilot-completion-map (kbd "C-TAB") nil)
  (define-key copilot-completion-map (kbd "C-<tab>") nil)

  ;; Accept keys
  (define-key copilot-completion-map (kbd "M-<return>")
              #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-<return>")
              #'copilot-accept-completion-by-word))

;; --------------------------------------------------
;; Language Server Protocol
;; --------------------------------------------------
(leaf eglot
  :ensure t
  :preface
  (defconst ms/nodejs-download-url
    "https://nodejs.org/ja/download")

  (defun ms/eglot-server-exists-p (command)
    (and command (executable-find command)))

  (defun ms/npm-exists-p ()
    (executable-find "npm"))

  (defun ms/open-nodejs-download-page ()
    (interactive)
    (browse-url ms/nodejs-download-url))

  (defun ms/eglot-run-install-command (install-command)
    (async-shell-command install-command "*ms-eglot-install*"))

  (defun ms/eglot-ensure-with-prompt (server-command install-command install-message)
    (cond
     ((ms/eglot-server-exists-p server-command)
      (eglot-ensure))
     ((and install-command
           (string-match-p "\\`npm\\b" install-command)
           (not (ms/npm-exists-p)))
      (when (y-or-n-p
             (format "npm is not installed. Open the Node.js download page for %s? "
                     server-command))
        (ms/open-nodejs-download-page)
        (message "Install Node.js and npm first, then reopen the buffer or run M-x eglot")))
     ((and install-command
           (y-or-n-p (format "%s is not installed. Install it now? " server-command)))
      (message "%s" install-message)
      (ms/eglot-run-install-command install-command))
     ((not install-command)
      (message "%s is not installed." server-command))
     (t
      (message "Skipped installing %s" server-command))))

  (defun ms/eglot-scheme ()
    (interactive)
    (ms/eglot-ensure-with-prompt
     "guile-lsp-server"
     nil
     "Install guile-lsp-server manually if needed."))

  (defun ms/eglot-python ()
    (interactive)
    (ms/eglot-ensure-with-prompt
     "pyright-langserver"
     "npm install -g pyright"
     "Installing pyright..."))

  (defun ms/eglot-c/c++ ()
    (interactive)
    (ms/eglot-ensure-with-prompt
     "clangd"
     nil
     "clangd is not installed. Please install it with your system package manager."))

  (defun ms/eglot-dockerfile ()
    (interactive)
    (ms/eglot-ensure-with-prompt
     "docker-langserver"
     "npm install -g dockerfile-language-server-nodejs"
     "Installing Dockerfile language server..."))

  (defun ms/eglot-html ()
    (interactive)
    (ms/eglot-ensure-with-prompt
     "vscode-html-language-server"
     "npm i -g vscode-langservers-extracted"
     "Installing HTML language server..."))

  (defun ms/eglot-racket ()
    (interactive)
    (ms/eglot-ensure-with-prompt
     "racket"
     nil
     "Install racket and racket-langserver manually if needed."))

  :setq
  `((eglot-ignored-server-capabilities
     . '(:documentHighlightProvider :inlayHintProvider))
    (eglot-send-changes-idle-time . 0.3))

  :hook
  (
   (c-mode-hook . ms/eglot-c/c++)
   (c++-mode-hook . ms/eglot-c/c++)
   (c-ts-mode-hook . ms/eglot-c/c++)
   (c++-ts-mode-hook . ms/eglot-c/c++)
   (dockerfile-mode-hook . ms/eglot-dockerfile)
   (dockerfile-ts-mode-hook . ms/eglot-dockerfile)
   (mhtml-mode-hook . ms/eglot-html)
   (html-mode-hook . ms/eglot-html)
   (html-ts-mode-hook . ms/eglot-html)
   (racket-mode-hook . ms/eglot-racket))

  :config
  (add-to-list 'eglot-server-programs
               '(scheme-mode . ("guile-lsp-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode)
                 . ("clangd")))
  (add-to-list 'eglot-server-programs
               '((dockerfile-mode dockerfile-ts-mode)
                 . ("docker-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((mhtml-mode html-mode html-ts-mode)
                 . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(racket-mode . ("racket" "-l" "racket-langserver"))))

;; --------------------------------------------------
;; diagnostics
;; --------------------------------------------------
(leaf flymake
  :hook
  (prog-mode-hook . flymake-mode)
  :setq
  ((flymake-no-changes-timeout . 0.3)))

;; --------------------------------------------------
;; syntax
;; --------------------------------------------------
(leaf treesit
  :preface
  (defvar ms/treesit-mode-pair-alist
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

  (defun ms/treesit-lang-for-mode (mode)
    (pcase mode
      ('python-mode 'python)
      ('c-mode 'c)
      ('c++-mode 'c++)
      ('dockerfile-mode 'dockerfile)
      ('javascript-mode 'js)
      ('css-mode 'css)
      ('php-mode 'php)
      ('js-json-mode 'json)
      ('matlab-mode 'matlab)
      (_ nil)))

  (defun ms/treesit-maybe-install-grammar ()
    (let ((lang (ms/treesit-lang-for-mode major-mode)))
      (when (and lang
                 (not (treesit-language-available-p lang))
                 (assoc lang treesit-language-source-alist)
                 (y-or-n-p (format "Treesit grammar for %s not found. Install it? " lang)))
        (treesit-install-language-grammar lang))))

  :setq
  (treesit-font-lock-level . 4)
  :config
  (setopt treesit-language-source-alist
          '((python     "https://github.com/tree-sitter/tree-sitter-python")
            (c          "https://github.com/tree-sitter/tree-sitter-c")
            (c++        "https://github.com/tree-sitter/tree-sitter-cpp")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
            (css        "https://github.com/tree-sitter/tree-sitter-css")
            (php        "https://github.com/tree-sitter/tree-sitter-php")
            (json       "https://github.com/tree-sitter/tree-sitter-json")
            (matlab     "https://github.com/acristoffers/tree-sitter-matlab")
            (scheme     "https://github.com/6cdh/tree-sitter-scheme")))

  (dolist (pair ms/treesit-mode-pair-alist)
    (let ((lang (ms/treesit-lang-for-mode (car pair))))
      (when (and lang (treesit-language-available-p lang))
        (add-to-list 'major-mode-remap-alist pair))))

  (add-hook 'find-file-hook #'ms/treesit-maybe-install-grammar))


;; --------------------------------------------------
;; remote
;; --------------------------------------------------
(leaf tramp
  :require t
  :custom
  ((tramp-default-method . "ssh")
   (tramp-verbose . 1)
   (tramp-auto-save-directory . temporary-file-directory)
   (tramp-connection-timeout . 30))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; --------------------------------------------------
;; edit mode
;; --------------------------------------------------
(leaf scheme
  :hook
  (scheme-mode-hook . ms/eglot-scheme))

(leaf markdown-mode
  :ensure t
  :preface
  (defun ms/gfm-mode-setup ()
    (setq-local truncate-lines t))
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :custom
  ((markdown-split-window-direction . 'right)
   (markdown-live-preview-delete-export . 'delete-on-export))
  :hook
  (gfm-mode-hook . ms/gfm-mode-setup))

(leaf cmake-mode
  :ensure t
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

(leaf racket-mode
  :ensure t
  :mode
  ("\\.rkt\\'" . racket-mode))

(leaf yaml-mode
  :ensure t
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode)))

(leaf php-mode
  :ensure t
  :mode
  ("\\.php\\'" . php-mode))

(leaf ssh-config-mode
  :ensure t)

(leaf js
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  ((js-indent-level . 2)))

(leaf makefile
  :hook
  (makefile-mode-hook . (lambda ()
                         (setq-local indent-tabs-mode t))))

(leaf python
  :mode
  ("\\.py\\'" . python-mode)
  :hook
  ((python-mode-hook . ms/update-doom-modeline-python-version)
   (python-ts-mode-hook . ms/update-doom-modeline-python-version)
   (python-mode-hook . ms/eglot-python)
   (python-ts-mode-hook . ms/eglot-python))
  :preface
  (require 'json)
  (require 'project)
  (require 'tramp)
  (require 'subr-x)
  (require 'seq)

  (defun ms/get-user-from-ssh-config (host)
    "Get the User for HOST from ~/.ssh/config safely."
    (with-temp-buffer
      (insert-file-contents (expand-file-name "~/.ssh/config"))
      (let ((case-fold-search t)
            (user nil)
            (blocks '()))
        (goto-char (point-min))
        (while (re-search-forward "^Host[ \t]+\\(.+\\)" nil t)
          (let ((start (line-beginning-position))
                (hosts (split-string (match-string 1)))
                (end (if (re-search-forward "^Host[ \t]+" nil t)
                         (line-beginning-position)
                       (point-max))))
            (push (list :hosts hosts :start start :end end) blocks)
            (goto-char end)))
        (dolist (block blocks)
          (when (member host (plist-get block :hosts))
            (goto-char (plist-get block :start))
            (while (re-search-forward
                    "^[ \t]*\\([^ \t\n]+\\)[ \t]+\\(.+\\)"
                    (plist-get block :end) t)
              (when (string= (match-string 1) "User")
                (setq user (string-trim (match-string 2)))))))
        user)))

  (defun ms/guess-user-from-buffer ()
    "Guess the username from the current buffer path."
    (let ((file buffer-file-name))
      (cond
       ((and file (string-match "/home/\\([^/]+\\)/" file))
        (match-string 1 file))
       ((and file (file-remote-p file))
        (let ((host (tramp-file-name-host
                     (tramp-dissect-file-name file))))
          (ms/get-user-from-ssh-config host))))))

  (defun ms/find-conda-envs-root ()
    "Find Conda/Mamba envs directory for local or remote current buffer."
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
           (remote-prefix (file-remote-p default-directory))
           (remote-user (when remote-prefix
                          (or (tramp-file-name-user
                               (tramp-dissect-file-name default-directory))
                              (ms/guess-user-from-buffer))))
           (candidates
            (mapcar
             (lambda (path)
               (let ((path-with-envs (format "%s/envs" path)))
                 (if (string-prefix-p "/" path-with-envs)
                     path-with-envs
                   (if remote-user
                       (format "/home/%s/%s" remote-user path-with-envs)
                     (expand-file-name (format "~/%s" path-with-envs))))))
             base-paths)))
      (or (seq-find
           (lambda (candidate)
             (file-directory-p
              (if remote-prefix
                  (format "%s%s" remote-prefix candidate)
                candidate)))
           candidates)
          (user-error "No Conda/Mamba envs directory found"))))

  (defun ms/python-project-root ()
    "Return project root or current directory."
    (let ((project (project-current)))
      (if project
          (project-root project)
        default-directory)))

  (defun ms/python-pyright-config-path ()
    "Return pyrightconfig.json path for current project."
    (expand-file-name "pyrightconfig.json" (ms/python-project-root)))

  (defun ms/python-current-eglot-server ()
    "Return current Eglot server or nil."
    (ignore-errors
      (when (fboundp 'eglot-current-server)
        (eglot-current-server))))

  (defun ms/python-reconnect-eglot-if-needed ()
    "Reconnect Eglot if active."
    (let ((server (ms/python-current-eglot-server)))
      (when server
        (eglot-reconnect server))))

  (defun ms/python-read-env-config ()
    "Read pyrightconfig.json and return hash table or nil."
    (let ((config-path (ms/python-pyright-config-path)))
      (when (file-exists-p config-path)
        (with-temp-buffer
          (insert-file-contents config-path)
          (let ((json-object-type 'hash-table)
                (json-array-type 'list)
                (json-key-type 'string))
            (json-parse-buffer))))))

  (defun ms/update-doom-modeline-python-version ()
    "Update doom-modeline Python executable from pyrightconfig.json."
    (let* ((data (ms/python-read-env-config))
           (remote-host (file-remote-p default-directory 'host)))
      (if data
          (let* ((venv-path (gethash "venvPath" data))
                 (venv-name (gethash "venv" data)))
            (if (and venv-path venv-name)
                (let* ((python-path (format "%s/%s/bin/python" venv-path venv-name))
                       (quoted-python (shell-quote-argument python-path))
                       (quoted-venv (shell-quote-argument venv-name))
                       (python-version-command
                        (if remote-host
                            (format "ssh %s %s --version | sed 's/$/(%s)/'"
                                    (shell-quote-argument remote-host)
                                    quoted-python
                                    quoted-venv)
                          (format "%s --version | sed 's/$/(%s)/'"
                                  quoted-python
                                  quoted-venv)))
                       (script-dir (expand-file-name ".cache/pyright-env"
                                                     user-emacs-directory))
                       (script-path (expand-file-name
                                     (format "modeline-python-version-%s-%s.sh"
                                             (or remote-host "local")
                                             venv-name)
                                     script-dir)))
                  (make-directory script-dir t)
                  (with-temp-file script-path
                    (insert "#!/bin/bash\n\n")
                    (insert python-version-command "\n"))
                  (set-file-modes script-path #o755)
                  (setq doom-modeline-env-python-executable script-path))
              (setq doom-modeline-env-python-executable nil)))
        (setq doom-modeline-env-python-executable nil))
      (force-mode-line-update t)))

  (defun ms/python-write-pyright-config (venv-path venv-name)
    "Write pyrightconfig.json using VENV-PATH and VENV-NAME."
    (let* ((config-path (ms/python-pyright-config-path))
           (config `(("venvPath" . ,venv-path)
                     ("venv" . ,venv-name)
                     ("diagnosticMode" . "openFilesOnly")
                     ("typeCheckingMode" . "basic")
                     ("autoSearchPaths" . :json-false)))
           (json-content (let ((json-object-type 'alist)
                               (json-array-type 'list)
                               (json-key-type 'string))
                           (json-encode config))))
      (with-temp-file config-path
        (insert json-content)
        (json-pretty-print (point-min) (point-max)))
      config-path))

  (defun ms/python-add-pyrightconfig-to-gitignore ()
    "Add pyrightconfig.json to .gitignore if appropriate."
    (let ((gitignore-path (expand-file-name ".gitignore" (ms/python-project-root))))
      (when (file-writable-p gitignore-path)
        (with-temp-buffer
          (when (file-exists-p gitignore-path)
            (insert-file-contents gitignore-path))
          (goto-char (point-min))
          (unless (re-search-forward "^pyrightconfig\\.json$" nil t)
            (goto-char (point-max))
            (unless (bolp)
              (insert "\n"))
            (insert "pyrightconfig.json\n")
            (write-region (point-min) (point-max) gitignore-path))))))

  (defun ms/pyright-env ()
    "Choose Conda/Mamba environment and reflect it in Pyright and modeline."
    (interactive)
    (let* ((venv-path (ms/find-conda-envs-root))
           (remote-prefix (file-remote-p default-directory))
           (envs-root (if remote-prefix
                          (format "%s%s" remote-prefix venv-path)
                        venv-path))
           (venv-list (seq-filter
                       (lambda (name)
                         (file-directory-p (expand-file-name name envs-root)))
                       (directory-files envs-root nil "^[^.]")))
           (venv (completing-read "Choose Python environment: " venv-list nil t)))
      (ms/python-write-pyright-config venv-path venv)
      (ms/python-add-pyrightconfig-to-gitignore)
      (ms/update-doom-modeline-python-version)
      (ms/python-reconnect-eglot-if-needed)
      (revert-buffer t t)
      (message "Updated pyrightconfig.json for %s/%s" venv-path venv))))

;; --------------------------------------------------
;; workspace
;; --------------------------------------------------
(leaf treemacs
  :ensure t
  :bind
  (("C-x t t" . treemacs)
   ("C-x t a" . treemacs-add-and-display-current-project)
   ("C-x t e" . treemacs-add-and-display-current-project-exclusively))
  :hook
  (treemacs-mode-hook . (lambda ()
                         (display-line-numbers-mode -1))))
(leaf diff-hl
  :ensure t
  :hook
  ((prog-mode-hook . diff-hl-mode)
   (vc-dir-mode-hook . diff-hl-mode))
  ;; :config
  ;; (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
)

;; --------------------------------------------------
;; editing
;; --------------------------------------------------
(leaf multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/mark-all-dwim)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/unmark-next-like-this)))

;; --------------------------------------------------
;; ui
;; --------------------------------------------------
(leaf which-key
  :ensure t
  :hook
  (after-init-hook . which-key-mode))

;; --------------------------------------------------
;; Japanese input
;; --------------------------------------------------
(leaf mozc
  :ensure t
  :when ms/os-linux-p
  :preface
  (defun ms/ime-on ()
    (interactive)
    (unless current-input-method
      (toggle-input-method)))

  (defun ms/ime-off ()
    (interactive)
    (when current-input-method
      (toggle-input-method)))
  :custom
  ((default-input-method . "japanese-mozc")
   (mozc-leim-title . "あ"))
  :bind
  (([zenkaku-hankaku] . toggle-input-method)
   ([henkan] . ms/ime-on)
   ([muhenkan] . ms/ime-off)))

;; --------------------------------------------------
;; custom file load                                              ;;;
;; --------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file
      'noerror
      'nomessage)

;; --------------------------------------------------
;; custom theme
;; --------------------------------------------------
(load (expand-file-name "themes/custom-theme.el" user-emacs-directory)
      'noerror
      'nomessage)
