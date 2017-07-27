;;; package --- Summary
;;; Commentary:
;;; Code:
;;----------------------------------------------------------------------------
;; require package
;;----------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")))

;;----------------------------------------------------------------------------
;; debugging message limit to 10000
;;----------------------------------------------------------------------------
(setq message-log-max 10000)

;;----------------------------------------------------------------------------
;; no autosave, backup files
;;----------------------------------------------------------------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;;----------------------------------------------------------------------------
;; put underline below the font bottom line
;;----------------------------------------------------------------------------
(setq x-underline-at-descent-line t)

;;----------------------------------------------------------------------------
;; keep window point when switching buffers this works across frames
;;----------------------------------------------------------------------------
(setq switch-to-buffer-preserve-window-point t)

;;----------------------------------------------------------------------------
;; Prompt for buffer switch in strongly dedicated windows
;;----------------------------------------------------------------------------
(setq switch-to-buffer-in-dedicated-window 'prompt)

;;----------------------------------------------------------------------------
;; do not save duplicates in history and kill ring
;;----------------------------------------------------------------------------
(setq history-delete-duplicates t)
(setq kill-do-not-save-duplicates t)

;;----------------------------------------------------------------------------
;; Package management
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;----------------------------------------------------------------------------
(package-initialize)

;;----------------------------------------------------------------------------
;; list the packages you want to install/load for now use-package is all I need
;;----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;----------------------------------------------------------------------------
;; don't load outdated byte code
;;----------------------------------------------------------------------------
(setq load-prefer-newer t)

;;----------------------------------------------------------------------------
;; go download any missing packages
;;----------------------------------------------------------------------------
(setq use-package-always-ensure t)

;;----------------------------------------------------------------------------
;; set path on mac
;;----------------------------------------------------------------------------
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;----------------------------------------------------------------------------
;; suppress some GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;;----------------------------------------------------------------------------
;; disable toolbars
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; ----------------------------------------------------------------------------
;; some basic preferences
;; ----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file "cache/bookmarks-items")
 bookmark-save-flag 1
 buffers-menu-max-size 30
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 scroll-preserve-screen-position 'always
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

;;----------------------------------------------------------------------------
;; show file path in frame title
;;----------------------------------------------------------------------------
(setq-default frame-title-format
              '(:eval
                (format "%s  %s"
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "« " (abbreviate-file-name default-directory) " »"))
                         (dired-directory
                          (concat "« " dired-directory " »"))
                         (t
                          "« no file »")))))

;; ▓

;;----------------------------------------------------------------------------
;; don't use ls command for dired mode
;;----------------------------------------------------------------------------
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))


;;----------------------------------------------------------------------------
;; set default directory for cache
;;----------------------------------------------------------------------------
(make-directory (locate-user-emacs-file "cache") t)

;;----------------------------------------------------------------------------
;; move point all the way when scrolling to buffer boundaries
;;----------------------------------------------------------------------------
(setq scroll-error-top-bottom t)

;;----------------------------------------------------------------------------
;; set regular font and unicode characters needs unicode font
;;----------------------------------------------------------------------------
(set-fontset-font "fontset-default" 'unicode "Fira Code")
(setq default-frame-alist '((font . "Fira Code-13")))

;;----------------------------------------------------------------------------
;; ligature support for fira code
;;----------------------------------------------------------------------------
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))

  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;;----------------------------------------------------------------------------
;; reopen desktop with same size as last closed session
;;----------------------------------------------------------------------------
(defvar desktop-base-file-name (locate-user-emacs-file "cache/emacs-desktop"))
(desktop-save-mode 1)

;;----------------------------------------------------------------------------
;; simple visible bell which works in all terminal types
;;----------------------------------------------------------------------------
(defun flash-mode-line ()
  "Flash modeline on bad commands."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))

(setq-default
 ring-bell-function 'flash-mode-line)

;;----------------------------------------------------------------------------
;; set option as super key and command as meta key
;;----------------------------------------------------------------------------
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;;----------------------------------------------------------------------------
;; enable all disabled commands
;;----------------------------------------------------------------------------
(setq disabled-command-function nil)

;;----------------------------------------------------------------------------
;; indicate minibuffer recursion depth
;;----------------------------------------------------------------------------
(minibuffer-depth-indicate-mode)

;;----------------------------------------------------------------------------
;; set indent/tab width to 2 spaces
;;----------------------------------------------------------------------------
(defvar css-indent-offset 2)
(defvar typescript-indent-level 2)
(defvar js-indent-level 2)

;;----------------------------------------------------------------------------
;; cleanup whitespace before saving a file
;;----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)

;;----------------------------------------------------------------------------
;; draw block cursor as wide as the glyph under it
;;----------------------------------------------------------------------------
(setq x-stretch-cursor t)

;;----------------------------------------------------------------------------
;; remove vc-mode from modeline
;; magit is powerful and I don't need branch name in mode line
;; https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html
;;----------------------------------------------------------------------------
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                ;; (vc-mode vc-mode)
                "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

;;----------------------------------------------------------------------------
;; Use Ibuffer for Buffer List
;;----------------------------------------------------------------------------
(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;;----------------------------------------------------------------------------
;; use conf-unix-mode for default dot files
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.npmrc\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\macos\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-unix-mode))

;;----------------------------------------------------------------------------
;; treat all themes as safe
;;----------------------------------------------------------------------------
(setq custom-safe-themes t)

;;----------------------------------------------------------------------------
;; rename both buffer and file name
;;----------------------------------------------------------------------------
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(global-set-key (kbd "C-c n r") 'rename-this-buffer-and-file)

;;----------------------------------------------------------------------------
;; mouse yank at point instead of click
;;----------------------------------------------------------------------------
(setq mouse-yank-at-point t)

;;----------------------------------------------------------------------------
;; increase maximum size of the mark ring
;;----------------------------------------------------------------------------
(setq mark-ring-max 30)

;;----------------------------------------------------------------------------
;; Repeat mark popping
;;----------------------------------------------------------------------------
(setq set-mark-command-repeat-pop t)

;;----------------------------------------------------------------------------
;; ansi-term and bash settings
;;----------------------------------------------------------------------------
(defvar my-term-shell "/bin/bash")

(defadvice ansi-term (before force-bash)
  "Open a bash shell by default."
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defun set-no-process-query-on-exit ()
  "Close ansi term without process running confirmation."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

;;----------------------------------------------------------------------------
;; use spaces instead of tabs and set default tab width
;;----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;----------------------------------------------------------------------------
;; Show column number and buffer size on the mode line
;;----------------------------------------------------------------------------
(column-number-mode)
(size-indication-mode)

;;----------------------------------------------------------------------------
;; Show unfinished keystrokes early
;;----------------------------------------------------------------------------
(setq echo-keystrokes 0.01)

;;----------------------------------------------------------------------------
;; Indicate buffer boundaries and empty lines
;;----------------------------------------------------------------------------
;; (setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)

;;----------------------------------------------------------------------------
;; Don't use dialogs for minibuffer input
;;----------------------------------------------------------------------------
(setq use-dialog-box nil)

;;----------------------------------------------------------------------------
;; Use spaces instead of tabs and set default tab width
;;----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;----------------------------------------------------------------------------
;; ignore case on completion
;;----------------------------------------------------------------------------
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;;----------------------------------------------------------------------------
;; emacs initial scratch buffer message
;;----------------------------------------------------------------------------
(setq-default initial-scratch-message "")

;;----------------------------------------------------------------------------
;; set scratch buffer to js-mode and never kill it
;;----------------------------------------------------------------------------
(setq initial-major-mode 'js-mode)

(defun unkillable-scratch-buffer ()
  "Don't kill scratch buffer."
  (if (or (equal (buffer-name (current-buffer)) "*scratch*") (equal (buffer-name (current-buffer)) "*Messages*"))
      (progn nil) t))

(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;;----------------------------------------------------------------------------
;; Forces the messages to 0, and kills the *Messages* buffer
;; Keep the messages buffer to see helpful logs
;;----------------------------------------------------------------------------
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;;----------------------------------------------------------------------------
;; maximize emacs window on load
;;----------------------------------------------------------------------------
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;----------------------------------------------------------------------------
;; kill all other open buffers except the current one
;; add C-u to kill special buffers too
;;----------------------------------------------------------------------------
(defun kill-other-buffers (&optional *special-buffers)
  "Kill all buffers but the current one.
*SPECIAL-BUFFERS is optional 'universal-arugment'
Don't mess with special buffers."
  (interactive "P")
  (if *special-buffers
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
  (progn
    (dolist (buffer (buffer-list))
      (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
        (kill-buffer buffer)))
    (message "Closed all other buffers")))

(global-set-key (kbd "C-c k") 'kill-other-buffers)

;;----------------------------------------------------------------------------
;; copy file or directory path from current buffer to clipboard
;;----------------------------------------------------------------------------
(defun surya-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
*DIR-PATH-ONLY-P is optional 'universal-argument' invoked using `C-u'
If `universal-argument' is called first, copy only the dir path"
  (interactive "P")
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (buffer-file-name)
               (buffer-file-name)
             (user-error "Current buffer is not associated with a file")))))
    (kill-new
     (if *dir-path-only-p
         (progn
           (message "Directory path copied: %s" (file-name-directory -fpath))
           (file-name-directory -fpath))
       (progn
         (message "File path copied: %s" -fpath)
         -fpath )))))

(global-set-key (kbd "C-c c f") 'surya-copy-file-path)

(defun surya-copy-directory-path ()
  "Copy the current buffer's dired path to `kill-ring'.
Result is full path."
  (interactive)
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (buffer-file-name)
               (buffer-file-name)
             (user-error "Current buffer is not associated with a file")))))
    (progn
      (kill-new
       (file-name-directory -fpath))
      (message "Directory path copied: %s" (file-name-directory -fpath)))))

(global-set-key (kbd "C-c c d") 'surya-copy-directory-path)

;;----------------------------------------------------------------------------
;; copy git root path to clipboard
;;----------------------------------------------------------------------------
(defun git-root-dir ()
  "Return the current directory's root Git repo directory."
  (let ((dir (locate-dominating-file default-directory ".git")))
    (when dir
      (file-name-directory dir))))

(defun git-root-path ()
  "Copy current git repo root file path.
If not in a Git repo, uses the current directory."
  (interactive)
  (if (git-root-dir)
      (progn
        (kill-new (git-root-dir))
        (message "GIT root path copied:%s" (git-root-dir)))
    (progn
      (kill-new default-directory)
      (message "File not in GIT repo, copied default path:%s" default-directory))))

(global-set-key (kbd "C-c c v") 'git-root-path)

;;----------------------------------------------------------------------------
;; make searches case sensitive
;; make dabbrev completion case sensitive
;;----------------------------------------------------------------------------
(defvar dabbrev-case-fold-search nil)


;;----------------------------------------------------------------------------
;; Replace dabbrev-expand with hippie-expand
;;----------------------------------------------------------------------------
(bind-key [remap dabbrev-expand] #'hippie-expand)

;;----------------------------------------------------------------------------
;; zap *up* to char is a handy pair for zap-to-char
;; zop-to-char for visual representation of content before zapping
;;----------------------------------------------------------------------------
(use-package zop-to-char
  :ensure t
  :bind (([remap zap-to-char] . zop-to-char)
         ("M-Z" . zop-up-to-char)))

;;----------------------------------------------------------------------------
;; fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;;----------------------------------------------------------------------------
;; set M-` to toggle all open EMACS windows
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-`") 'ns-next-frame)

;;----------------------------------------------------------------------------
;; don't open a new frame when emacs is already open
;;----------------------------------------------------------------------------
(setq ns-pop-up-frames nil)

;;----------------------------------------------------------------------------
;; launch terminal at the git root or at the current file location
;;----------------------------------------------------------------------------
(defun surya/open-Terminal-here (&optional *git-root-path)
  "Launch terminal from the current file location.  Use GIT-ROOT-PATH."
  (interactive "P")
  (if *git-root-path
      (if (git-root-dir)
          (progn
            (shell-command
             (format "open -a Terminal %s"
                     (git-root-dir))))
        (progn
          (message (concat "'" (file-name-nondirectory buffer-file-name) "' is not in git repository"))))
    (progn
      (shell-command
       (format "open -a Terminal %s"
               (expand-file-name default-directory))))))

(bind-key "C-c o t" 'surya/open-Terminal-here)

;;----------------------------------------------------------------------------
;; change all prompts to y or n
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------------------
;; browse url of file from emacs opens in default browser
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c o b") 'browse-url-of-file)

;;----------------------------------------------------------------------------
;; reveal file in finder
;;----------------------------------------------------------------------------
(defun reveal-in-finder ()
  "Open current file in finder."
  (interactive)
  (shell-command (format "open -R %s" (buffer-file-name))))

(global-set-key (kbd "C-c o f") 'reveal-in-finder)

;;----------------------------------------------------------------------------
;; smex gets list of recent files, commands as first option
;;----------------------------------------------------------------------------
(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (locate-user-emacs-file "cache/smex-items")))

;;----------------------------------------------------------------------------
;; save recent files list
;;----------------------------------------------------------------------------
(use-package recentf
  :config
  (setq recentf-save-file (locate-user-emacs-file "cache/recent-files"))
  (setq recentf-exclude '("/\\.git/.*\\'"
                          "/elpa/.*\\'"
                          "/elfeed/.*\\'"
                          "/cache/.*\\'"
                          ".*\\.gz\\'"))
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 20)
  (setq recentf-auto-cleanup 600)
  (recentf-mode))

;;----------------------------------------------------------------------------
;; remember cursor/point position in buffers using saveplace
;;----------------------------------------------------------------------------
;; (save-place-mode 1)
;; (setq save-place-file (locate-user-emacs-file "cache/saved-places"))
;; Remember point position in files
(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "cache/saved-places"))
  (save-place-mode))

;;----------------------------------------------------------------------------
;; prompts all available key bindings in a given buffer
;;----------------------------------------------------------------------------
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode) (which-key-setup-side-window-right))

;;----------------------------------------------------------------------------
;; hide show mode
;;----------------------------------------------------------------------------
(use-package hideshow
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :init
  (dolist (hook '(c-mode-common-hook
                  prog-mode-hook
                  emacs-lisp-mode-hook
                  python-mode-hook))
    (add-hook hook #'hs-minor-mode))
  :config
  ;; Unfold when search is active
  (setq hs-isearch-open t))

;;----------------------------------------------------------------------------
;; don't show some minor modes
;;----------------------------------------------------------------------------
(use-package diminish
  :config
  (eval-after-load "hideshow" '(diminish 'hs-minor-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "autorevert" '(diminish 'auto-revert-mode))
  (eval-after-load "abbrev-mode-hook" '(diminish 'abbrev-mode)))

;;----------------------------------------------------------------------------
;; use rust mode
;;----------------------------------------------------------------------------
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c b") #'rust-format-buffer))))

;;----------------------------------------------------------------------------
;; compile rust file and display output in mini buffer
;;----------------------------------------------------------------------------
(defun rust-save-compile-and-run ()
  "Run and compile rust code."
  (interactive)
  (save-buffer)
  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
      (compile "cargo run")
    (shell-command
     (format "rustc %s && %s"
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name))))))

(add-hook 'rust-mode-hook
          (lambda ()
            (define-key rust-mode-map (kbd "C-c e") 'rust-save-compile-and-run)))

;;----------------------------------------------------------------------------
;; expand region
;;----------------------------------------------------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;----------------------------------------------------------------------------
;; symbol highlight at point and navigate next previous
;;----------------------------------------------------------------------------
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind (
         ("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
  (add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
  :config
  (defadvice highlight-symbol-temp-highlight (around maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
                (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

;;----------------------------------------------------------------------------
;; multiple cursors
;;----------------------------------------------------------------------------
(defvar mc/list-file (locate-user-emacs-file "cache/mc-lists"))

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c m r" . set-rectangular-region-anchor)
         ("C-c m c" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)))

;;----------------------------------------------------------------------------
;; add js, html, json and css mode maps after compiling
;;----------------------------------------------------------------------------
(eval-when-compile
  (progn
    (defvar js-mode-map)
    (defvar json-mode-map)
    (defvar html-mode-map)
    (defvar css-mode-map)))

;;----------------------------------------------------------------------------
;; javascript file build with node and show output
;;----------------------------------------------------------------------------
(eval-after-load 'js
  (lambda()
    (define-key js-mode-map (kbd "C-c e") '(lambda ()  (interactive) (shell-command-on-region (point-min) (point-max) "node")))
    (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)))

;;----------------------------------------------------------------------------
;; enable web beautify mode for js, css, html
;;----------------------------------------------------------------------------
(use-package web-beautify)

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'json-mode-beautify))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(eval-after-load 'less-css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;;----------------------------------------------------------------------------
;; use Prettier for JS mode formatting
;; prettier js used to format javascript, useful for react and jsx
;;----------------------------------------------------------------------------
(use-package prettier-js
  :bind ("C-c p" . prettier-js))

;;----------------------------------------------------------------------------
;; use js-mode for react jsx and disable flycheck
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . (lambda ()
                                                (js-mode)
                                                (flycheck-mode -1))))

;;----------------------------------------------------------------------------
;; markdown mode
;;----------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-hook 'markdown-mode-hook #'tildify-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode))

;;----------------------------------------------------------------------------
;; set default color theme
;;----------------------------------------------------------------------------
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

;;----------------------------------------------------------------------------
;; sse json-mode
;;----------------------------------------------------------------------------
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist `(,(rx ".json" string-end) . json-mode)))

;;----------------------------------------------------------------------------
;; setup clang-format and execute C programs configuration
;; (add-hook 'before-save-hook 'clang-format-before-save).
;;----------------------------------------------------------------------------
(use-package clang-format)
(defun clang-format-before-save ()
  "Add this to .emacs to clang-format on save."
  (interactive)
  (when (eq major-mode 'c-mode) (clang-format-buffer)))

;; install hook to use clang-format on save
(add-hook 'before-save-hook 'clang-format-before-save)

(eval-after-load 'cc-mode
  '(define-key c-mode-map (kbd "C-c e") '(lambda ()  (interactive) (defvar sk-build-command) (setq sk-build-command (concat "clang " (buffer-name) " && ./a.out")) (shell-command sk-build-command) )))

;;----------------------------------------------------------------------------
;; load yasnippets
;;----------------------------------------------------------------------------
(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :init
  (add-hook 'sgml-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'term-mode-hook (lambda()
                              (yas-minor-mode -1)))
  :config (yas-reload-all))

;;----------------------------------------------------------------------------
;; enable global electric-indent-mode
;;----------------------------------------------------------------------------
(electric-indent-mode t)

;;----------------------------------------------------------------------------
;; auto generate closing brackets globally using Electric pair mode
;;----------------------------------------------------------------------------
(electric-pair-mode t)

;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\< . ?\>)
                            ))

;; ----------------------------------------------------------------------------
;; Use less-css-mode
;; Activate company mode css hints for less mode
;; Set less indentation to two spaces
;; ----------------------------------------------------------------------------
(use-package less-css-mode)

;;----------------------------------------------------------------------------
;; use undo-tree
;;----------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode)
  :commands global-undo-tree-mode
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,(locate-user-emacs-file "cache/undo/"))))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps t))

;;----------------------------------------------------------------------------
;; use Avy to jump between words in visible buffers
;;----------------------------------------------------------------------------
(use-package avy
  :bind
  ("C-;" . avy-goto-char))
  ;; ("C-:" . avy-goto-subword-1))

;;----------------------------------------------------------------------------
;; enable flycheck mode globally
;;----------------------------------------------------------------------------
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(html-tidy javascript-jshint)))

;;----------------------------------------------------------------------------
;; load eslint,tslint from local node_modules when possible
;;----------------------------------------------------------------------------
(defun use-eslint-from-node-modules ()
  "Load eslint from local node_modules if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)

(defun use-tslint-from-node-modules ()
  "Load tslint from local node_modules if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name (if (eq system-type 'windows-nt)
                                            "node_modules/.bin/tslint.cmd"
                                          "node_modules/.bin/tslint")
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

(add-hook 'flycheck-mode-hook #'use-tslint-from-node-modules)

;;----------------------------------------------------------------------------
;; set typescript mode
;;----------------------------------------------------------------------------
(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1))

(use-package tide
  :diminish tide-mode
  :config
  (define-key tide-mode-map (kbd "C-c b") 'tide-format))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

;;----------------------------------------------------------------------------
;; use diff-hl  mode
;;----------------------------------------------------------------------------
(use-package diff-hl
  :ensure t
  :commands global-diff-hl-mode
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  :config
  ;; Update diffs immediately
  (diff-hl-flydiff-mode)
  ;; Add hooks for other packages
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; Use margin display when in terminal
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

;;----------------------------------------------------------------------------
;; show matching parens
;; highlight matching parentheses
;;----------------------------------------------------------------------------
(use-package paren
  :config
  (setq show-paren-delay 0)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (show-paren-mode))

;;----------------------------------------------------------------------------
;; magit status
;;----------------------------------------------------------------------------
(use-package magit
  :config
  (add-hook 'magit-mode-hook 'visual-line-mode)
  (setq magit-push-current-to-pushremote t)
  :ensure t
  :bind (("C-c v v" . magit-status)
         ("C-c v c" . magit-clone)
         ("C-c v b" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull)))


;;----------------------------------------------------------------------------
;; set feature mode to edit Gherkin feature files
;; feature-mode needs a hook to diminish orgtbl-mode
;;----------------------------------------------------------------------------
(use-package feature-mode)
(add-hook 'feature-mode-hook
          (lambda ()
            (diminish 'orgtbl-mode)))

;;----------------------------------------------------------------------------
;; set ivy mode, counsel for auto completions across emacs
;;----------------------------------------------------------------------------
(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (setq counsel-find-file-at-point t))

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  :bind (
         ("M-x" . counsel-M-x)
         ("\C-s" . swiper)
         ("C-S-s" . swiper-all)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-git)
         ("C-c r" . counsel-rg)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)))

;;----------------------------------------------------------------------------
;; ivy-hydra brings some extra goodness to ivy-buffer with C-o
;;----------------------------------------------------------------------------
(use-package ivy-hydra)

;;----------------------------------------------------------------------------
;; use rg frontend for ripgrep search
;;----------------------------------------------------------------------------
(use-package rg
  :bind ("C-c R" . rg))

;;----------------------------------------------------------------------------
;; use ido-mode
;; use C-f during file selection to switch to regular find-file
;;----------------------------------------------------------------------------
(use-package ido
  :config
  (setq ido-everywhere t)
  (setq ido-case-fold t)
  (setq ido-use-filename-at-point  nil)
  (setq ido-use-filename-at-point nil) ; don't use filename at point (annoying)
  (setq ido-use-url-at-point nil) ; don't use url at point (annoying)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching nil) ; don't try to be too smart
  (setq-default org-completion-use-ido t)
  (setq-default magit-completing-read-function 'magit-ido-completing-read)
  (ido-mode 1))

(setq ido-save-directory-list-file (locate-user-emacs-file "cache/ido-last"))

;;----------------------------------------------------------------------------
;; add rainbow mode to highlight hex/rgb colors in html, css, sass, js etc
;;----------------------------------------------------------------------------
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (dolist (hook '(css-mode-hook html-mode-hook js-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

;;----------------------------------------------------------------------------
;; Fast window switching
;; shift <-- --> up down arrow keys to move point between buffers
;;----------------------------------------------------------------------------
(use-package windmove
  :config
  (setq windmove-wrap-around t)
  (windmove-default-keybindings))

;;----------------------------------------------------------------------------
;; winner mode for saving windows layouts and toggle between them
;;----------------------------------------------------------------------------
;; Undo and redo the window configuration
(use-package winner
  :bind (:map winner-mode-map
              ("C-c w u" . winner-undo)
              ("C-c w r" . winner-redo))
  :commands winner-mode
  :init
  (winner-mode)
  :config
  ;; Disable conflicting key bindings
  (unbind-key "C-c <left>" winner-mode-map)
  (unbind-key "C-c <right>" winner-mode-map))

;;----------------------------------------------------------------------------
;; rearrange window/buffer size
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-<up>") 'enlarge-window)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)

;;----------------------------------------------------------------------------
;; shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(use-package move-dup
  :bind (("M-<up>" . md/move-lines-up)
         ("M-<down>" . md/move-lines-down)
         ("C-c d d" . md/duplicate-down)
         ("C-c d u" . md/duplicate-up)))

;;----------------------------------------------------------------------------
;; add clipboard kills from other programs to emacs kill ring
;;----------------------------------------------------------------------------
(setq select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;;----------------------------------------------------------------------------
;; delete selection when pasting text
;;----------------------------------------------------------------------------
(delete-selection-mode 1)


;;----------------------------------------------------------------------------
;; cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode t))
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

;;----------------------------------------------------------------------------
;; new line and indent
;;----------------------------------------------------------------------------
(defun newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'newline-at-end-of-line)

(defun newline-before-the-current-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-<return>") 'newline-before-the-current-line)

;;----------------------------------------------------------------------------
;; update buffer if changed on disk automatically
;;----------------------------------------------------------------------------
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

;;----------------------------------------------------------------------------
;; page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

;;----------------------------------------------------------------------------
;; use package wgrep to edit multiple search results
;;----------------------------------------------------------------------------
(use-package wgrep)

;;----------------------------------------------------------------------------
;; kill back to indentation
;;----------------------------------------------------------------------------
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
;;----------------------------------------------------------------------------
(defun sk/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sk/split-window)
      (progn
        (jump-to-register :sk/split-window)
        (setq this-command 'sk/unsplit-window))
    (window-configuration-to-register :sk/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sk/split-window)

;;----------------------------------------------------------------------------
;; removes default key binding for M-left and M-right
;; train myself to use M-f and M-b instead
;;----------------------------------------------------------------------------
(global-unset-key [M-left])
(global-unset-key [M-right])

;;----------------------------------------------------------------------------
;; delete pairs of quotes brackets, parens, etc...
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c d p") 'delete-pair)

;;----------------------------------------------------------------------------
;; experimental settings - try them before adding to init.el
;;----------------------------------------------------------------------------















;;----------------------------------------------------------------------------
;; end of init settings, provide init
;;----------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here

;;----------------------------------------------------------------------------
;; Try not to change custom-set-variables, also try to keep them to minimum
;;----------------------------------------------------------------------------



;;----------------------------------------------------------------------------
;; custom settings
;;----------------------------------------------------------------------------
