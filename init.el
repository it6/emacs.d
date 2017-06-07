;;; package --- Summary
;;; Commentary:
;;; Code:
;;----------------------------------------------------------------------------
;; Require package
;;----------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")))

;;----------------------------------------------------------------------------
;; Debugging Message Limit to 10000
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
;; Put underline below the font bottom line
;;----------------------------------------------------------------------------
(setq x-underline-at-descent-line t)

;;----------------------------------------------------------------------------
;; Keep window point when switching buffers this works across frames
;;----------------------------------------------------------------------------
(setq switch-to-buffer-preserve-window-point t)

;;----------------------------------------------------------------------------
;; Do not save duplicates in history and kill ring
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
;; Please don't load outdated byte code
;;----------------------------------------------------------------------------
(setq load-prefer-newer t)

;;----------------------------------------------------------------------------
;; Go download any package if not already present on my system
;;----------------------------------------------------------------------------
(setq use-package-always-ensure t)

;;----------------------------------------------------------------------------
;; set path on mac
;;----------------------------------------------------------------------------
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ----------------------------------------------------------------------------
;; Some basic preferences
;; ----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file "cache/bookmarks-items")
 buffers-menu-max-size 30
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)
(transient-mark-mode t)

;;----------------------------------------------------------------------------
;; Show file path in frame title
;;----------------------------------------------------------------------------
(setq-default frame-title-format
        '(:eval
    (format "%s - %s"
      (buffer-name)
      (cond
       (buffer-file-truename
        (concat "(" (abbreviate-file-name default-directory) ")"))
       (dired-directory
        (concat "{" dired-directory "}"))
       (t
        "[no file]")))))

;;----------------------------------------------------------------------------
;; Set default directory for cache
;;----------------------------------------------------------------------------
(make-directory (locate-user-emacs-file "cache") t)

;;----------------------------------------------------------------------------
;; Move point all the way when scrolling to buffer boundaries
;;----------------------------------------------------------------------------
(setq scroll-error-top-bottom t)

;;----------------------------------------------------------------------------
;; set regular font and unicode characters needs unicode font
;;----------------------------------------------------------------------------
(set-fontset-font "fontset-default" 'unicode "Source Code Pro")
(setq default-frame-alist '((font . "mononoki-15")))

;;----------------------------------------------------------------------------
;; reopen desktop with same size as last closed session
;;----------------------------------------------------------------------------
(setq desktop-base-file-name (locate-user-emacs-file "cache/emacs-desktop"))
(desktop-save-mode 1)

;;----------------------------------------------------------------------------
;; A simple visible bell which works in all terminal types
;;----------------------------------------------------------------------------
(defun flash-mode-line ()
  "Flash modeline on bad commands."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))

(setq-default
 ring-bell-function 'flash-mode-line)

;;----------------------------------------------------------------------------
;; set option as super and command as meta keys on osx
;;----------------------------------------------------------------------------
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;;----------------------------------------------------------------------------
;; auto generate closing brackets globally
;;----------------------------------------------------------------------------
(electric-pair-mode t)

;;----------------------------------------------------------------------------
;; Enable recursive minibuffers
;;----------------------------------------------------------------------------
(setq enable-recursive-minibuffers t)

;;----------------------------------------------------------------------------
;; Enable all disabled commands
;;----------------------------------------------------------------------------
(setq disabled-command-function nil)

;;----------------------------------------------------------------------------
;; Indicate minibuffer recursion depth
;;----------------------------------------------------------------------------
(minibuffer-depth-indicate-mode)

;;----------------------------------------------------------------------------
;; set indent/tab width to 2 spaces
;;----------------------------------------------------------------------------
(setq css-indent-offset 2)
(setq typescript-indent-level 2)
(setq js-indent-level 2)

;;----------------------------------------------------------------------------
;; cleanup whitespace before saving a file
;;----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)

;;----------------------------------------------------------------------------
;; Draw block cursor as wide as the glyph under it
;;----------------------------------------------------------------------------
(setq x-stretch-cursor t)

;;----------------------------------------------------------------------------
;; use conf-unix-mode for default dot files
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.npmrc\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\zpreztorc\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-unix-mode))

;;----------------------------------------------------------------------------
;; Use Ibuffer for buffer list
;; use ibuffer instead of the default buffer list
;; Turning off ibuffer-show-empty-filter-groups is particularly useful,
;; because the empty filter groups can really clutter things up.
;;----------------------------------------------------------------------------
(use-package ibuffer-vc)

(defun ibuffer-set-up-preferred-filters ()
  "Set up filters for showing ibuffer."
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq-default ibuffer-show-empty-filter-groups nil)


(eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

;; Explicitly require ibuffer-vc to get its column definitions, which
;; can't be autoloaded
(eval-after-load 'ibuffer
  (use-package ibuffer-vc))

;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
        (name 18 18 :left :elide)
        " "
        (size-h 9 -1 :right)
        " "
        (mode 16 16 :left :elide)
        " "
        filename-and-process)
  (mark modified read-only vc-status-mini " "
        (name 18 18 :left :elide)
        " "
        (size-h 9 -1 :right)
        " "
        (mode 16 16 :left :elide)
        " "
        (vc-status 16 16 :left)
        " "
        filename-and-process)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;----------------------------------------------------------------------------
;; Treat all themes as safe
;;----------------------------------------------------------------------------
(setq custom-safe-themes t)

;;----------------------------------------------------------------------------
;; Imenu configuration
;;----------------------------------------------------------------------------
(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan t))

;;----------------------------------------------------------------------------
;; Rename both buffer and file name
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

(global-set-key (kbd "C-c n") 'rename-this-buffer-and-file)

;;----------------------------------------------------------------------------
;; add new line at the end of the file
;;----------------------------------------------------------------------------
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;;----------------------------------------------------------------------------
;; Mouse yank at point instead of click
;;----------------------------------------------------------------------------
(setq mouse-yank-at-point t)

;;----------------------------------------------------------------------------
;; Increase maximum size of the mark ring
;;----------------------------------------------------------------------------
(setq mark-ring-max 30)

;;----------------------------------------------------------------------------
;; Use spaces instead of tabs and set default tab width
;;----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;----------------------------------------------------------------------------
;; Ignore case on completion
;;----------------------------------------------------------------------------
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;;----------------------------------------------------------------------------
;; Disable toolbars and startup screen
;;----------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;;----------------------------------------------------------------------------
;; Emacs Initial Scratch Message
;;----------------------------------------------------------------------------
(setq-default initial-scratch-message
  (concat ";; Happy hacking Surya - Emacs ♥ you!\n\n"))

;;----------------------------------------------------------------------------
;; Maximize emacs window on load
;;----------------------------------------------------------------------------
(setq frame-resize-pixelwise t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

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
  (kill-buffer buffer)))))

(global-set-key (kbd "C-c k") 'kill-other-buffers)

;;----------------------------------------------------------------------------
;; copy file or directory path from current buffer to clipboard
;;----------------------------------------------------------------------------
(defun xah-copy-file-path (&optional *dir-path-only-p)
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
     (message "Directory path copied: 「%s」" (file-name-directory -fpath))
     (file-name-directory -fpath))
       (progn
   (message "File path copied: 「%s」" -fpath)
   -fpath )))))

(global-set-key (kbd "C-c c") 'xah-copy-file-path)

;;----------------------------------------------------------------------------
;; open git root path
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
        (message "GIT root path copied:「%s」" (git-root-dir))
        (kill-new (git-root-dir)))
    (progn
      (message "File not in GIT repo, copied default path:「%s」" default-directory)
      (kill-new default-directory))))

(global-set-key (kbd "C-c v") 'git-root-path)

;;----------------------------------------------------------------------------
;; Make searches case sensitive
;; make dabbrev completion case sensitive
;;----------------------------------------------------------------------------
(setq dabbrev-case-fold-search nil)

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;;----------------------------------------------------------------------------
;; Delete upto camel case or sub words
;;----------------------------------------------------------------------------
(defconst camelCase-regexp "\\([A-Z]?[a-z]+\\|[A-Z]+\\|[0-9]+\\)"
  ;; capital must be before uppercase
  "Regular expression that matches a camelCase word.
defined as Capitalized, lowercase, or UPPERCASE sequence of letters,
or sequence of digits.")

(defun camelCase-forward-word (count)
  "Move point foward COUNT camelCase words."
  (interactive "p")
  ;; search forward increments point until some match occurs;
  ;; extent of match is as large as possible at that point.
  ;; point is left at END of match.
  (if (< count 0)
      (camelCase-backward-word (- count))
    (let ((old-case-fold-search case-fold-search)
    (case-fold-search nil)) ;; search case sensitively
      (unwind-protect
    (when (re-search-forward camelCase-regexp nil t count)
      ;; something matched, just check for special case.
      ;; If uppercase acronym is in camelCase word as in "URLNext",
      ;; search will leave point after N rather than after L.
      ;; So if match starting back one char doesn't end same place,
      ;; then back-up one char.
      (when (save-excursion
  (let ((search-end (point)))
    (forward-char -1)
    (and (looking-at camelCase-regexp)
   (not (= search-end (match-end 0))))))
  (forward-char -1))
      (point))
  (setq case-fold-search old-case-fold-search)))))

(defun camelCase-backward-word (count)
  "Move point backward COUNT camelCase words."
  (interactive "p")
  ;; search backward decrements point until some match occurs;
  ;; extent of match is as large as possible at that point.
  ;; So once point is found, have to keep decrementing point until we cross
  ;; into another word, which changes the match end.
  ;; for multiple words, have to do whole thing COUNT times.
  (if (< count 0)
      (camelCase-forward-word (- count))
    (let ((start-position (point))
    (old-case-fold-search case-fold-search)
    (case-fold-search nil)) ;; search case-sensitively
      (unwind-protect
    (while (< 0 count)
      (setq count (1- count))
      (let ((start (point)))
  (when (re-search-backward camelCase-regexp nil t)
    (let ((end-word (match-end 0)))
      (forward-char -1)
      (while (save-excursion
   ;;like looking-at, but stop match at start
   (let ((position (point)))
     (re-search-forward camelCase-regexp start t)
     (and (= position (match-beginning 0))
    (= end-word (match-end 0)))))
  (forward-char -1))
      (forward-char 1)))))
  (setq case-fold-search old-case-fold-search))
      (if (= start-position (point)) nil (point)))))

(defun camelCase-forward-kill-word (count)
  "Kill text between current point and end of next camelCase word.
COUNT will take an argument"
  (interactive "*p")
  (kill-region (point) (progn (camelCase-forward-word count) (point))))

(defun camelCase-backward-kill-word (count)
  "Kill text between current point and end of previous camelCase word.
COUNT will take an argument"
  (interactive "*p")
  (kill-region (point) (progn (camelCase-backward-word count) (point))))

(global-set-key (kbd "C-c <backspace>") 'camelCase-forward-kill-word)
(global-set-key (kbd "<C-backspace>") 'camelCase-backward-kill-word)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-`") 'switch-bury-or-kill-buffer)

(defun switch-bury-or-kill-buffer (&optional aggr)
  "With no argument, switch (but unlike C-x b, without the need
to confirm).  With C-u, bury current buffer.  With double C-u,
kill it (unless it's modified)."
  (interactive "P")
  (cond
   ((eq aggr nil) (progn
        (cl-dolist (buf '("*Buffer List*" "*IBuffer*"))
          (when (get-buffer buf)
      (bury-buffer buf)))
        (switch-to-buffer (other-buffer))))
   ((equal aggr '(4)) (bury-buffer))
   ((equal aggr '(16)) (kill-buffer-if-not-modified (current-buffer)))))

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
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
;; New line and indent
;;----------------------------------------------------------------------------
(global-set-key (kbd "RET") 'newline-and-indent)

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
;; don't open a new frame when emacs is already open
;;----------------------------------------------------------------------------
(setq ns-pop-up-frames nil)

;;----------------------------------------------------------------------------
;; Rearrange window height
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-<up>") 'enlarge-window)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)

;;----------------------------------------------------------------------------
;; change all prompts to y or n
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------------------
;; browse url of file from emacs opens in default browser
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c i") 'browse-url-of-file)

;;----------------------------------------------------------------------------
;; delete pairs of quotes brackets, parens, etc...
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c h") 'delete-pair)

;;----------------------------------------------------------------------------
;; reveal file in finder
;;----------------------------------------------------------------------------
(defun reveal-in-finder ()
  "Open current file in finder."
  (interactive)
  (shell-command (concat "open -R " buffer-file-name)))

(global-set-key (kbd "C-c o") 'reveal-in-finder)

;;----------------------------------------------------------------------------
;; Javascript file build with node and show output
;;----------------------------------------------------------------------------
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c e") '(lambda ()  (interactive) (shell-command-on-region (point-min) (point-max) "node"))))

;;----------------------------------------------------------------------------
;; smex gets list of recent files, commands as first option
;;----------------------------------------------------------------------------
(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (locate-user-emacs-file "cache/smex-items")))

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(use-package whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
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
;; Save recent files list
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
;; Remember cursor/point position in buffers using saveplace
;;----------------------------------------------------------------------------
(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "cache/saved-places"))

;;----------------------------------------------------------------------------
;; Prompts all available key bindings in a given buffer
;;----------------------------------------------------------------------------
(use-package which-key
  :init (which-key-mode) (which-key-setup-side-window-right))

;;----------------------------------------------------------------------------
;; Expand Region
;;----------------------------------------------------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;----------------------------------------------------------------------------
;; Symbol Highlight at Point and navigate next previous
;;----------------------------------------------------------------------------
(use-package highlight-symbol
  :bind (
    ("M-n" . highlight-symbol-next)
    ("M-p" . highlight-symbol-prev))
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
    (add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
    (diminish 'highlight-symbol-mode)
  :config
  (defadvice highlight-symbol-temp-highlight (around maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
    (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

;;----------------------------------------------------------------------------
;; Multiple Cursors
;;----------------------------------------------------------------------------
(setq mc/list-file (locate-user-emacs-file "cache/mc-lists"))

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
    ("C->" . mc/mark-next-like-this)
    ("C-+" . mc/mark-next-like-this)
    ("C-c C-<" . mc/mark-all-like-this)
    ("C-c m r" . set-rectangular-region-anchor)
    ("C-c m c" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)))

;;----------------------------------------------------------------------------
;; Enable web beautify mode for js, css, html
;;----------------------------------------------------------------------------
(use-package web-beautify)

;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'json-mode-beautify))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(eval-after-load 'less-css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;;----------------------------------------------------------------------------
;; Use Prettier for JS2 mode formatting
;; prettier js used to format javascript, useful for react and jsx
;;----------------------------------------------------------------------------
(use-package prettier-js
  :bind ("C-c p" . prettier-js))

;;----------------------------------------------------------------------------
;; set default color theme
;;----------------------------------------------------------------------------
(use-package color-theme-sanityinc-tomorrow
  :config
    (load-theme 'sanityinc-tomorrow-eighties t))

;;----------------------------------------------------------------------------
;; Use json-mode
;;----------------------------------------------------------------------------
(use-package json-mode
  :init
    (add-to-list 'auto-mode-alist `(,(rx ".json" string-end) . json-mode)))

;;----------------------------------------------------------------------------
;; Setup clang-format and execute C programs configuration
;; (add-hook 'before-save-hook 'clang-format-before-save).
;;----------------------------------------------------------------------------
(use-package clang-format)
(defun clang-format-before-save ()
  "Add this to .emacs to clang-format on save."
  (interactive)
  (when (eq major-mode 'c-mode) (clang-format-buffer)))

;; Install hook to use clang-format on save
(add-hook 'before-save-hook 'clang-format-before-save)

(eval-after-load 'cc-mode
  '(define-key c-mode-map (kbd "C-c e") '(lambda ()  (interactive) (defvar sk-build-command) (setq sk-build-command (concat "clang " (buffer-name) " && ./a.out")) (shell-command sk-build-command) )))

;;----------------------------------------------------------------------------
;; Load Yasnippets
;;----------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :commands yas-global-mode
  :init
  (add-hook 'after-init-hook #'yas-global-mode))

;;----------------------------------------------------------------------------
;; disable global electric-indent-mode
;;----------------------------------------------------------------------------
(electric-indent-mode 0)

;;----------------------------------------------------------------------------
;; use company mode
;;----------------------------------------------------------------------------
(use-package company)
; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; ----------------------------------------------------------------------------
;; Use less-css-mode
;; Activate company mode css hints for less mode
;; Set less indentation to two spaces
;; ----------------------------------------------------------------------------
(use-package less-css-mode)
(add-hook 'less-css-mode-hook
    (lambda ()
      (company-mode +1)
      (set (make-local-variable 'company-backends) '(company-css))))

;;----------------------------------------------------------------------------
;; Use undo-tree
;;----------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "UnT")
  :commands global-undo-tree-mode
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,(locate-user-emacs-file "cache/undo/"))))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps t))

;;----------------------------------------------------------------------------
;; Use Avy to jump between words in visible buffers
;;----------------------------------------------------------------------------
(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  ("C-;" . avy-goto-subword-1))

;;----------------------------------------------------------------------------
;; Enable flycheck mode globally
;;----------------------------------------------------------------------------
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(html-tidy javascript-jshint)))

;;----------------------------------------------------------------------------
;; Load eslint from local node_modules when possible
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

;;----------------------------------------------------------------------------
;; set typescript mode to use js2 syntax
;;----------------------------------------------------------------------------
(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
 :config
 (define-key tide-mode-map (kbd "C-c b") 'tide-format))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;----------------------------------------------------------------------------
;; use diff-hl  mode
;;----------------------------------------------------------------------------
(use-package diff-hl)
(global-diff-hl-mode)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Magit status
;;----------------------------------------------------------------------------
(use-package magit
  :config
  (add-hook 'magit-mode-hook 'visual-line-mode)
  :bind (("C-x g" . magit-status))
  :init (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;----------------------------------------------------------------------------
;; set feature mode to edit Gherkin feature files
;;----------------------------------------------------------------------------
(use-package feature-mode)

;;----------------------------------------------------------------------------
;; Set Ivy mode, counsel for auto completions across emacs
;;----------------------------------------------------------------------------
(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (setq counsel-find-file-at-point t))

 (use-package ivy
   :init
     (setq ivy-use-virtual-buffers t)
     (setq ivy-count-format "(%d/%d) ")
     (ivy-mode 1)
     :bind (
     ("M-x" . counsel-M-x)
     ("\C-s" . swiper)
     ("C-c s" . swiper-all)
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
  :bind ("C-c g" . rg))

;;----------------------------------------------------------------------------
;;use ido-mode
;; Use C-f during file selection to switch to regular find-file
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

;; ----------------------------------------------------------------------------
;; idomenu ubiquitous gets auto completion on all possible mini buffer menus
;; ----------------------------------------------------------------------------
(use-package ido-ubiquitous)
(ido-ubiquitous-mode t)

;;----------------------------------------------------------------------------
;; add rainbow mode to highlight hex/rgb colors in html, css, sass, js etc
;;----------------------------------------------------------------------------
(use-package rainbow-mode
  :config
  (dolist (hook '(css-mode-hook html-mode-hook js-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

;;----------------------------------------------------------------------------
;; Make "C-x o" prompt for a target window when there are more than 2
;;----------------------------------------------------------------------------
(use-package switch-window
  :config
    (setq-default switch-window-shortcut-style 'alphabet)
    (setq-default switch-window-timeout nil)
  :bind("C-x o" . switch-window))

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(use-package move-dup
  :bind (("M-<up>" . md/move-lines-up)
   ("M-<down>" . md/move-lines-down)
   ("C-c d" . md/duplicate-down)
   ("C-c D" . md/duplicate-up)))

;;----------------------------------------------------------------------------
;; Add clipboard kills from other programs to emacs kill ring
;;----------------------------------------------------------------------------
(setq save-interprogram-paste-before-kill t)

;;----------------------------------------------------------------------------
;; update buffer if changed on disk automatically
;;----------------------------------------------------------------------------
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;;----------------------------------------------------------------------------
;; M-; to comment/uncomment current line
;;----------------------------------------------------------------------------
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c C-;") 'toggle-comment-on-line)

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :init
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode))

;;----------------------------------------------------------------------------
;; use package wgrep to edit multiple search results
;;----------------------------------------------------------------------------
(use-package wgrep)

;;----------------------------------------------------------------------------
;; Move buffers between frames
;;----------------------------------------------------------------------------
(use-package buffer-move
  :bind(
  ("<M-S-up>"    . buf-move-up)
  ("<M-S-down>"  . buf-move-down)
  ("<M-S-left>"  . buf-move-left)
  ("<M-S-right>" . buf-move-right)
  ))

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
;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
;;----------------------------------------------------------------------------
(defun split-window()
  "Split the window to see the most recent buffer in the other window.Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'split-window)
      (progn
  (jump-to-register :split-window)
  (setq this-command 'unsplit-window))
    (window-configuration-to-register :split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'split-window)

;;----------------------------------------------------------------------------
;; Removes default key binding for M-left and M-right
;; Train myself to use M-f and M-b instead
;;----------------------------------------------------------------------------
(global-unset-key [M-left])
(global-unset-key [M-right])

;;----------------------------------------------------------------------------
;; Re-indent new open line
;;----------------------------------------------------------------------------
(defun open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.  If there is a fill prefix and/or a `left-margin', insert them on the new line if the line avy-goto-word-or-subword-1ld have been blank.  With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
   (do-left-margin (and (bolp) (> (current-left-margin) 0)))
   (loc (point-marker))
   ;; Don't expand an abbrev before point.
   (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
       (if do-left-margin (indent-to (current-left-margin)))
       (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-line-with-reindent)

;;----------------------------------------------------------------------------
;;; experimental settings - try them before adding to init.el
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