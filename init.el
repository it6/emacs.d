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
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;----------------------------------------------------------------------------
;; suppress some GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
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
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
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
        (concat " ▓ " (abbreviate-file-name default-directory)))
       (dired-directory
        (concat " ▓ " dired-directory))
       (t
        " ▓ no file")))))

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
(set-fontset-font "fontset-default" 'unicode "Source Code Pro")
(setq default-frame-alist '((font . "operator mono-15")))

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
;; enable recursive minibuffers
;;----------------------------------------------------------------------------
(setq enable-recursive-minibuffers t)

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
(setq css-indent-offset 2)
(setq typescript-indent-level 2)
(setq js-indent-level 2)

;;----------------------------------------------------------------------------
;; cleanup whitespace before saving a file
;;----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)

;;----------------------------------------------------------------------------
;; draw block cursor as wide as the glyph under it
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

(global-set-key (kbd "C-c n") 'rename-this-buffer-and-file)

;;----------------------------------------------------------------------------
;; mouse yank at point instead of click
;;----------------------------------------------------------------------------
(setq mouse-yank-at-point t)

;;----------------------------------------------------------------------------
;; increase maximum size of the mark ring
;;----------------------------------------------------------------------------
(setq mark-ring-max 30)

;;----------------------------------------------------------------------------
;; use spaces instead of tabs and set default tab width
;;----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;----------------------------------------------------------------------------
;; ignore case on completion
;;----------------------------------------------------------------------------
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;;----------------------------------------------------------------------------
;; emacs initial scratch message
;;----------------------------------------------------------------------------
(setq-default initial-scratch-message
  (concat ";; Happy hacking Surya - Emacs ♥ you!\n\n"))

;;----------------------------------------------------------------------------
;; maximize emacs window on load
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
  (kill-buffer buffer)))
    (message "「Closed all other buffers」")))

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
     (message "Directory path copied: 「%s」" (file-name-directory -fpath))
     (file-name-directory -fpath))
       (progn
   (message "File path copied: 「%s」" -fpath)
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
     (message "Directory path copied: 「%s」" (file-name-directory -fpath)))))

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
        (message "GIT root path copied:「%s」" (git-root-dir)))
    (progn
      (kill-new default-directory)
      (message "File not in GIT repo, copied default path:「%s」" default-directory))))

(global-set-key (kbd "C-c c v") 'git-root-path)

;;----------------------------------------------------------------------------
;; make searches case sensitive
;; make dabbrev completion case sensitive
;;----------------------------------------------------------------------------
(defvar dabbrev-case-fold-search nil)

;;----------------------------------------------------------------------------
;; zap *up* to char is a handy pair for zap-to-char
;; zop-to-char for visual representation of content before zapping
;;----------------------------------------------------------------------------
(use-package zop-to-char
  :ensure t
  :bind (([remap zap-to-char] . zop-to-char)
         ("M-Z" . zop-up-to-char)))

;;----------------------------------------------------------------------------
;; delete upto camel case or sub words
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
;; toggle between all open buffers quickly
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-`") 'switch-bury-or-kill-buffer)

(defun switch-bury-or-kill-buffer (&optional aggr)
  "With no argument, switch (but unlike `C-x b', without the need to confirm).
With `C-u', bury current buffer.  With double `C-u',
kill it (unless it's modified).  Optional argument AGGR."
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
;; don't open a new frame when emacs is already open
;;----------------------------------------------------------------------------
(setq ns-pop-up-frames nil)

;;----------------------------------------------------------------------------
;; rearrange window height
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
(global-set-key (kbd "C-c o b") 'browse-url-of-file)

;;----------------------------------------------------------------------------
;; ANSI Term
;;----------------------------------------------------------------------------
(bind-key "C-c o t" #'ansi-term)

;;----------------------------------------------------------------------------
;; reveal file in finder
;;----------------------------------------------------------------------------
(defun reveal-in-finder ()
  "Open current file in finder."
  (interactive)
  (shell-command (concat "open -R " buffer-file-name)))

(global-set-key (kbd "C-c o f") 'reveal-in-finder)

;;----------------------------------------------------------------------------
;; enable Dash Help
;; support for the http://kapeli.com/dash documentation browser
;;----------------------------------------------------------------------------
(defconst *is-a-mac* (eq system-type 'darwin))
(defun sanityinc/dash-installed-p ()
  "Return t if Dash is installed on this machine, or nil otherwise."
  (let ((lsregister "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"))
    (and (file-executable-p lsregister)
         (not (string-equal
               ""
               (shell-command-to-string
                (concat lsregister " -dump|grep com.kapeli.dash")))))))

(when (and *is-a-mac* (not (package-installed-p 'dash-at-point)))
  (message "Checking whether Dash is installed")
  (when (sanityinc/dash-installed-p)
    (use-package dash-at-point)))

(when (package-installed-p 'dash-at-point)
  (global-set-key (kbd "C-c o d") 'dash-at-point))

;;----------------------------------------------------------------------------
;; smex gets list of recent files, commands as first option
;;----------------------------------------------------------------------------
(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (locate-user-emacs-file "cache/smex-items")))

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
(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "cache/saved-places"))

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
  (defun drot|display-code-line-counts (ov)
    "Unique overlay function to be applied with `hs-minor-mode'."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (format "<---> | %d"
                           (count-lines (overlay-start ov)
                                        (overlay-end ov))))))
  ;; Unfold when search is active and apply custom overlay
  (setq hs-set-up-overlay #'drot|display-code-line-counts)
  (setq hs-isearch-open t))

;;----------------------------------------------------------------------------
;; show indent guides
;;----------------------------------------------------------------------------
(use-package indent-guide
  :diminish indent-guide-mode
  :config
  (add-hook 'prog-mode-hook 'indent-guide-mode)
  (setq indent-guide-char "¦"))
  ;; (setq indent-guide-char "┊┆ ⁞, ⋮, ┆, ┊, ┋, ┇, ︙, │, ┆,│, ┊, ┆, ¦"))

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
    ("C-+" . mc/mark-next-like-this)
    ("C-c C-<" . mc/mark-all-like-this)
    ("C-c m r" . set-rectangular-region-anchor)
    ("C-c m c" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)))

;;----------------------------------------------------------------------------
;; javascript file build with node and show output
;;----------------------------------------------------------------------------
(eval-when-compile
  (progn
         (defvar js-mode-map)
         (defvar json-mode-map)
         (defvar html-mode-map)
         (defvar css-mode-map)))

(eval-after-load 'js
  (lambda()
     (define-key js-mode-map (kbd "C-c e") '(lambda ()  (interactive) (shell-command-on-region (point-min) (point-max) "node")))
     (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)))

;;----------------------------------------------------------------------------
;; use letters in circles as mode names to reduce clutter
;;----------------------------------------------------------------------------
(add-hook 'js-mode-hook
  (lambda()
    (setq mode-name "Ⓙ")))

(add-hook 'c-mode-hook
  (lambda()
    (setq mode-name "Ⓒ")))

(add-hook 'json-mode-hook
  (lambda()
    (setq mode-name "Ⓙ")))

(add-hook 'sgml-mode-hook
  (lambda()
    (setq mode-name "Ⓗ")))

(add-hook 'css-mode-hook
  (lambda()
    (setq mode-name "Ⓒ")))

(add-hook 'less-css-mode-hook
  (lambda()
    (setq mode-name "Ⓛ")))

(add-hook 'scss-mode-hook
  (lambda()
    (setq mode-name "Ⓢ")))

(add-hook 'feature-mode-hook
  (lambda()
    (setq mode-name "Ⓕ")))

(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq mode-name "Ⓔ")))

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
  :commands yas-global-mode
  :init
  ;; we don't want yasnippet running in terminals
  (add-hook 'term-mode-hook (lambda()
                              (yas-minor-mode -1))))

;;----------------------------------------------------------------------------
;; enable global electric-indent-mode
;;----------------------------------------------------------------------------
(electric-indent-mode t)

;;----------------------------------------------------------------------------
;; auto generate closing brackets globally
;;----------------------------------------------------------------------------
(electric-pair-mode t)

;;----------------------------------------------------------------------------
;; use company mode
;;----------------------------------------------------------------------------
(use-package company
  :diminish company-mode)
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

(add-hook 'css-mode-hook
    (lambda ()
      (company-mode +1)
      (set (make-local-variable 'company-backends) '(company-css))))

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
  ("C-:" . avy-goto-char)
  ("C-;" . avy-goto-subword-1))

;;----------------------------------------------------------------------------
;; enable flycheck mode globally
;;----------------------------------------------------------------------------
(use-package flycheck
  :diminish(flycheck-mode "ⓕ")
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(html-tidy javascript-jshint)))

;;----------------------------------------------------------------------------
;; load eslint from local node_modules when possible
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
;; set typescript mode
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
;; (use-package diff-hl)
;; (global-diff-hl-mode)

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
(show-paren-mode 1)
(setq show-paren-when-point-inside-paren t)

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
;;----------------------------------------------------------------------------
(use-package feature-mode)

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
  :diminish (ivy-mode "ⓘ")
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
;; make "C-x o" prompt for a target window when there are more than 2
;;----------------------------------------------------------------------------
(use-package switch-window
  :config
    (setq-default switch-window-shortcut-style 'alphabet)
    (setq-default switch-window-timeout nil)
  :bind("C-x o" . switch-window))

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
(setq save-interprogram-paste-before-kill t)

;;----------------------------------------------------------------------------
;; update buffer if changed on disk automatically
;;----------------------------------------------------------------------------
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

;;----------------------------------------------------------------------------
;; don't show some minor modes
;;----------------------------------------------------------------------------
(diminish 'auto-revert-mode)
(add-hook 'orgtbl-mode-hook
          '(lambda ()
             (diminish 'orgtbl-mode)))
(add-hook 'abbrev-mode-hook
          '(lambda ()
             (diminish 'abbrev-mode)))

;;----------------------------------------------------------------------------
;; C-x ;, C-x : to comment/uncomment current line
;;----------------------------------------------------------------------------
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-x :") 'toggle-comment-on-line)

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
;; move buffers between frames
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
;; re-indent new open line
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
;; delete pairs of quotes brackets, parens, etc...
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c d p") 'delete-pair)

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
