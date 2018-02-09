;;; package --- Summary
;;; Commentary:
;;; Code:



;;----------------------------------------------------------------------------
;; Delay garbage collection during startup
;;----------------------------------------------------------------------------
(setq gc-cons-threshold most-positive-fixnum)

;;----------------------------------------------------------------------------
;; Reset garbage collection threshold value to default after startup
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
    (lambda () (setq gc-cons-threshold 400000)))

;;----------------------------------------------------------------------------
;; Set default directory for save files
;;----------------------------------------------------------------------------
(make-directory (locate-user-emacs-file "cache") t)

;;----------------------------------------------------------------------------
;; Disable the site default settings
;;----------------------------------------------------------------------------
(setq inhibit-default-init t)

;;----------------------------------------------------------------------------
;; Activate packages and add the MELPA package archive
;;----------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/"))

;;----------------------------------------------------------------------------
;; Bootstrap use-package
;;----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;----------------------------------------------------------------------------
;; Load use-package
;;----------------------------------------------------------------------------
(eval-when-compile
  (require 'use-package))

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
;; auto save buffer or window on emacs focus lost
;;----------------------------------------------------------------------------
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

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
;; don't load outdated byte code, prefer newest version of a file
;;----------------------------------------------------------------------------
(setq load-prefer-newer t)

;;----------------------------------------------------------------------------
;; removes default key binding for M-left and M-right
;; train myself to use M-f and M-b instead
;;----------------------------------------------------------------------------
(global-unset-key [M-left])
(global-unset-key [M-right])

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

;; ----------------------------------------------------------------------------
;; some basic preferences
;; ----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file "cache/bookmarks-items")
 bookmark-save-flag 1
 buffers-menu-max-size 30
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 scroll-preserve-screen-position 'always
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

;;----------------------------------------------------------------------------
;; move point all the way when scrolling to buffer boundaries
;;----------------------------------------------------------------------------
(setq scroll-error-top-bottom t)

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
;; change all prompts to y or n
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------------------
;; cleanup whitespace before saving a file
;;----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)

;;----------------------------------------------------------------------------
;; draw block cursor as wide as the glyph under it
;;----------------------------------------------------------------------------
(setq x-stretch-cursor t)

;;----------------------------------------------------------------------------
;; show file path in frame title
;;----------------------------------------------------------------------------
(setq frame-title-format
      '(:eval (if (buffer-file-name)
      (abbreviate-file-name (buffer-file-name)) "%b")))

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
;; Enable recursive minibuffers
;;----------------------------------------------------------------------------
(setq enable-recursive-minibuffers t)

;;----------------------------------------------------------------------------
;; Indicate minibuffer recursion depth
;;----------------------------------------------------------------------------
(minibuffer-depth-indicate-mode)

;;----------------------------------------------------------------------------
;; don't open a new frame when emacs is already open
;;----------------------------------------------------------------------------
(setq ns-pop-up-frames nil)

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
(setq-default indicate-empty-lines t)

;;----------------------------------------------------------------------------
;; Require final new line for all buffers
;;----------------------------------------------------------------------------
(setq require-final-newline t)

;;----------------------------------------------------------------------------
;; Eval Buffer using C-c e in emacslisp mode
;;----------------------------------------------------------------------------
(define-key lisp-mode-shared-map (kbd "C-c e") 'eval-buffer)

;;----------------------------------------------------------------------------
;; Don't use dialogs for minibuffer input
;;----------------------------------------------------------------------------
(setq use-dialog-box nil)

;;----------------------------------------------------------------------------
;; maximize emacs window-height on load
;;----------------------------------------------------------------------------
(setq frame-resize-pixelwise t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;----------------------------------------------------------------------------
;; reopen same file and same window size as last closed session
;;----------------------------------------------------------------------------
(defvar desktop-base-file-name (locate-user-emacs-file "cache/emacs-desktop"))
(desktop-save-mode 1)

;;----------------------------------------------------------------------------
;; ignore case on completion
;;----------------------------------------------------------------------------
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;;----------------------------------------------------------------------------
;; auto generate closing brackets globally using Electric pair mode
;;----------------------------------------------------------------------------
(electric-pair-mode)

;;----------------------------------------------------------------------------
;; enable global case sensitive search
;; works when searching rg but not so great for swiper within buffer
;; ----------------------------------------------------------------------------
;; (setq-default case-fold-search nil)

;;----------------------------------------------------------------------------
;; make dabbrev completion case sensitive
;;----------------------------------------------------------------------------
(defvar dabbrev-case-fold-search nil)

;;----------------------------------------------------------------------------
;; electric mode don't indent current line
;; ----------------------------------------------------------------------------
(setq-default electric-indent-inhibit t)

;;----------------------------------------------------------------------------
;; emacs initial scratch buffer message
;;----------------------------------------------------------------------------
(setq-default initial-scratch-message "")

;;----------------------------------------------------------------------------
;; browse url of file from emacs opens in default browser
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c o b") 'browse-url-of-file)

;;----------------------------------------------------------------------------
;; delete matching pairs
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c d p") 'delete-pair)

;;----------------------------------------------------------------------------
;; delete back to sexpression
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-C-<backspace>") 'backward-kill-sexp)

;;----------------------------------------------------------------------------
;; set M-` to toggle all open EMACS windows
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-`") 'ns-next-frame)

;;----------------------------------------------------------------------------
;; delete selection when pasting text
;;----------------------------------------------------------------------------
(delete-selection-mode 1)

;;----------------------------------------------------------------------------
;; add clipboard kills from other programs to emacs kill ring
;;----------------------------------------------------------------------------
(setq save-interprogram-paste-before-kill t)

;;----------------------------------------------------------------------------
;; Dired configuration
;;----------------------------------------------------------------------------
(use-package dired
  :ensure nil
  :config
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
       (when gls (setq insert-directory-program gls)))
  ;; Default `ls' switches
  (setq dired-listing-switches "-alhF")
  ;; Do certain operations recursively
  (setq dired-recursive-deletes 'top)
  (setq dired-recursive-copies 'always)
  ;; Imitate orthodox file managers with two buffers open
  (setq dired-dwim-target t))

;;----------------------------------------------------------------------------
;; use visible bell which works in all terminal types
;;----------------------------------------------------------------------------
(defun flash-mode-line ()
  "Flash modeline on bad commands."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))

(setq-default
 ring-bell-function 'flash-mode-line)

;;----------------------------------------------------------------------------
;; increase line spacing
;;----------------------------------------------------------------------------
(setq-default line-spacing 0.2)

;;----------------------------------------------------------------------------
;; set regular font and unicode character font
;;----------------------------------------------------------------------------
(set-fontset-font "fontset-default" 'unicode "operator mono")
(setq default-frame-alist '((font . "operator mono 13")))

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
;; scroll up/down by one line, leaving cursor in place
;; ----------------------------------------------------------------------------
(defun scroll-in-place (scroll-up)
  "Scroll window up (or down) without moving point (if possible).
SCROLL-UP is non-nil to scroll up one line, nil to scroll down."
  (interactive)
  (let ((pos (point))
        (col (current-column))
        (up-or-down (if scroll-up 1 -1)))
    (scroll-up up-or-down)
    (if (pos-visible-in-window-p pos)
        (goto-char pos)
      (if (or (eq last-command 'next-line)
              (eq last-command 'previous-line))
          (move-to-column temporary-goal-column)
        (move-to-column col)
        (setq temporary-goal-column col))
      (setq this-command 'next-line))))

(defun scroll-up-in-place ()
  "Scroll window up without moving point (if possible)."
  (interactive)
  (scroll-in-place t))

(defun scroll-down-in-place ()
  "Scroll window up without moving point (if possible)."
  (interactive)
  (scroll-in-place nil))

(global-set-key (read-kbd-macro "C-s-n") 'scroll-up-in-place)
(global-set-key (read-kbd-macro "C-s-p") 'scroll-down-in-place)

;;----------------------------------------------------------------------------
;; don't use ls command for dired mode
;;----------------------------------------------------------------------------
(when (string= system-type "darwin")
  (defvar dired-use-ls-dired nil))

;;----------------------------------------------------------------------------
;; use conf-unix-mode for default dot files
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.npmrc\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\macos\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.svg$" . xml-mode))

;;----------------------------------------------------------------------------
;; indent after pasting text into emacs
;;----------------------------------------------------------------------------
(defadvice yank (after indent-region activate)
  "Indent text after pasting."
  (indent-region (region-beginning) (region-end) nil))

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
;; delete file and buffer
;;----------------------------------------------------------------------------
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c d f")  'delete-file-and-buffer)

;;----------------------------------------------------------------------------
;; set scratch buffer to js-mode and never kill it
;;----------------------------------------------------------------------------
(setq initial-major-mode 'js-mode)

(defun donot-kill-scratch-buffer ()
  "Don't kill scratch buffer."
  (if (or (equal (buffer-name (current-buffer)) "*scratch*") (equal (buffer-name (current-buffer)) "*Messages*"))
      (progn nil) t))

(add-hook 'kill-buffer-query-functions 'donot-kill-scratch-buffer)

;;----------------------------------------------------------------------------
;; kill all other open buffers except the current one
;; add C-u to kill special buffers too
;;----------------------------------------------------------------------------
(defun kill-other-buffers (&optional *special-buffers)
  "Kill regular and dired buffers, leave current and special buffers.
*SPECIAL-BUFFERS is optional 'universal-argument' invoked using `C-u'
If `universal-argument' is called first, kill special buffers too"
  (interactive "P")
  (if *special-buffers
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
  (progn
    (dolist (buffer (buffer-list))
      (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
        (kill-buffer buffer))))
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list))
  (message "Closed all other buffers"))

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

;;----------------------------------------------------------------------------
;; copy git root path to clipboard.
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

(global-set-key (kbd "C-c c g") 'git-root-path)

;;----------------------------------------------------------------------------
;; copy/paste/clear current line or selected text to register 1
;;----------------------------------------------------------------------------
(defun xah-append-to-register-1 ()
  "Append current line or text selection to register 1.
When no selection, append current line with newline char.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (line-beginning-position))
             (setq $p2 (line-end-position))))
    (append-to-register ?1 $p1 $p2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Appended to register 1: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(defun xah-clear-register-1 ()
  "Clear register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (progn
    (copy-to-register ?1 (point-min) (point-min))
    (message "Cleared register 1.")))

(global-set-key (kbd "C-c c r") 'xah-append-to-register-1)
(global-set-key (kbd "C-c c c") 'xah-clear-register-1)

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
;; ansi-term and bash settings within emacs
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

(global-set-key (kbd "C-c o s") 'ansi-term)

;;----------------------------------------------------------------------------
;; reveal file in finder
;;----------------------------------------------------------------------------
(defun do-applescript+ (&rest scripts)
  "Like `do-applescript', but execute concatenated SCRIPTS.
In case the execution fails, return an error."
  (condition-case err
      (do-applescript
       (apply 'concat scripts))
    (error err)))

(defun show-in-finder (&optional path behind)
  "Display current file/directory in a Finder window.  PATH BEHIND can be passed."
  (interactive)
  (let ((item (or path
                  buffer-file-name
                  (and (eq major-mode 'dired-mode) default-directory))))
    (cond
     ((not (stringp item)))
     ((file-remote-p item)
      (error "This item is located on a remote system"))
     (t
      (setq item (expand-file-name item))
      (do-applescript+
       "tell application \"Finder\" to select (\""
       item
       "\" as POSIX file)\n"
       (unless behind
         "tell application \"Finder\" to activate"))))))

(global-set-key (kbd "C-c o f") 'show-in-finder)

;;----------------------------------------------------------------------------
;; load eslint,tslint from local node_modules when possible
;;----------------------------------------------------------------------------
(defun use-eslint-from-node-modules ()
  "Load eslint from local node_modules if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root (expand-file-name (if (eq system-type 'windows-nt)
                                                 "node_modules/.bin/eslint.cmd"
                                               "node_modules/.bin/eslint")
                                             root))))
    (when (and eslint (file-executable-p eslint))
      (defvar flycheck-javascript-eslint-executable nil)
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)

(defun use-tslint-from-node-modules ()
  "Load tslint from local node_modules if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root (expand-file-name (if (eq system-type 'windows-nt)
                                                 "node_modules/.bin/tslint.cmd"
                                               "node_modules/.bin/tslint")
                                             root))))
    (when (and tslint (file-executable-p tslint))
      (defvar flycheck-typescript-tslint-executable nil)
      (setq-local flycheck-typescript-tslint-executable tslint))))

(add-hook 'flycheck-mode-hook #'use-tslint-from-node-modules)

;;----------------------------------------------------------------------------
;; new line above/below current line with fixed line indentation
;;----------------------------------------------------------------------------
(defun newline-before-the-current-line (&optional *newline-above)
  "Move to end of line, enter a newline, and reindent.
*NEWLINE-ABOVE is optional `universal-argument' to insert new line above"
  (interactive "P")
  (if *newline-above
      (progn
        (unless (bolp)
          (beginning-of-line))
        (let ((col (save-excursion
                     (back-to-indentation)
                     (current-column))))
          (newline)
          (forward-line -1)
          (indent-to-column col)))
    (progn
      (move-end-of-line 1)
      (let ((col (save-excursion
                   (back-to-indentation)
                   (current-column))))
        (newline)
        (indent-to-column col)))))

(global-set-key (kbd "C-<return>") 'newline-before-the-current-line)

;;----------------------------------------------------------------------------
;; new line above current line and indent accordingly
;;----------------------------------------------------------------------------
(defun newline-before-the-current-line-indent ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "S-<return>") 'newline-before-the-current-line-indent)

;;----------------------------------------------------------------------------
;; swap query replace - query replace regexp keybindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;;----------------------------------------------------------------------------
;; M-] to indent and M-[ to unindent
;; numerical argument will indent (arg) * (tab-width) spaces
;;----------------------------------------------------------------------------
(defun keyboard-indent (&optional arg)
  "Unindent line with optional numerical ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun keyboard-unindent (&optional arg)
  "Indent line with optional numerical ARG."
  (interactive "P")
  (keyboard-indent (* -1 (or arg 1))))

(global-set-key (kbd "M-]") 'keyboard-indent)
(global-set-key (kbd "M-[") 'keyboard-unindent)

;;----------------------------------------------------------------------------
;; reload update buffer if changed on disk automatically
;; reload file is updated outside emacs
;;----------------------------------------------------------------------------
(global-auto-revert-mode 1)
(defvar auto-revert-verbose nil)
(setq auto-revert-verbose nil)
(defvar global-auto-revert-non-file-buffers nil)
(setq global-auto-revert-non-file-buffers t)
(global-set-key (kbd "<f5>") 'revert-buffer)

;;----------------------------------------------------------------------------
;; kill back to indentation
;;----------------------------------------------------------------------------
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-S-k") 'kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; kill entire line to indentation
;;----------------------------------------------------------------------------
(defun smart-kill-whole-line (&optional arg)
  "A wrapper around that respects indentation and arguments ARG."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

;;----------------------------------------------------------------------------
;; use package wgrep to edit multiple search results
;;----------------------------------------------------------------------------
(use-package wgrep)

;;----------------------------------------------------------------------------
;; treat all themes as safe
;;----------------------------------------------------------------------------
(setq custom-safe-themes t)

;;----------------------------------------------------------------------------
;; set default color theme
;;----------------------------------------------------------------------------
(use-package spacemacs-theme
  :no-require t
  :config (load-theme 'spacemacs-dark t)
  :custom
  ;; color cursor
  (spacemacs-theme-custom-colors '((cursor . "#ecac2c")))
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-comment-italic t))

;;----------------------------------------------------------------------------
;; Use Ibuffer for Buffer List
;;----------------------------------------------------------------------------
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

;;----------------------------------------------------------------------------
;; smex gets list of recent files, commands as first option
;;----------------------------------------------------------------------------
(use-package smex
  :config
  (setq smex-save-file (locate-user-emacs-file "cache/smex-items")))

;;----------------------------------------------------------------------------
;; save recent files list, exclude .git elpa folders
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
;; remember cursor/point position in buffers
;;----------------------------------------------------------------------------
(use-package saveplace
  :config
  (setq save-place-file (locate-user-emacs-file "cache/saved-places"))
  (save-place-mode))

;;----------------------------------------------------------------------------
;; prompts all available key bindings in a given buffer
;;----------------------------------------------------------------------------
(use-package which-key
  :diminish which-key-mode
  :bind ("C-c h" . which-key-show-top-level)
  :commands which-key-mode
  :init
  (setq which-key-idle-delay 2.0)
  (setq which-key-idle-secondary-delay 1.0)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (add-hook 'after-init-hook #'which-key-mode))

;;----------------------------------------------------------------------------
;; hide show modeu
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
  (eval-after-load "abbrev" '(diminish 'abbrev-mode)))

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
  (add-hook 'org-mode-hook 'highlight-symbol-nav-mode))

;;----------------------------------------------------------------------------
;; iedit to edit sexpressions
;;----------------------------------------------------------------------------
(use-package iedit
  :commands iedit-mode
  :bind ("M-I" . iedit-mode)
  :config (progn
            (setq iedit-log-level 0)
            (define-key iedit-mode-keymap "\C-h" nil)
            (define-key iedit-lib-keymap "\C-s" 'iedit-next-occurrence)
            (define-key iedit-lib-keymap "\C-r" 'iedit-prev-occurrence))
  :init (setq iedit-toggle-key-default nil))

;;----------------------------------------------------------------------------
;; markdown mode
;;----------------------------------------------------------------------------
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'tildify-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode))

;;----------------------------------------------------------------------------
;; load yasnippets
;;----------------------------------------------------------------------------
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (add-hook 'sgml-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'term-mode-hook (lambda()
                              (yas-minor-mode -1)))
  :config
  ;; (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-indent-line 'fixed)
  (yas-reload-all))

;;----------------------------------------------------------------------------
;; set feature mode to edit Gherkin feature files
;; feature-mode needs a hook to diminish orgtbl-mode
;;----------------------------------------------------------------------------
(use-package feature-mode
  :bind (:map feature-mode-map
              ("RET" . newline-and-indent))
  :init
  (add-hook 'feature-mode-hook
            (lambda ()
              (diminish 'orgtbl-mode))))

;;----------------------------------------------------------------------------
;; enable flycheck mode globally
;;----------------------------------------------------------------------------
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(html-tidy javascript-jshint json-python-json)))

;;----------------------------------------------------------------------------
;; enable web beautify mode for js, css, html
;;----------------------------------------------------------------------------
(use-package web-beautify)

;;----------------------------------------------------------------------------
;; use Prettier for JS mode formatting
;; prettier js used to format javascript, useful for react and jsx
;;----------------------------------------------------------------------------
(use-package prettier-js
  ;; :init (add-hook 'js-mode-hook 'prettier-js-mode)
  :diminish prettier-js-mode)

;;----------------------------------------------------------------------------
;; javascript mode hook keybindings
;;----------------------------------------------------------------------------
(add-hook 'js-mode-hook
          (lambda ()
            (defvar js-indent-level nil)
            (setq js-indent-level 2)
            (local-set-key (kbd "C-c e") '(lambda ()  (interactive) (shell-command-on-region (point-min) (point-max) "node")))
            (local-set-key (kbd "C-c C-b") 'sgml-skip-tag-backward)
            (local-set-key (kbd "C-c C-f") 'sgml-skip-tag-forward)
            (local-set-key (kbd "C-c p") 'prettier-js)
            (local-set-key (kbd "C-c b") 'web-beautify-js)
            (local-set-key (kbd "M-j") 'c-indent-new-comment-line)))

;;----------------------------------------------------------------------------
;; set typescript mode
;; use default tsserver that comes bundled with tide
;; run the following in EVAL to find which tsserver.js is being used
;; (tide-locate-tsserver-executable)
;;----------------------------------------------------------------------------
(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (defvar tide-tsserver-executable)
  (defvar tide--tsserver)
  (defvar tide-tsserver-directory)
  (setq tide-tsserver-executable (expand-file-name tide--tsserver tide-tsserver-directory))
  (tide-setup)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  ;; (tide-hl-identifier-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package tide
  :diminish tide-mode
  :config
  (defvar typescript-indent-level nil)
  (setq typescript-indent-level 2)
  (define-key tide-mode-map (kbd "C-c p") 'prettier-js)
  (define-key tide-mode-map (kbd "C-c b") 'tide-format)
  (define-key tide-mode-map (kbd "C-c F") 'tide-fix))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;----------------------------------------------------------------------------
;; use json-mode
;; json mode hook keybindings
;;----------------------------------------------------------------------------
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist `(,(rx ".json" string-end) . json-mode))
  :config
  (define-key json-mode-map (kbd "C-c b") 'json-mode-beautify)
  (define-key json-mode-map (kbd "C-c p") 'prettier-js))

;;----------------------------------------------------------------------------
;; use tagedit for working with html, adds quotes after equals and provides
;; various other helpful html tag editing methods
;; enable tagedit in html mode
;;----------------------------------------------------------------------------
(use-package tagedit
  :init
  (add-hook 'html-mode-hook #'tagedit-mode)
  :diminish tagedit-mode
  :commands tagedit-mode
  :config
  (tagedit-add-paredit-like-keybindings))

;;----------------------------------------------------------------------------
;; html mode hook keybindings
;;----------------------------------------------------------------------------
(add-hook 'sgml-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c b") 'web-beautify-html)))

;;----------------------------------------------------------------------------
;; css mode hook keybindings
;;----------------------------------------------------------------------------
(add-hook 'css-mode-hook
          (lambda ()
            (defvar css-indent-offset nil)
            (setq css-indent-offset 2)
            (local-set-key (kbd "C-c p") 'prettier-js)))

;; ----------------------------------------------------------------------------
;; less mode, and web beautify css
;; ----------------------------------------------------------------------------
(use-package less-css-mode)

;;----------------------------------------------------------------------------
;; add rainbow mode to highlight hex/rgb colors in html, css, sass, js etc
;;----------------------------------------------------------------------------
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (dolist (hook '(css-mode-hook html-mode-hook js-mode-hook emacs-lisp-mode-hook))
    (add-hook hook 'rainbow-mode)))

;;----------------------------------------------------------------------------
;; use diff-hl mode, shows git diff in the gutter
;;----------------------------------------------------------------------------
(use-package diff-hl
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
;; Magit
;;----------------------------------------------------------------------------
(use-package magit
  :config
  (magit-define-popup-switch 'magit-push-popup ?u
    "Set upstream" "--set-upstream")
  (add-hook 'magit-mode-hook 'visual-line-mode)
  :bind ("C-x g" . magit-status))

;;----------------------------------------------------------------------------
;; Ivy
;;----------------------------------------------------------------------------
(use-package ivy
  :ensure ivy-hydra
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done))
  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill))
  :init
  (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  ;; press C-c C-a to show all hidden buffers
  ;; ignoring buffers starting with *
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-format-function #'ivy-format-function-arrow)
  (setq ivy-wrap t)
  (setq ivy-truncate-lines nil)
  (setq ivy-action-wrap t))

;;----------------------------------------------------------------------------
;; Counsel
;;----------------------------------------------------------------------------
(use-package counsel
  :diminish counsel-mode
  :bind (
         ("C-c f" . counsel-git)
         ("C-c s" . counsel-rg)
         ("C-s" . counsel-grep-or-swiper)
         ("C-x r b" . counsel-bookmark)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :commands counsel-mode
  :init
  (add-hook 'after-init-hook #'counsel-mode)
  :config
  ;; ignore dot files from counsel find file to see them press dot
  (setq counsel-find-file-ignore-regexp "\\`\\.")
  (setq counsel-preselect-current-file t)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s"))

;;----------------------------------------------------------------------------
;; use Avy to jump between words in visible buffers
;;----------------------------------------------------------------------------
(use-package avy
  :bind
  ("C-;" . avy-goto-char))

;;----------------------------------------------------------------------------
;; Swiper
;;----------------------------------------------------------------------------
(use-package swiper
  :bind (("C-S-s" . swiper-all))
  :config
  (setq swiper-include-line-number-in-search t))

;;----------------------------------------------------------------------------
;; hydra brings some extra goodness to ivy-buffer with C-o
;;----------------------------------------------------------------------------
(use-package hydra
  :config
  ;; Enable syntax coloring for Hydra definitions
  (hydra-add-font-lock)

  (defhydra hydra-next-previous-buffer
    (global-map "C-x"
                :color red)
    "cycle buffers"
    ("<right>" next-buffer "→ next buffer")
    ("<left>" (lambda () (interactive) (previous-buffer)) "← previous buffer")
    ("q" nil "Quit"))

  (defhydra hydra-fold
    (global-map "C-c @"
                :color red)
    "hide/show"
    ("h" hs-hide-all "hide all")
    ("s" hs-show-all "show all")
    ("l" hs-hide-level "hide same level")
    ("t" hs-toggle-hiding "toggle show hide")
    ("q" nil "Quit")))

;;----------------------------------------------------------------------------
;; Company mode
;;----------------------------------------------------------------------------
(use-package company
  :bind ("C-c t" . company-manual-begin)
  :diminish company-mode
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend))
  (setq company-backends '(company-css
                           company-tide
                           company-capf
                           company-files
                           (company-dabbrev-code company-keywords)
                           company-dabbrev))
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-idle-delay 0.4)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (defvar company-dabbrev-downcase nil)
  (defvar company-dabbrev-other-buffers nil)
  (defvar company-dabbrev-ignore-case nil)
  (setq company-dabbrev-ignore-case t)
  (defvar company-dabbrev-code-everywhere nil)
  (setq company-dabbrev-code-everywhere t)

  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
          (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(defun ora-company-number ()
  "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number
       (if (equal k "0")
           10
         (string-to-number k))))))

;;----------------------------------------------------------------------------
;; use rg frontend for ripgrep search
;;----------------------------------------------------------------------------
(use-package rg
  :config
  (rg-enable-default-bindings (kbd "C-c r"))
  (setq rg-group-result t))

;;----------------------------------------------------------------------------
;; ace-window to easily navigate between frames
;;----------------------------------------------------------------------------
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;----------------------------------------------------------------------------
;; winner mode for saving windows layouts and toggle between them
;; undo and redo window configuration
;;----------------------------------------------------------------------------
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
;; shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(use-package move-dup
  :bind (("C-," . md/move-lines-up)
         ("C-." . md/move-lines-down)
         ("C-c d d" . md/duplicate-down)
         ("C-c d u" . md/duplicate-up)))

;;----------------------------------------------------------------------------
;; github gist
;;----------------------------------------------------------------------------
(use-package gist
  :bind (("C-c g l" . gist-list)
         ("C-c g r" . gist-region-or-buffer-private)
         ("C-c g c" . gist-buffer))
  :init
  (setq gist-ask-for-description t
        gist-ask-for-filename t
        gist-list-format
        '((created "Created" 15 nil
                   "%D %R")
          (visibility "Visibility" 10 nil
                      (lambda (public)
                        (or (and public "public")
                            "private")))
          (description "Description" 30 nil identity)
          (files "Files" 0 nil
                 (lambda (names)
                   (mapconcat 'identity names ", ")))))
  :config
  (setq gist-view-gist t))

;;----------------------------------------------------------------------------
;; cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode t))
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
;; Increase, Decrease font size across all buffers
;; "C-M-=" default-text-scale-increase, "C-M--" default-text-scale-decrease
;;----------------------------------------------------------------------------
(use-package default-text-scale
  :config (default-text-scale-mode))

;;----------------------------------------------------------------------------
;; page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

;;----------------------------------------------------------------------------
;; use evil nerd commenter for comments
;;----------------------------------------------------------------------------
(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

;;----------------------------------------------------------------------------
;; Rainbow Delimiters
;; show matching parens in rainbow colors using rainbow mode
;;----------------------------------------------------------------------------
(use-package rainbow-delimiters
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  js-mode-hook
                  css-mode-hook
                  tide-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;;----------------------------------------------------------------------------
;; experimental settings - try them before adding to init.el
;;----------------------------------------------------------------------------

























;;----------------------------------------------------------------------------
;; end of init settings, provide init
;;----------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here

;;----------------------------------------------------------------------------
;; Try not to add too many custom-set-variables
;;----------------------------------------------------------------------------



;;----------------------------------------------------------------------------
;; custom settings
;;----------------------------------------------------------------------------
