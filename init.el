;; package.el
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; theme
; (load-theme 'cyberpunk t)
; ;; (load-theme 'monokai t)


;; power-line
(powerline-center-theme)


;; yascroll
(global-yascroll-bar-mode)


;; rainbow-delimeter
(rainbow-delimiters-mode t)


;; fix the mac PATH variable
(defun ome-set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "zsh -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (eq system-type 'darwin)
  (when window-system (ome-set-exec-path-from-shell-PATH)))


;; goto-last-change
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;; helm
(require 'helm-config)
(setq helm-input-idle-delay 0.2)
(helm-mode t)
(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)
(global-set-key (kbd "C-x c o") 'helm-occur)
(global-set-key (kbd "C-x C-/") 'helm-find)
(global-set-key (kbd "M-g s") 'helm-do-grep)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)


;; expand-region
(global-set-key (kbd "M-i") 'er/expand-region)
(global-set-key (kbd "M-S-<up>") 'er/mark-inside-quotes)
(global-set-key (kbd "M-S-<down>") 'er/mark-inside-pairs)


;; undo-tree
(global-undo-tree-mode)


;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 2)

;; git-gutter
(setq git-gutter:window-width 2)
(global-git-gutter-mode t)
(setq git-gutter:lighter " G-+")
(setq git-gutter:modified-sign "~ ")
(setq git-gutter:added-sign "+ ")
(setq git-gutter:deleted-sign "- ")
(setq git-gutter:unchanged-sign nil)
(global-set-key (kbd "C-c n")
                'git-gutter:next-hunk)
(global-set-key (kbd "C-c l")
                'git-gutter:previous-hunk)
(global-set-key (kbd "C-c h")
                'git-gutter:stage-hunk)


; ;; window-number
(autoload 'window-number-mode "window-number" t)
(window-number-mode 1)
(autoload 'window-number-meta-mode
  "window-number" t)
(window-number-meta-mode 1)

; ;; elisp
; (defun ome-remove-elc-on-save ()
;   "If you're saving an elisp file, likely the .elc is no longer valid."
;   (make-local-variable 'after-save-hook)
;   (add-hook 'after-save-hook
;             (lambda ()
;               (if (file-exists-p (concat buffer-file-name "c"))
;                   (delete-file (concat buffer-file-name "c"))))))

; (add-hook 'emacs-lisp-mode-hook 'ome-remove-elc-on-save)

; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
; (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

; ;; elisp-slime-nav
; (defun ome-elisp-slime-nav-setup ()
;   (dolist (hook '(emacs-lisp-mode-hook
;                   lisp-interaction-mode-hook
;                   ielm-mode-hook
;                   eshell-mode-hook))
;     (add-hook hook 'turn-on-elisp-slime-nav-mode)))

; (add-to-list 'el-get-sources
;              '(:name elisp-slime-nav
;                      :after (progn
;                               (ome-elisp-slime-nav-setup))))

; ;; ace-jump-mode
(require 'ace-jump-mode)
(setq ace-jump-mode-submode-list
      '(ace-jump-char-mode
        ace-jump-line-mode))
(global-set-key (kbd "C-o") 'ace-jump-mode)
(ace-jump-mode-enable-mark-sync)


; ;; multiple-cursors
; (add-to-list 'el-get-sources
;              '(:name multiple-cursors
;                      :after (progn
;                               (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;                               (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;                               (global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)
;                               (global-set-key (kbd "C-c e l")
;                                               'mc/edit-lines))))




;;; Customized configuration

 ;; mac specific settings

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; key-bindings

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "M-h") 'help-command)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

; (global-set-key (kbd "C-c b") 'winner-undo)
; (global-set-key (kbd "C-c f") 'winner-redo)

; ;;; language specific setting

;;emacs-lisp shortcuts
(global-set-key (kbd "C-c C-b") 'eval-buffer)
(global-set-key (kbd "C-c C-p") 'eval-print-last-sexp)
(global-set-key (kbd "C-c C-r") 'eval-region)
(global-set-key (kbd "C-j") 'eval-last-sexp)


;;fast vertical naviation
(global-set-key  (kbd "M-U") (lambda () (interactive) (forward-line -10)))
(global-set-key  (kbd "M-D") (lambda () (interactive) (forward-line 10)))
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

;; Dired
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-M-down>") 'move-line-down)
(global-set-key (kbd "<C-M-up>") 'move-line-up)

;;mimic vim
;;Turn off cua mode first
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

; (defun live-show-messages ()
;   (interactive)
;   (popwin:display-buffer "*Messages*"))

; (global-set-key (kbd "C-c s m") 'live-show-messages)

; ;;scroll other window
; (global-set-key (kbd "C-M-]") 'scroll-other-window)
; (global-set-key (kbd "C-M-[") 'scroll-other-window-down)

; (global-set-key (kbd "M-'") 'repeat)
; (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
; (global-set-key (kbd "C-c y") 'bury-buffer)
; (global-set-key (kbd "C-x C-b") 'ibuffer)


(defun live-lisp-describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym)))))

(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)

; ;;imenu
; (global-set-key (kbd "C-.") 'imenu)

; ;;bookmark
; (global-set-key (kbd "<f8>") 'bookmark-set)
; (global-set-key (kbd "<f9>") 'bookmark-jump)
; (global-set-key (kbd "<f7>") 'bookmark-bmenu-list)

; ;;idomenu
; (global-set-key (kbd "C-c M-b") 'find-file-at-point)

; ;;candidates
; (global-set-key (kbd "C-c j p") 'quick-jump-go-back)
; (global-set-key (kbd "C-c j b") 'quick-jump-go-back)
; (global-set-key (kbd "C-c j m") 'quick-jump-push-marker)
; (global-set-key (kbd "C-c j n") 'quick-jump-go-forward)
; (global-set-key (kbd "C-c j f") 'quick-jump-go-forward)
; (global-set-key (kbd "C-c j c") 'quick-jump-clear-all-marker)

; (global-set-key (kbd "C-c d f") 'diff-buffer-with-file)



;; misc
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory ".saved-places"))
(require 'saveplace)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(require 'uniquify)

;; use aspell instead of ispell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(set-language-environment "UTF-8")
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(setq ring-bell-function 'ignore)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; enable to support navigate in camelCase words
(global-subword-mode t)
; ;; hide startup splash screen
; (setq inhibit-startup-screen t)

(setq-default major-mode 'text-mode)

; ;;; ido-mode
; (setq ido-enable-prefix nil)
; (setq ido-enable-case nil)
; (setq ido-enable-flex-matching t)
; (setq ido-everywhere t)
; (ido-mode t)

; ;; use icomplete in minibuffer
; (icomplete-mode t)
; (delete-selection-mode t)

; ;; Auto refresh buffers
; (global-auto-revert-mode 1)

; ;; Also auto refresh dired, but be quiet about it
; (setq global-auto-revert-non-file-buffers t)
; (setq auto-revert-verbose nil)



;;; GUI

;; popwin
(require 'popwin)
(popwin-mode 1)
(push '("*Messages*" :noselect t :height 30)
      popwin:special-display-config)

;; remove useless gui
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; show parenthesis match
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(global-hl-line-mode t)

;; auto-fill mode
(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; font
(if (member "Monaco" (font-family-list))
    (set-face-attribute
     'default nil :font "Monaco 13"))


;; mode-hooks
;; outline-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (outline-minor-mode t)))

; ;; winner undo and redo
; (when (fboundp 'winner-mode)
;   (winner-mode 1))

;; primitive backup-dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; ;; eshell
; (add-hook 'eshell-mode-hook
;           (lambda ()
;             (add-to-list 'ac-sources 'ac-source-pcomplete)))

; (add-to-list 'ac-modes 'eshell-mode)
; (add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)
; (add-hook 'eshell-mode-hook 'ac-emacs-lisp-mode-setup)

; ;;; shell-mode settings

; (unless (eq system-type 'windows-nt)
;   (setq explicit-shell-file-name "/bin/bash")
;   (setq shell-file-name "/bin/bash"))
; ;; always insert at the bottom
; (setq comint-scroll-to-bottom-on-input t)
; (setq comint-input-ignoredups t)
; ; what to run when press enter on a line above the current prompt
; (setq comint-get-old-input (lambda () ""))
; ;; set lang to enable Chinese display in shell-mode
; (setenv "LANG" "en_US.UTF-8")

; ;; ido resentf
; (defun ido-recentf-open ()
;   "Use `ido-completing-read' to \\[find-file] a recent file"
;   (interactive)
;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;       (message "Opening file...")
;     (message "Aborting")))

; (setq recentf-save-file (concat user-emacs-directory ".recentf"))
; (recentf-mode t)
; (setq recentf-max-saved-items 50)


(setq sgml-basic-offset 4)

; ;;; flycheck
; (defun flycheck-setup ()
;   (eval-after-load 'flycheck
;     '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
;   (add-hook 'prog-mode-hook 'flycheck-mode))

; (flycheck-setup)

; (require 'dired-x)
; (setq-default dired-omit-files-p t)
; (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))


(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(global-set-key (kbd "M-k") 'backward-kill-line)

; (set-default-font "Source Code Pro-14")
; (set-fontset-font t 'han (font-spec :family "Hiragino Sans GB" :size 16))
                                        ; (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/moe-theme-20151124.1509/")
(setq ad-redefinition-action 'accept)
(load-theme 'moe-dark t)

(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
