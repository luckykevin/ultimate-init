(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

(setq el-get-user-package-directory "~/.emacs.d/config/")

;; package.el
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; el-get sources
(setq el-get-sources
      '((:name expand-region
               :after (progn
                        (global-set-key (kbd "M-i") 'er/expand-region)
                        (global-set-key (kbd "M-S-<up>") 'er/mark-inside-pairs)
                        (global-set-key (kbd "M-S-<down>") 'er/mark-outside-pairs)
                        (global-set-key (kbd "C-c e q") 'er/mark-inside-quotes)))
        (:name powerline
               :after (powerline-center-theme))
        (:name rainbow-delimiters
               :after (global-rainbow-delimiters-mode))
        (:name yascroll
               :after (global-yascroll-bar-mode 1))
        (:name undo-tree
               :after (global-undo-tree-mode))
        (:name markdown-mode
               :before (add-to-list 'auto-mode-alist
                                    '("\\.mdpp" . markdown-mode)))
        (:name find-file-in-project
               :after (global-set-key (kbd "C-x C-M-f") 'find-file-in-project))
        (:name exec-path-from-shell
               :after (when (memq window-system '(mac ns))
                        (exec-path-from-shell-initialize)))
        (:name goto-last-change
               :after (global-set-key (kbd "C-x C-\\") 'goto-last-change))
        (:name volatile-highlights
               :after (volatile-highlights-mode t))
        (:name popwin
               :after (setq display-buffer-function 'popwin:display-buffer))
        (:name projectile
               :after (progn
                        (projectile-global-mode)
                        (setq projectile-enable-caching t)
                        (global-set-key (kbd "C-x c h")
                                        'helm-projectile)))
        (:name midje-mode
               :after (require 'clojure-jump-to-file))))


;; clojure
(add-to-list 'el-get-sources
             '(:name clojure-mode
                     (progn
                       (define-key clojure-test-mode-map (kbd "M-n") 'outline-next-visible-heading)
                       (define-key clojure-test-mode-map (kbd "M-p") 'outline-previous-visible-heading)
                       (define-key clojure-mode-map (kbd "C-j")
                         'cider-eval-last-expression)
                       (define-xkey clojure-mode-map (kbd "C-S-j")
                         'cider-eval-print-last-expression)
                       (define-key clojure-mode-map (kbd "C-c C-b") 'nrepl-eval-buffer))))

;; cider
(defun cider-setup ()
  (add-hook 'cider-repl-mode-hook
            'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook
            'cider-turn-on-eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-auto-select-error-buffer t)
  (setq nrepl-buffer-name-separator "-")
  (setq nrepl-buffer-name-show-port t)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-wrap-history t)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))

;; ac-nrepl
(defun ac-nrepl-config()
  (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-repl-mode)))

(add-hook 'after-init-hook 'cider-setup)
(add-hook 'after-init-hook 'ac-nrepl-config)


;; fix the mac PATH variable
(defun ome-set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "zsh -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (eq system-type 'darwin)
  (when window-system (ome-set-exec-path-from-shell-PATH)))

(defun ome-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun ome-smartparens-setup ()
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-navigate-close-if-unbalanced t)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  ;; keybinding management
  (define-key sp-keymap (kbd "M-s f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "M-s b") 'sp-backward-sexp)

  (define-key sp-keymap (kbd "M-s d") 'sp-down-sexp)
  (define-key sp-keymap (kbd "M-s D") 'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "M-s a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "M-s e") 'sp-end-of-sexp)

  (define-key sp-keymap (kbd "M-s u") 'sp-up-sexp)
  ;; (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  (define-key sp-keymap (kbd "M-s U") 'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "M-s t") 'sp-transpose-sexp)

  (define-key sp-keymap (kbd "M-s n") 'sp-next-sexp)
  (define-key sp-keymap (kbd "M-s p") 'sp-previous-sexp)

  (define-key sp-keymap (kbd "M-s k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "M-s w") 'sp-copy-sexp)

  (define-key sp-keymap (kbd "M-s s") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "M-s r") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "M-s S") 'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "M-s R") 'sp-backward-barf-sexp)
  (define-key sp-keymap (kbd "M-s F") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-s B") 'sp-backward-symbol)

  (define-key sp-keymap (kbd "M-s [") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "M-s ]") 'sp-select-next-thing)

  (define-key sp-keymap (kbd "M-s M-i") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "M-s <delete>") 'sp-splice-sexp-killing-forward)
  (define-key sp-keymap (kbd "M-s <backspace>") 'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "M-s M-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key sp-keymap (kbd "M-s M-d") 'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "M-s M-b") 'sp-backward-unwrap-sexp)

  (define-key sp-keymap (kbd "M-s M-t") 'sp-prefix-tag-object)
  (define-key sp-keymap (kbd "M-s M-p") 'sp-prefix-pair-object)
  (define-key sp-keymap (kbd "M-s M-c") 'sp-convolute-sexp)
  (define-key sp-keymap (kbd "M-s M-a") 'sp-absorb-sexp)
  (define-key sp-keymap (kbd "M-s M-e") 'sp-emit-sexp)
  (define-key sp-keymap (kbd "M-s M-p") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "M-s M-n") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "M-s M-j") 'sp-join-sexp)
  (define-key sp-keymap (kbd "M-s M-s") 'sp-split-sexp)
  (define-key sp-keymap (kbd "M-s M-r") 'sp-raise-sexp)

  ;; pair management
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  ;; markdown-mode
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  ;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))

  ;; html-mode
  (sp-with-modes '(html-mode sgml-mode)
    (sp-local-pair "<" ">"))

  ;; lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :bind "C-("))

  (dolist (mode '(c-mode c++-mode java-mode js2-mode sh-mode))
    (sp-local-pair mode
                   "{"
                   nil
                   :post-handlers
                   '((ome-create-newline-and-enter-sexp "RET")))))

;; smartparens
(add-to-list 'el-get-sources
             '(:name smartparens
                     :after (progn
                              (ome-smartparens-setup)
                              (setq sp-navigate-consider-symbols nil))))

;; helm
(add-to-list 'el-get-sources
             '(:name helm
                     :after (progn
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
                              (global-set-key (kbd "C-x C-r") 'helm-recentf))))


;; auto-complete
(add-to-list 'el-get-sources
             '(:name auto-complete
                     :after (progn
                              (require 'auto-complete-config)
                              (ac-config-default)
                              (global-auto-complete-mode t)
                              (setq ac-dwim t)
                              (setq ac-use-menu-map t)
                              (setq ac-quick-help-delay 1)
                              (setq ac-quick-help-height 60)
                              (setq ac-auto-start 2)
                              (setq ac-candidate-menu-min 2))))

(load-theme 'cyberpunk t)

;; window-number
(add-to-list 'el-get-sources
             '(:name window-number
                     :after (progn
                              (autoload 'window-number-mode "window-number" t)
                              (window-number-mode 1)
                              (autoload 'window-number-meta-mode
                                "window-number" t)
                              (window-number-meta-mode 1))))

;; elisp
(defun ome-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'ome-remove-elc-on-save)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; elisp-slime-nav
(defun ome-elisp-slime-nav-setup ()
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook
                  eshell-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(add-to-list 'el-get-sources
             '(:name elisp-slime-nav
                     :after (progn
                              (ome-elisp-slime-nav-setup))))

;; ace-jump-mode
(add-to-list 'el-get-sources
             '(:name ace-jump-mode
                     :after (progn
                              (setq ace-jump-mode-submode-list
                                    '(ace-jump-char-mode
                                      ace-jump-line-mode))
                              (global-set-key (kbd "C-o") 'ace-jump-mode)
                              (ace-jump-mode-enable-mark-sync)
                              (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark))))

;; key-chord
(add-to-list 'el-get-sources
             '(:name key-chord
                     :after (progn
                              (key-chord-mode 1)
                              (key-chord-define-global "jk"     'undo)
                              (key-chord-define-global ",."     "<>\C-b")
                              (key-chord-define-global ",,"     'indent-for-comment)
                              (key-chord-define-global "qq"     "the ")
                              (key-chord-define-global "QQ"     "The "))))

(add-to-list 'el-get-sources
             '(:name git-gutter
                     :after (progn
                              (setq git-gutter:window-width 2)
                              (global-git-gutter-mode t)
                              (setq git-gutter:lighter " G-+")
                              (setq git-gutter:modified-sign "~ ")
                              (setq git-gutter:added-sign "+ ")
                              (setq git-gutter:deleted-sign "- ")
                              (setq git-gutter:unchanged-sign nil)
                              (global-set-key (kbd "C-c n") 'git-gutter:next-hunk)
                              (global-set-key (kbd "C-c l") 'git-gutter:previous-hunk))))

;; multiple-cursors
(add-to-list 'el-get-sources
             '(:name multiple-cursors
                     :after (progn
                              (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                              (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                              (global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)
                              (global-set-key (kbd "C-c e l")
                                              'mc/edit-lines))))



(eval-after-load 'popup
  '(progn
     (define-key popup-menu-keymap (kbd "C-n") 'popup-next)
     (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
     (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
     (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
     (define-key popup-menu-keymap (kbd "C-p") 'popup-previous)))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))

;; yasnippet
(add-to-list 'el-get-sources
             '(:name yasnippet
                     :depends (popup)
                     :after (progn
                              (setq yas-prompt-functions
                                    '(yas-popup-isearch-prompt
                                      yas-no-prompt))
                              (yas-reload-all)
                              (add-hook 'prog-mode-hook
                                        '(lambda ()
                                           (yas-minor-mode))))))


(when (or (executable-find "ack") (executable-find "ack-grep"))
  (add-to-list 'el-get-sources
               '(:name ack-and-a-half)))

(setq my-packages
      (append
       '(smooth-scrolling quickrun quick-jump scratch)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
;; el-get ends


;; Customized configuration

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

(global-set-key (kbd "C-c b") 'winner-undo)
(global-set-key (kbd "C-c f") 'winner-redo)

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

(defun live-show-messages ()
  (interactive)
  (popwin:display-buffer "*Messages*"))

(global-set-key (kbd "C-c s m") 'live-show-messages)

;;scroll other window
(global-set-key (kbd "C-M-]") 'scroll-other-window)
(global-set-key (kbd "C-M-[") 'scroll-other-window-down)

(global-set-key (kbd "M-'") 'repeat)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;el-get
(global-set-key (kbd "C-c e i") 'el-get-install)
(global-set-key (kbd "C-c e d") 'el-get-describe)

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

;;imenu
(global-set-key (kbd "C-.") 'imenu)

;;bookmark
(global-set-key (kbd "<f8>") 'bookmark-set)
(global-set-key (kbd "<f9>") 'bookmark-jump)
(global-set-key (kbd "<f7>") 'bookmark-bmenu-list)

;;idomenu
(global-set-key (kbd "C-c M-b") 'find-file-at-point)

;;candidates
(global-set-key (kbd "C-c j p") 'quick-jump-go-back)
(global-set-key (kbd "C-c j b") 'quick-jump-go-back)
(global-set-key (kbd "C-c j m") 'quick-jump-push-marker)
(global-set-key (kbd "C-c j n") 'quick-jump-go-forward)
(global-set-key (kbd "C-c j f") 'quick-jump-go-forward)
(global-set-key (kbd "C-c j c") 'quick-jump-clear-all-marker)

(global-set-key (kbd "C-c d f") 'diff-buffer-with-file)



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
;; hide startup splash screen
(setq inhibit-startup-screen t)

(setq-default major-mode 'text-mode)

;;; ido-mode
(setq ido-enable-prefix nil)
(setq ido-enable-case nil)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; use icomplete in minibuffer
(icomplete-mode t)
(delete-selection-mode t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;; GUI
(setq popwin:special-display-config
      '(("*Help*"  :height 30)
        ("*Completions*" :noselect t)
        ("*Messages*" :noselect t :height 30)
        ("*Apropos*" :noselect t :height 30)
        ("*compilation*" :noselect t)
        ("*Backtrace*" :height 30)
        ("*Messages*" :height 30)
        ("*Occur*" :noselect t)
        ("*Ido Completions*" :noselect t :height 30)
        ("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
        ("*magit-diff*" :noselect t :height 40 :width 80)
        ("*magit-edit-log*" :noselect t :height 15 :width 80)
        ("\\*ansi-term\\*.*" :regexp t :height 30)
        ("*shell*" :height 30)
        (".*overtone.log" :regexp t :height 30)
        ("*gists*" :height 30)
        ("*sldb.*":regexp t :height 30)
        ;; ("*nrepl-error*" :height 30 :stick t)
        ;; ("*nrepl-doc*" :height 30 :stick t)
        ;; ("*nrepl-src*" :height 30 :stick t)
        ;; ("*nrepl-result*" :height 30 :stick t)
        ;; ("*nrepl-macroexpansion*" :height 30 :stick t)
        ("*Kill Ring*" :height 30)
        ("*Compile-Log*" :height 30 :stick t)
        ("*git-gutter:diff*" :height 30 :stick t)))

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

;; winner undo and redo
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; primitive backup-dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-pcomplete)))

(add-to-list 'ac-modes 'eshell-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'ac-emacs-lisp-mode-setup)

;; ido resentf
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode t)
(setq recentf-max-saved-items 50)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq sgml-basic-offset 4)
