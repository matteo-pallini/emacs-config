; Other custom changes:
; - I disable paredit default bindings because they were conflicting with xref-find-references
; as per https://stackoverflow.com/questions/16605571/why-cant-i-change-paredit-keybindings
; disable it by commenting out line 81 in core/prelude-edior.el

; set custom.el as customizations file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
 (load custom-file))


(require 'package)

;Use the option package-archive-priorities which was made for this very purpose. To prefer MELPA Stable over MELPA, add the following to your init file:
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
	("MARMALADE"    . "http://marmalade-repo.org/packages/"))
      package-archive-priorities
      '(
	("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
	("MARMALADE"    . 3)
	("MELPA"        . 0))
      )

; check if package is installed, and installed it if not. Credit https://stackoverflow.com/questions/31079204/emacs-package-install-script-in-init-file
; define the list of packages we may want to make sure are installed
(setq package-list
      '(use-package yasnippet yasnippet-snippets py-autopep8 magit treemacs-projectile zenburn-theme terraform-mode lsp-ui dash ht))


(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; set threshold for garbage collector to be triggered
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)


; various aesthetic configs
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(setq inhibit-startup-screen t)


(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))


; replace yes and no with y and n in prompts
(fset 'yes-or-no-p 'y-or-n-p)

; reload file automatically if edited outside of emacs
(global-auto-revert-mode t)

; store all backup files in specific directory
(setq backup-directory-alist `(("." . "~/.emacs_backups")))

; show file path in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; set personal theme directory
(add-to-list 'custom-theme-load-path (expand-file-name "themes"
                                                       user-emacs-directory))

(load-theme 'zenburn t)

(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

(use-package direnv
  :config
  (direnv-mode))

; add line on 80th column
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)


;; Python specific configs
;; annoyingly given that python-mode is being reffered to in other snippets
;; we need to define it earlier on here. I am sure there is a way to avoid
;; the code to have a specific order of execution in order to work
(use-package python-mode
  :bind
  (:map python-mode-map
   ("C-c C-p" . run-python)
   ("C-c C-r" . python-shell-send-region)
   ("C-c C-c" . python-shell-send-file))
  :custom
  (python-shell-interpreter "python3")
  )

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  :bind (:map python-mode-map
              ("C-c C-f" . blacken-buffer))
  :init
   (setq-default blacken-line-length 80)
 )

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs/")
  :config
  (pyvenv-mode 1))


(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\'")


(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))


(use-package flycheck
  :diminish flycheck-mode
  :after lsp-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(with-eval-after-load 'lsp-mode
  (defun custom_chain:lsp-next-checker ()
    (flycheck-add-next-checker 'lsp 'python-flake8))
  (add-hook 'flycheck-chain-to-lsp-after-open-hook
            #'custom_chain:lsp-next-checker))

(use-package projectile
  :diminish projectile-mode
  :hook
  (after-init . projectile-mode)
  :init
  (setq projectile-project-search-path '("~/Projects"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
 )


(use-package helm
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
)

; remember to install the specific LSP server for the language of interest
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :hook
  ((sh-mode . lsp-deferred))
  :custom
  (lsp-eldoc-enable-hover t)
  (lsp-enable-symbol-highlighting t)
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-sideline-show-diagnostics t)
  ;(lsp-ui-sideline-show-hover t) ; this is pretty cool, but buggy at the moment
  (lsp-ui-sideline-delay 1)
  :config
  (setq lsp-ui-doc-position 'top)
  :bind
  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ([remap xref-find-references] . lsp-ui-peek-find-references)
  )

(use-package lsp-treemacs
  :after (lsp-mode)
  )

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                  (require 'lsp-pyright)
                  (lsp-deferred)))
  :init
  (setq lsp-pyright-typechecking-mode "basic")
  )

; search for a specific symbol in the all project
(use-package lsp-ivy
  )

(use-package markdown-mode
             :mode ("README\\.md\\'" . gfm-mode)
             :init (setq markdown-command "multimarkdown"))

(use-package yasnippet
  :custom
  (yas-verbosity 2)
  (yas-wrap-around-region t)
  :config
  (yas-reload-all)
  (yas-global-mode))


(use-package yasnippet-snippets
  :after yasnippet)

; avoid conflict with org mode
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))


; remove whitespaces at the end of the line automatically
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use IPython for REPEL
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

; the changes below make white spaces shown when whitespace-mode is enabled
; credit https://emacs.stackexchange.com/questions/9571/enable-whitespace-mode-with-prelude-in-c-mode
;; (add-to-list 'whitespace-style 'space-mark)
;; (add-to-list 'whitespace-style 'tab-mark)
;; (add-to-list 'whitespace-style 'newline-mark)

; define register for quick access to emacs notes
(set-register ?t (cons `file "~/.emacs.d/personal_cheatsheet.txt"))
(set-register ?i (cons `file "~/.emacs.d/init.el"))

; custom keybindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-!") 'treemacs)
(global-set-key (kbd "C-@") 'treemacs-select-window)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
;(global-set-key (kbd "C-M-g") 'elpy-goto-definition)


; mac specific meta remapping
(setq mac-command-modifier 'meta) ; set alt-key to meta

; credit https://github.com/flycheck/flycheck/issues/1762#issuecomment-750458442
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (python-flake8)))))))))

; projectile keybinding
(projectile-mode +1)


; ensure that emacs has the same environment variables of the user's shell
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; (use-package ein
;;   :bind (:map ein:execute
;; 	      ("C-c C-c" . ein:worksheet-execute-cell-and-got-next)))(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(eval-after-load "smartparens-mode"
  '(progn
     (define-key paredit-mode-map (kbd "M-?") nil)
     (global-set-key (kbd "M-?") 'xref-find-references)
     ))

; if the emacs server is not running start it
(require 'server)
(if (not (server-running-p)) (server-start))
