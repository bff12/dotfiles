(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
(setq debug-on-error t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auth-sources '("~/.authinfo"))
;; gpg it will ask you directly in the minibuffer at the bottom of your screen.
(setq epa-pinentry-mode 'loopback)
(setq org-startup-folded 'overview)
(setq initial-major-mode 'org-mode)
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'nil)
;; (setq pdf-view-continuous nil)
(setq ispell-dictionary "en")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq org-html-inline-images t)
(setq org-html-inline-image-rules
      '(("file" . "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)\\'")))
(setq ispell-program-name "/opt/homebrew/bin/aspell")
(setq ring-bell-function 'ignore)
(setq org-clock-sound "~/Downloads/bell.wav")
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq use-package-always-ensure t)
(set-face-attribute 'default nil :font "IBM Plex Mono" :height 150)
(setq message-kill-buffer-on-exit t)
(setq company-tooltip-align-annotations t)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)
(setq org-hide-emphasis-markers t)
(setq prettier-js-args '(
                         "--single-quote" "true"
                         "--arrow-parens" "avoid"
                         ;; "--tab-width" "2"
                         ))


(winner-mode 1)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(fset 'yes-or-no-p '
      y-or-n-p)

(global-auto-revert-mode t)

(delete-selection-mode 1)

(set-face-foreground 'highlight nil)     ;;

(put 'narrow-to-region 'disabled nil)

'(forge-topic-list-limit '(60 . 60))

'(org-agenda-files
  '("/Users/brenofarias/Documents/agenda.org"))

'(org-safe-remote-resources
  '("\\`https://fniessen\\.github\\.io/org-html-themes/org/readtheorg\\.setup\\'"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(global-visual-line-mode t)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)


(global-set-key (kbd "s-k") 'previous-line)
(global-set-key (kbd "s-j") 'next-line)
(global-set-key (kbd "s-h") 'left-char)
(global-set-key (kbd "s-l") 'right-char)
(global-set-key (kbd "<backtab>") 'hs-toggle-hiding)
(global-set-key (kbd "M-s-[") 'winner-undo)
(global-set-key (kbd "M-s-]") 'winner-redo)
(global-set-key (kbd "M-m") 'toggle-frame-maximized)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-<return>") 'smart-open-line)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "s-[") 'sp-unwrap-sexp)
(global-set-key (kbd "s-]") 'sp-rewrap-sexp)
(global-set-key (kbd "s-n") 'my-scratch-buffer)
(global-set-key (kbd "s-t") 'my-vterm-custom-buffer-name)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


(defun smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))


(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))


(defun scroll-pdf ()
  "Auto Scroll PDF."
  (interactive)
  (run-with-timer 0 (* 0.8) 'pdf-view-next-line-or-next-page 1))

(defun my-vterm-custom-buffer-name (name)
  "Open a new vterm buffer with a specified NAME."
  (interactive "sEnter buffer name: ")
  (let ((buffer (get-buffer-create (concat "*VTERM-" name "*"))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))
    (switch-to-buffer buffer)))

(defun my-scratch-buffer ()
  (interactive)
  (let ((n 0)
        bufname buffer)
    (catch 'done
      (while t
        (setq bufname (concat "*hello-world"
                              (if (= n 0) "" (int-to-string n))
                              "*"))
        (setq n (1+ n))
        (when (not (get-buffer bufname))
          (setq buffer (get-buffer-create bufname))
          (with-current-buffer buffer
            (org-mode))
          (throw 'done (display-buffer buffer t))) ))))


(defun my/set-local-eslint ()
  (setq-local flycheck-javascript-eslint-executable (executable-find "eslint")))


(defun my-counsel-rg-ignore-extensions ()
  "Run `counsel-rg` ignoring certain file extensions."
  (interactive)
  (let ((counsel-rg-base-command
         "rg -S -w -M 200 --no-heading --line-number --color never --glob '!*.json' --glob '!*.md' --glob '!*.snap' --glob '!*.svg' %s"))
    (counsel-projectile-rg)))



(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


(use-package tide :ensure t)

(use-package diminish)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package expand-region
  :config
  (global-set-key (kbd "s-'") 'er/expand-region)
  (global-set-key (kbd "s-\"") 'er/contract-region))

(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)
  (global-set-key (kbd "M-s-d") 'mc/edit-beginnings-of-lines)
  (define-key mc/keymap (kbd "<return>") nil))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))

  (global-set-key (kbd "s-b") 'ivy-switch-buffer)
  (global-set-key (kbd "M-s-b") 'ivy-resume))

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "s-o") 'counsel-find-file))

(use-package smex)
(use-package flx)
(use-package avy)

(use-package hide-mode-line
  :ensure t)


(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper))


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )

(use-package exec-path-from-shell    ;;
  :ensure t)                        ;;
(when (memq window-system '(mac ns)) ;;
  (exec-path-from-shell-initialize))   ;;

(use-package vterm
  :ensure t)

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))


(use-package yaml-mode)
(use-package haml-mode)
(use-package markdown-mode)

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode)))


(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package move-text
  :config
  (move-text-default-bindings))


(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(pdf-tools-install)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))


(use-package prettier-js
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/BreNotes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package visual-regexp
  :config
  (define-key global-map (kbd "M-s-f") 'vr/replace)
  (define-key global-map (kbd "s-r") 'vr/replace))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1)
  (global-set-key (kbd "s-F") 'my-counsel-rg-ignore-extensions))

(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-universal-key "s-=")))

(use-package visual-regexp
  :config
  (define-key global-map (kbd "M-s-f") 'vr/replace)
  (define-key global-map (kbd "s-r") 'vr/replace))

(global-auto-revert-mode t)

(use-package magit
  :config
  (global-set-key (kbd "s-g") 'magit-status))

(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist '("xxx.xx.com" "github.xxx.com/api/v3"
                              "github.xxx.com" forge-github-repository)))

(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode 't)
  (set-face-background 'git-gutter:modified 'nil)
  (set-face-foreground 'git-gutter:added "green4")
  (set-face-foreground 'git-gutter:deleted "red"))

(use-package plantuml-mode
  :init
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/temp/plantuml-1.2022.8.jar")
  (setq org-plantuml-jar-path (expand-file-name "~/temp/plantuml-1.2022.8.jar"))
  (setq org-startup-with-inline-images t)
  (with-eval-after-load "org"
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
                                                           (js . t))))

( use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  )  (evil-mode)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil))

(define-key evil-normal-state-map "q" nil)

(evil-define-operator evil-change-without-register (beg end type yank-handler)
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler))
(define-key evil-normal-state-map (kbd "c") 'evil-change-without-register)

(evil-define-operator evil-delete-without-register (beg end type yank-handler)
  (interactive "<R><y>")
  (evil-delete beg end type ?_ yank-handler))
(define-key evil-normal-state-map (kbd "d") 'evil-delete-without-register)
(define-key evil-visual-state-map (kbd "d") 'evil-delete-without-register)
(define-key evil-normal-state-map (kbd "D") 'evil-delete)
(define-key evil-visual-state-map (kbd "D") 'evil-delete)

(evil-define-key 'normal 'global
  "qq" "$V%")

(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-undo-system 'undo-tree)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary)

(evil-commentary-mode)

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

(use-package add-node-modules-path)

(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook #'add-node-modules-path)
     (add-hook 'web-mode-hook #'prettier-js-mode)))

(use-package flycheck
  :ensure t
  :hook (web-mode . my/set-local-eslint)
  :init
  (global-flycheck-mode 1)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package restclient)

(use-package ob-restclient)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))
