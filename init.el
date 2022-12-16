(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq auth-sources '("~/.authinfo"))

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq initial-major-mode 'org-mode)

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(setq pdf-view-continuous nil)

(setq ring-bell-function 'ignore)

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
(require 'bind-key)
(setq use-package-always-ensure t)

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

(setq inhibit-startup-screen t)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)


(global-set-key (kbd "s-k") 'previous-line)
(global-set-key (kbd "s-j") 'next-line)
(global-set-key (kbd "s-h") 'left-char)
(global-set-key (kbd "s-l") 'right-char)

(set-face-attribute 'default nil :font "CamingoCode-15")

(winner-mode 1)
(global-set-key (kbd "M-s-[") 'winner-undo)
(global-set-key (kbd "M-s-]") 'winner-redo)

(use-package diminish)

(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/")

(use-package smtpmail)

;; we installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu"))

;; this is the directory we created before:
(setq mu4e-maildir "~/.maildir")

;; this command is called to sync imap servers:
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
;; how often to call it in seconds:
(setq mu4e-update-interval nil)

;; save attachment to desktop by default
;; or another choice of yours:
(setq mu4e-attachment-dir "~/Desktop")

;; rename files when moving - needed for mbsync:
(setq mu4e-change-filenames-when-moving t)

;; list of your email adresses:
(setq mu4e-user-mail-address-list '("brenofarias87@yahoo.com.br"))

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "yahoo"
          :enter-func
          (lambda () (mu4e-message "Enter brenofarias87@yahoo.com.br context"))
          :leave-func
          (lambda () (mu4e-message "Leave brenofarias87@yahoo.com.br context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "brenofarias87@yahoo.com.br")))
          :vars '((user-mail-address . "brenofarias87@yahoo.com.br" )
                  (user-full-name . "Breno Farias")
                  (mu4e-drafts-folder . "/yahoo/Drafts")
                  (mu4e-refile-folder . "/yahoo/Archive")
                  (mu4e-sent-folder . "/yahoo/Sent Messages")
                  (mu4e-trash-folder . "/yahoo/Deleted Messages")))))

(setq message-kill-buffer-on-exit t)

(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

(setq sendmail-program (executable-find "msmtp"))

(setq message-sendmail-envelope-from 'header)


(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode 1))


(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)

(global-set-key (kbd "s-a") 'mark-whole-buffer)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(fset 'yes-or-no-p '
      y-or-n-p)

(global-auto-revert-mode t)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

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

(defun my/org-tree-slide-setup ()
  (org-display-inline-images)
  (hide-mode-line-mode 1))

(defun my/org-tree-slide-end ()
  (org-display-inline-images)
  (hide-mode-line-mode 0))

(use-package org-tree-slide
  :ensure t
  :defer t
  :custom
  (org-image-actual-width nil)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  :hook ((org-tree-slide-play . my/org-tree-slide-setup)
         (org-tree-slide-stop . my/org-tree-slide-end)))


(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
  )

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "s-f") 'swiper))

(delete-selection-mode 1)

(global-set-key (kbd "s-<right>") (kbd "C-e"))
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e"))
(global-set-key (kbd "s-<left>") (kbd "M-m"))
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

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

(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))


(defun scroll-pdf ()
  "Auto Scroll PDF."
  (interactive)
  (run-with-timer 0 (* 0.8) 'pdf-view-next-line-or-next-page 1))

(use-package yaml-mode)
(use-package haml-mode)
(use-package markdown-mode)

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
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

(global-set-key (kbd "s-<return>") 'smart-open-line)


(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(pdf-tools-install)

(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))


(setq org-hide-emphasis-markers t)

(use-package prettier-js
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))


;; (setq prettier-js-args '(
;;                          "--arrow-parens" "avoid"
;;                          ))

(setq org-emphasis-alist
      '(("*" (bold :slant italic :weight black :foreground "#000000" :background "#FCFF03" ))
        ("/" (italic :foreground "#F3CA40" ))
        ("_" (:underline t  ))
        ("=" (:foreground "#E74C3C"))
        ("~" (:foreground "#53df83" ))
        ("+" (:strike-through nil :foreground "#FFC300" ))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/BreNotes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(global-set-key "\C-ca" 'org-agenda)

(global-set-key (kbd "s-[") 'sp-unwrap-sexp)
(global-set-key (kbd "s-]") 'sp-rewrap-sexp)

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (vterm 'N))

(global-set-key (kbd "s-t") 'eshell-new)

(use-package visual-regexp
  :config
  (define-key global-map (kbd "M-s-f") 'vr/replace)
  (define-key global-map (kbd "s-r") 'vr/replace))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1)
  (global-set-key (kbd "s-F") 'counsel-projectile-rg) )

(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-universal-key "s-=")))

(use-package elfeed
  :ensure t
  :commands (elfeed))

(use-package elfeed-protocol
  :ensure t
  :demand t
  :after elfeed
  :config
  (elfeed-protocol-enable))

(use-package password-store
  :ensure t
  :demand t
  :after elfeed-protocol)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '("https://cprss.s3.amazonaws.com/javascriptweekly.com.xml"
        "https://css-tricks.com/feed/"
        ;; "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
        "https://www.smashingmagazine.com/feed"
        ))

(use-package visual-regexp
  :config
  (define-key global-map (kbd "M-s-f") 'vr/replace)
  (define-key global-map (kbd "s-r") 'vr/replace))

(global-auto-revert-mode t)

;; Magit
(use-package magit
  :config
  (global-set-key (kbd "s-g") 'magit-status))

(use-package forge
  :after magit)

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
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

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

(global-set-key (kbd "s-n") 'my-scratch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (unless (and (fboundp 'play-sound-internal)                  ;;
;;              (subrp (symbol-function 'play-sound-internal))) ;;
;;   (require 'play-sound))                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (set-face-background 'hl-line "#3e4446") ;;
(set-face-foreground 'highlight nil)     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (savehist-mode)
;; (setq-default line-spacing 2)

;; (setq org-clock-sound t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook                                                           ;; ;;;   ;;
;;  'after-init-hook                                                   ;; ;; ;; ;;
;;  (lambda ()                                                         ;; ;; ;; ;;
;;    (let ((private-file (concat user-emacs-directory "private.el"))) ;; ;; ;; ;;
;;      (when (file-exists-p private-file)                             ;; ;; ;; ;;
;;        (load-file private-file)))))                                 ;; ;; ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package modus-vivendi-theme
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

;; (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)

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

(use-package add-node-modules-path)

(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook #'add-node-modules-path)
     (add-hook 'web-mode-hook #'prettier-js-mode)))


(defun my/set-local-eslint ()
  (setq-local flycheck-javascript-eslint-executable (executable-find "eslint")))
;;
(use-package flycheck
  :ensure t
  :hook (web-mode . my/set-local-eslint)
  :init
  (global-flycheck-mode 1))
(flycheck-add-mode 'javascript-eslint 'web-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b167a732d4023f0c040e1ad4a7ebe13db9c9851f2ee211f0f89c1ff5120d03bd" "adab4bfb5305761964dbaab07061360e2c68d2e5af91592d0c0e936d5f70c652" "ec5e80e491d3f500217fc9d28171600d1927692a607d0cd6cacb91318e0f0fe3" "1782fc4921306f3e5de5462c505938aeeb7a0105c235c3c524cef5fe74ffa6b2" "40a6f4318895475665d999566f360f0c237c2fc5b0b99dbd7e5af9ceceb993ea" "c44032eaa7edf8c0178985817d7b2bc13378171454acd56ab75eed6e32c2cf1c" "f1ddbd51e9848e7f6036a6e7c40ff09f3421bde03c4793753ca1bc9e0110a52d" "9c030815497cff41dbdb0cb4d3ae08268d99ac2926cd69999249181d9c1b3109" "275a6269ddd403be6bae290a5b5f9bd5d4d8321d76f5aa5931178617f1223a65" "f790a0f52ca8e939b5108cbae2580ca3431b1697ae321dc827bb029937410841" "a81d814e9021cd20ae59346bd49c2d74ce1a3ef967e56fada5aeace01d4a7854" default))
 '(forge-topic-list-limit '(60 . 60))
 '(org-agenda-files '("~/Documents/agenda.org"))
 '(package-selected-packages
   '(diminish org-tree-slide evil-commentary vterm plantuml-mode forge modus-vivendi-theme org-roam yaml-mode which-key web-mode visual-regexp vi-tilde-fringe use-package undo-fu smex smartparens smart-mode-line-powerline-theme simpleclip shell-pop prettier-js powerthesaurus org-superstar org-pdftools neotree multiple-cursors move-text markdown-mode magit ivy-rich haml-mode git-gutter flyspell-correct-popup flx expand-region exec-path-from-shell emmet-mode define-word counsel-projectile company avy all-the-icons))
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))
 '(shell-pop-universal-key "s-="))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
