(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(setq auto-save-default nil)
(setq make-backup-files nil)

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

(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

(setq inhibit-startup-screen t)

(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)
(global-set-key (kbd "s-j") 'left-char)
(global-set-key (kbd "s-l") 'right-char)

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

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(add-hook 'before-save-hook 'whitespace-cleanup)

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

(use-package exec-path-from-shell
   :ensure t)
(when (memq window-system '(mac ns))
(exec-path-from-shell-initialize))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))

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
(global-set-key (kbd "s-S-<return>") 'smart-open-line-above)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(pdf-tools-install)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))


(setq org-hide-emphasis-markers t)

(use-package prettier-js
  :hook (web-mode . prettier-js-mode))

(setq prettier-js-args '(
  "--arrow-parens" "avoid"))

(setq org-emphasis-alist
  '(("*" (bold :slant italic :weight black :foreground "#1ABC9C" :background "#C0C0C0" ))
    ("/" (italic :foreground "#F3CA40" ))
    ("_" (:underline t  ))
    ("=" (:foreground "#E74C3C"))
    ("~" (:foreground "#53df83" ))
    ("+" (:strike-through nil :foreground "#FFC300" ))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/home/breno/BreNotes"))
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
  (eshell 'N))

(use-package visual-regexp
  :config
  (define-key global-map (kbd "M-s-f") 'vr/replace)
  (define-key global-map (kbd "s-r") 'vr/replace))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("/home/breno/agenda.org"))
 '(package-selected-packages
   '(org-roam yaml-mode which-key web-mode visual-regexp vi-tilde-fringe use-package undo-fu smex smartparens smart-mode-line-powerline-theme simpleclip shell-pop prettier-js powerthesaurus org-superstar org-pdftools neotree multiple-cursors move-text markdown-mode magit ivy-rich haml-mode git-gutter flyspell-correct-popup flx expand-region exec-path-from-shell emmet-mode define-word counsel-projectile company avy all-the-icons add-node-modules-path)))
