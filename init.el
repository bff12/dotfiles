;; Global Keys
(global-set-key "\C-ca" 'org-agenda)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; set full size echo buffer
(setq eval-expression-print-length nil)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
;; Scroll with C-u
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("/Users/brenofarias/org/life.org"))
 '(org-deadline-warning-days 7)
 '(package-selected-packages '(evil org)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; work with tab in org mode + evil mode
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
