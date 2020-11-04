(setq inhibit-startup-message t)

(scroll-bar-mode -1)

(tool-bar-mode -1)

(tooltip-mode -1)

(set-fringe-mode 10)

(menu-bar-mode -1)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq visible-bell nil)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

(load-theme 'vs-dark t)
;;(use-package doom-themes
;;  :init (load-theme 'doom-dark+ t))

(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'atom-one-dark)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-set-navigator t)

(setq dashboard-banner-logo-title "Welcome to world of Emacs !!")

(setq dashboard-startup-banner "/home/vamshi/.emacs.d/spacemacs.png")

(setq dashboard-center-content t)

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(defun dashboard-insert-custom (list-size)
  (insert "Custom text"))
(add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
(add-to-list 'dashboard-items '(custom) t)

(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Homepage"
         "Browse homepage"
         (lambda (&rest _) (browse-url "homepage")))
        ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
        ("?" "" "?/h" #'show-help nil "<" ">"))
         ;; line 2
        ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
          "Linkedin"
          ""
          (lambda (&rest _) (browse-url "homepage")))
         ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

(rune/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package command-log-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-height 14)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-bar 'left)
(centaur-tabs-change-fonts "lucida mac" 170)
