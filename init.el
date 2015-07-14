;;; init.el --- My emacs configuration

;;; License:

;; Copyright (C) 2015  Niklas Köhler

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; init.el contains all my emacs settings and configs

;;; Code:

;;; Package-Management

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar package-list '(ace-jump-mode
		       base16-theme
		       bbdb
		       company
		       company-jedi
		       elscreen
		       emms
		       emms-player-mpv
		       expand-region
		       evil
		       flx-ido
		       flycheck
		       geiser
		       git-gutter
		       helm
		       helm-projectile
		       helm-swoop
		       hydra
		       linum-relative
		       magit
		       markdown-mode
		       paredit
		       pretty-lambdada
		       projectile
		       rainbow-delimiters
		       rainbow-mode
		       slime
		       slime-company
		       smart-mode-line
		       smex
		       undo-tree
		       wanderlust
		       web-mode
		       whole-line-or-region))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Sane Defaults

;; Font
(when (member "DenjaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Hide GUI stuff / Prettify scratch buffer
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode -1)
(setq initial-scratch-message nil
			inhibit-startup-screen t)

;; Misc
(load-theme 'base16-monokai-dark t)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(subword-mode 1)(setq tab-width 2)
(savehist-mode 1)
(global-hl-line-mode 1)
(global-flycheck-mode 1)
(global-subword-mode 1)
(global-superword-mode 1)
(setq savehist-mode 1
      compilation-ask-about-save nil
      global-hl-line-mode t
      make-backup-files nil
      show-trailing-whitespace t
      echo-keystrokes 0.1
      sentence-end-double-space nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Auto-save-files in the /temp directory
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Auto saving
;; (setq auto-save-interval 1)
;; (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;;; Global Keybindings

;; Fixing mappings for the use with a german keyboard layout
(global-set-key (kbd "M-Q") 'mark-word)
(global-set-key (kbd "C-M-5") 'query-replace-regexp)

;; Switch buffers
(global-set-key (kbd "M-o") 'other-window)

;; (defun join-line-and-kill-whitespace ()
;;   "When point is at eol the lines are joined together
;; with 'just' one space between them. Otherwise
;; whitespace is shortened to one space."
;;   (interactive)
;;   (if (eq (point) (point-max))
;;       (progn
;; 				(kill-line)
;; 				(just-one-space))
;;     (just-one-space)))

;; (global-set-key (kbd "M-<SPC>") 'join-line-and-kill-whitespace)

;;; Helm

(require 'helm)
(defvar helm-buffers-fuzzy-matching t)
(defvar helm-recentf-fuzzy-match t)
(defvar recentf-exclude (list "/home/niklas/.emacs.d/elpa" "/usr/share/emacs"))
(helm-autoresize-mode 1)
(global-set-key (kbd "C-x C-b") 'helm-mini )
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;; Whole line or region

(whole-line-or-region-mode 1)

;;; Smart Mode Line

(setq sml/no-confirm-load-theme t)
(sml/setup)

;;; Undo-tree

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist
      '((".*" . "~/.cache/emacs/undo")))
(setq undo-tree-auto-save-history t)
(global-set-key (kbd "C-,") 'undo-tree-undo)
(global-set-key (kbd "C-.") 'undo-tree-redo)

;;; Company

(require 'company)
(global-company-mode 1)
(setq company-idle-delay 0.1)
(setq company-tooltip-limit 20)
(setq company-echo-delay 0)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(add-to-list 'company-backends 'slime-company t)

;; Some comfy mappings for company
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete)

;;; AceJump

(require 'ace-jump-mode)
(global-set-key (kbd "C-c C-f") 'ace-jump-mode)

;;; Flx

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t
      ido-use-faces nil)

;;; Smex

(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;;; Org

(require 'org)
(add-hook 'org-mode-hook (lambda ()
			   (interactive)
			   (linum-mode 1)
			   (local-unset-key (kbd "C-,"))
			   (define-key org-mode-map (kbd "C-,") 'undo-tree-undo)))
(setq org-hide-leading-stars 'hidestars
      org-return-follows-links t)
(add-to-list 'org-modules "org-habit")
(setq org-agenda-files '("~/org"))

;; Global mappings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Babel
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ruby . t)
   (scheme . t)
   (lisp . t)
   (emacs-lisp . t)))
(defvar org-bable-scheme-command "racket")

;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (interactive)
	    (paredit-mode 1)
	    (linum-mode 1)
	    (pretty-lambda-mode 1)))

(defun load-current-buffer ()
  "Save and load the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name)))
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'load-current-buffer)

;;; Web Mode

(require 'web-mode)
(add-hook 'css-mode-hook (lambda () (web-mode)))
(add-hook 'html-mode-hook (lambda () (web-mode)))

;;; Python

(require 'python)
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))

;;; Scheme

(add-hook 'scheme-mode-hook (lambda ()
			      (interactive)
			      (linum-mode 1)
			      (pretty-lambda-mode 1)
			      (paredit-mode 1)))

;;; Geiser

(defvar geiser-active-implementations '(racket))
(defvar geiser-mode-autodoc-p nil)
(add-hook 'geiser-repl-mode-hook (lambda ()
				   (interactive)
				   (paredit-mode 1)
				   (hl-line-mode -1)))

;;; Rainbow-Delimiters

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Slime

(require 'slime-autoloads)
(defvar inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-scratch slime-editing-commands slime-repl))
(slime-setup '(slime-fancy))
(defvar slime-use-autodoc-mode nil)
(add-hook 'slime-mode-hook (lambda ()
			     (interactive)
			     (linum-mode 1)
			     (paredit-mode 1)))

;;; Assign the Common Lisp filetype (".cl") to slime
(add-to-list 'auto-mode-alist '("\\.cl\\'" . slime-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;;; BBDB

(require 'bbdb)
(bbdb-initialize)

;;; EMMS

(require 'emms-setup)
(require 'emms-player-mpv)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/music"
      emms-player-list '(emms-player-mpv))

;;; Eshell

(defvar eshell-prompt-function (lambda ()
				 (concat (propertize (eshell/pwd) 'face `(:foreground "#66d9ef"))
					 (propertize " λ" 'face `(:foreground "#f92672"))
					 (propertize " " 'face `(:foreground "#f9f8f5")))))
(defvar eshell-highlight-prompt nil)

;;; Relative Line Numbers

(require 'linum-relative)
(setq linum-relative-current-symbol "")

;;; Wanderlust

(autoload 'wl "wl" "Wanderlust" t)

;;; Elscreen

(elscreen-start)
(setq elscreen-tab-display-control nil
      elscreen-tab-display-kill-screen nil
      elscreen-display-tab nil)

;;; Projectile (+ Helm)

(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode 1)
(helm-projectile-on)
(defvar projectile-completion-system 'helm)
(defvar projectile-globally-ignored-directories
  `(,(concat user-emacs-directory "eshell")
    ,(concat user-emacs-directory "elpa")
    ,(concat user-emacs-directory "themes")
    ,(concat user-emacs-directory "auto-save-list")
    "\\.git\\"
    "\\.cache"))
(defvar projectile-globally-ignored-files
  '(,(concat user-emacs-directory "smex-items")
    "\\#\\"))

;;; Rice

(custom-set-faces
 '(company-preview-common ((t (:background "#383830" :foreground "#75715e"))))
 '(company-scrollbar-bg ((t (:background "#49483e"))))
 '(company-scrollbar-fg ((t (:background "#a59f85"))))
 '(company-tooltip ((t (:background "#383830" :foreground "#f9f8f5"))))
 '(company-tooltip-annotation ((t (:background "#383830" :foreground "#66d9ef"))))
 '(company-tooltip-common ((t (:background "#383830" :foreground "#75715e"))))
 '(company-tooltip-common-selection ((t (:background "#383830" :foreground "#66d9ef"))))
 '(company-tooltip-selection ((t (:background "#383830" :foreground "#f92672"))))
 '(elscreen-tab-background-face ((t (:background "#383830"))))
 '(elscreen-tab-current-screen-face ((t (:background "#66d9ef" :foreground "#49483e"))))
 '(helm-buffer-file ((t (:foreground "#a59f85"))))
 '(helm-buffer-process ((t (:foreground "#a6e22e"))))
 '(helm-buffer-size ((t (:foreground "#a6e22e"))))
 '(helm-candidate-number ((t (:foreground "#a59f85"))))
 '(helm-header ((t (:foreground "#a6e22e"))))
 '(helm-helper ((t (:foreground "#a6e22e"))))
 '(helm-selection ((t (:background "#49483e"))))
 '(helm-source-header ((t (:foreground "#f92672"))))
 '(helm-visible-mark ((t (:background "#a59f85" :foreground "#49483e"))))
 '(hl-line ((t (:background "#383830"))))
 '(linum-relative-current-face ((t (:background "#49483e" :foreground "#fd971f"))))
 '(mode-line ((t (:background "#49483e"))))
 '(mode-line-inactive ((t (:background "#000"))))
 '(sml/filename ((t (:foreground "#f8f8f2"))))
 '(sml/folder ((t (:foreground "#a59f85"))))
 '(sml/git ((t (:foreground "#f92671"))))
 '(sml/line-number ((t (:foreground "#a6e22e"))))
 '(sml/modified ((t (:foreground "#66d9ef"))))
 '(sml/position-percentage ((t (:foreground "#a6e22e"))))
 '(sml/prefix ((t (:foreground "#f92671")))))

(provide 'init)
;;; init.el ends here
