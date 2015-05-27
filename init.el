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

;; Package-Management

(require 'package)

(defvar package-list '(ace-jump-mode
		       base16-theme
		       company
		       company-jedi
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
		       magit
		       markdown-mode
		       paredit
		       rainbow-delimiters
		       rainbow-mode
		       projectile
		       slime
		       slime-company
		       smex
		       undo-tree))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Sane Defaults

; Font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

; Hide GUI stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode -1)

;Misc
(load-theme 'base16-monokai-dark t)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(subword-mode 1)(setq tab-width 2)
(savehist-mode 1)
(global-hl-line-mode 1)
(global-flycheck-mode 1)
(setq savehist-mode 1
      compilation-ask-about-save nil
      global-hl-line-mode t
      inhibit-startup-screen t
      initial-scratch-message nil
      make-backup-files nil
      show-trailing-whitespace t
      echo-keystrokes 0.1)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Resize windows
(global-set-key (kbd "C-c h") 'shrink-window-horizontally)
(global-set-key (kbd "C-c j") 'enlarge-window)
(global-set-key (kbd "C-c k") 'shrink-window)
(global-set-key (kbd "C-c l") 'enlarge-window-horizontally)

; Instanlty quitting emacs using C-x C-c
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

; Store auto-save-files in the /temp directory
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; Auto saving
(setq auto-save-interval 1)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;;; Mode specific settings

;; Undo-tree

(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C--") 'undo-tree-undo)
(global-set-key (kbd "M--") 'undo-tree-redo)
(defvar undo-dir (concat (getenv "HOME") "/.cache/emacs/undo/"))
(setq undo-tree-auto-save-history)

;; Helm

(require 'helm)
(defvar helm-buffers-fuzzy-matching t)
(defvar helm-recentf-fuzzy-match t)
(defvar recentf-exclude (list "/home/niklas/.emacs.d/elpa" "/usr/share/emacs"))
(helm-autoresize-mode 1)
(global-set-key (kbd "C-x C-b") 'helm-mini )

;; Projectile (+ Helm)

(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
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

;; AceJump

(require 'ace-jump-mode)
(global-set-key (kbd "C-c f") 'ace-jump-char-mode)
(global-set-key (kbd "C-c C-f") 'avy-goto-char-2)

;; Flx

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(global-set-key (kbd "M-x") 'smex)

;; Org

(require 'org)

; Global mappings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-hide-leading-stars 'hidestars)
(defvar org-return-follows-links t)

; TODO-States
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "DELAY(g@/!)"
		  "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#FF0055" :weight bold))
	("STARTED" (:foreground "#FF4000" :weight bold))
	("DELAY" (:foreground "#4EB4FA" :weight bold))
	("DONE" (:foreground "#B1D631" :weight bold))
	("CANCELED" (:foreground "#ED0028" :weight bold))
	("APPn" (:foreground "#7E40A5" :weight bold))))
(setq org-log-done 'time)

; Babel
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ruby . t)
   (scheme . t)
   (lisp . t)
   (emacs-lisp . t)))

; Clocking
(add-hook 'org-mode-hook (lambda ()
			   (interactive)
			   (define-key org-mode-map (kbd "C-c C-i") 'org-clock-in)
			   (define-key org-mode-map (kbd "C-c C-o") 'org-clock-out)))
(org-clock-persistence-insinuate)
(defvar org-clock-history-length 35)
(defvar org-clock-in-resume t)
(defvar org-clock-persist t)
(defvar org-agenda-start-with-clockreport-mode t)
(defvar org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 4)))

; Agenda
(setq org-agenda-files '("~/org/"))
(defvar org-agenda-skip-deadline-if-done t)
(defvar org-agenda-skip-scheduled-if-done t)
(defvar org-log-into-drawer t)
(defvar org-agenda-filter-preset '(-someday))
(defvar org-agenda-span 1)

;; Emacs Lisp

(defun load-current-buffer ()
  "Save and load the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (interactive)
	    (paredit-mode 1)
	    (linum-mode 1)
	    (pretty-lambda-mode 1)
	    (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'load-current-buffer)))

;; Python
(require 'python)
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))

;; Scheme
(add-hook 'scheme-mode-hook (lambda ()
			      (interactive)
			      (linum-mode 1)
			      (pretty-lambda-mode 1)
			      (paredit-mode 1)))
;; Geiser

(defvar geiser-default-implementation 'racket)
(defvar active-implementation 'racket)
(defvar org-bable-scheme-command "racket")

;; Company

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-tooltip-limit 20)
(setq company-echo-delay 0)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)

;; Rainbow-Delimiters

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; SLIME

(require 'slime-autoloads)
(defvar inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-scratch slime-editing-commands slime-repl))

;;; Assign the Common Lisp filetype (".cl") to slime
(add-to-list 'auto-mode-alist '("\\.cl\\'" . slime-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;; Rice
(custom-set-faces
 ;;; company
 '(company-preview-common ((t (:background "#383830" :foreground "#75715e"))))
 '(company-tooltip ((t (:background "#383830" :foreground "#f9f8f5"))))
 '(company-tooltip-selection ((t (:background "#383830" :foreground "#f92672"))))
 '(company-tooltip-common ((t (:background "#383830" :foreground "#75715e"))))
 '(company-tooltip-common-selection ((t (:background "#383830" :foreground "#66d9ef"))))
 '(company-scrollbar-bg ((t (:background "#49483e"))))
 '(company-scrollbar-fg ((t (:background "#a59f85"))))
 ;;; hl-line+
 '(hl-line ((t (:background "#383830"))))
 ;;; helm
 '(helm-selection ((t (:foreground "#f92672"))))
 '(helm-candidate-number ((t (:foreground "#a59f85"))))
 '(helm-source-header ((t (:foreground "#fd971f")))))

(provide 'init)
;;; init.el ends here

