; Set custom variables
; -----------------------------------------------------

(custom-set-variables
  '(inhibit-startup-screen t)
  '(tab-width 4)
  '(wg-emacs-exit-save-behavior nil)
)
; Packages
; -----------------------------------------------------

(require 'package)
(package-initialize)

(setq package-archives '(
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")
))

;; Adding Plugins to path
(add-to-list 'load-path "~/.config/emacs/plugins/org-mode/" t)
(add-to-list 'load-path "~/.config/emacs/plugins/org-mode/contrib/lisp/" t)

;; SLIME
(add-to-list 'load-path "~/.config/emacs/plugins/slime/" t)
(require 'slime-autoloads)

; Font
; -----------------------------------------------------

(custom-set-faces
  '(default ((t (:height 110 :family "Inconsolata for Powerline"))))
  '(magit-item-highlight ((t (:background "black"))))
)
(add-to-list 'default-frame-alist '(font . "Inconsolata for Powerline-12"))


; Colorscheme
; -----------------------------------------------------

;; Adding Colorschemes to path
(add-to-list 'load-path "~/.config/emacs/themes/hemisu-theme/" t)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes" t)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/hemisu-theme/" t)

;; Load the Colorscheme
(load-theme 'hemisu-light t)

; General Settings
; -----------------------------------------------------

(setq visible-bell 1) ;; Disable bell

(require 'org)
(setq make-backup-files nil)
(setq gnus-button-url 'browse-url-generic
    browse-url-generic-program (if
        (eq system-type 'windows-nt)
        "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
        "google-chrome-stable"
    )
    browse-url-browser-function gnus-button-url
)

(transient-mark-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-auto-revert-mode 1)

(add-hook 'after-change-major-mode-hook #' (lambda ()
    (setq indicate-buffer-boundaries t)
))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)

(put 'dired-find-alternate-file 'disabled nil)
(ido-mode 1)
(setq ido-enable-flex-matching t)
; (setq-default indent-tabs-mode nil)
(put 'upcase-region 'disabled nil)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
; (global-set-key (kbd "C-x C-c") (lambda ()
;     (interactive)
;     (when (yes-or-no-p "Are you really sure?")
;         (call-interactively #'save-buffers-kill-emacs)
;     )
; ))

; Set up unicode
; -----------------------------------------------------

(when (eq system-type 'windows-nt)
    (prefer-coding-system       'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (setq default-file-name-coding-system 'cp1252)
    (add-hook 'ido-minibuffer-setup-hook (lambda ()
        (set-buffer-file-coding-system 'cp1252)
    ))
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)) ;; From Emacs wiki
    (set-clipboard-coding-system 'utf-16le-dos) ;; MS Windows clipboard is UTF-16LE
)
; Clean up
; -----------------------------------------------------

(defun cleanup-buffer-safe ()
    (interactive)
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)
    (untabify (point-min) (point-max))
)

(defun cleanup-buffer ()
    (interactive)
    (cleanup-buffer-safe)
    (indent-region (point-min) (point-max))
)

(global-set-key (kbd "C-c s") 'cleanup-buffer)

; Tetris
; -----------------------------------------------------

(setq tetris-score-file "~/.config/emacs/tetris-scores")

; Init.el ends here
; -----------------------------------------------------

(provide 'init)
