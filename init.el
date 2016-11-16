
;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------
(add-to-list 'load-path "~/.emacs.d/packages/")

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    material-theme
    py-autopep8
    magit
    auctex
    smex
    rainbow-delimiters
    highlight-parentheses
    ace-jump-mode
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(cond
 ((eq system-type 'windows-nt)
  (set-default-font "Lucida Sans Unicode 12")))
(global-set-key (kbd "C-x g") 'magit-status) ;;set magit shortcut


;; PROGRAMMING SETTING
;; ----------------------------------------
;; delimiter settings
(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; code navigation
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; comment setting
 (defun comment-dwim-line (&optional arg)
        "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
          (interactive "*P")
          (comment-normalize-vars)
          (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))
            (comment-dwim arg)))
  (global-set-key "\M-;" 'comment-dwim-line)
;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(elpy-use-ipython)
;; (require 'request)
;; (require 'ein)        
;;(setq ein:use-auto-complete t)
;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; LaTeX SETTING
;; ----------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("Sumatra PDF"
      ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
       (mode-io-correlate " -forward-search %b %n")
       " %o")))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and start")
     (output-dvi "Yap")
     (output-pdf "Sumatra PDF")
     (output-html "start"))))
 '(default-input-method "chinese-flypy")
 '(org-agenda-files (quote ("~/org/build.org" "~/org/lab.org")))
 '(package-selected-packages
   (quote
    (auctex smex rainbow-delimiters py-autopep8 material-theme magit highlight-parentheses flycheck elpy ein better-defaults ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ORG-MODE SETTING
;; ----------------------------------------
(setq org-latex-to-pdf-process '("PDFLATEX=\"pdflatex –shell-escape\" texi2dvi -p %f"))
(global-set-key (kbd "C-c a") 'org-agenda)

;; window navigation settings
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; chinese XiaoHe input
(require 'flypy)


;;smex
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)
;;hyphen on space
    (defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command 
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))
;;update on load file
   (defun smex-update-after-load (unused)
      (when (boundp 'smex-cache)
        (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)
;;use acronyms
;;; Filters ido-matches setting acronynm matches in front of the results
(defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
  (if (and (fboundp 'smex-already-running) (smex-already-running)
           (> (length ido-text) 1))
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match ido-text item) ;; exact match
              (add-to-list 'acronym-matches item)
            (if (string-match (concat regex "[^-]*$") item) ;; strict match
                (add-to-list 'acronym-matches item)
              (if (string-match regex item) ;; appending relaxed match
                  (add-to-list 'acronym-matches item t)))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))
;; tramp for windows
(require 'tramp)
(cond
 ((eq system-type 'windows-nt)
  (setq tramp-default-method "plink"
      tramp-password-end-of-line "\r\n"))
  ((eq system-type 'gnu/linux)
   (setq tramp-default-method "ssh")))
(setq tramp-default-user "ztx"
      tramp-default-host "166.111.139.147")
(setq password-cache-expiry 36000)
;; init.el ends here

