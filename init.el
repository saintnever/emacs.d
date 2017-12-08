
;; init.el - -- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(add-to-list 'load-path "~/.emacs.d/packages/")
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    auto-complete
    ac-math
    ein
    elpy
    flycheck
    flyspell-correct
    py-autopep8
    material-theme
    magit
    auctex
    smex
    rainbow-delimiters
    highlight-parentheses
    ace-jump-mode
    smart-mode-line
    nyan-mode
    arduino-mode
    anaconda-mode
    ))

(mapc #'(lambda (package)
     (unless(package-installed-p package)
       (package-install package)))
 	 myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(require 'ido)
(ido-mode t)
(tool-bar-mode -1)
(setq inhibit-startup-message t); ; hide the startup message
(load-theme 'material t); ; load material theme
(global-linum-mode t); ; enable line numbers globally
(cond
 ((eq system-type 'windows-nt)
  ;; (set-default-font "Lucida Grande 10")
  (set-default-font "Ubuntu Mono 12")
  ))
;; Chinese Font 
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "Microsoft YaHei" :Size 12)))
(global-set-key (kbd "C-x g") 'magit-status) ;;set magit shortcut
(global-set-key (kbd "M-o")  'mode-line-other-buffer)
(setq sentence-end-double-space nil)

;; YASNIPPET SETTING
(require 'yasnippet)
(yas-global-mode 1)
;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

(define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

;;MODELINE SETTING
(setq sml/name-width 30)  
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/no-confirm-load-theme t)
(add-hook 'after-init-hook 'sml/setup)
(if nyan-mode () (nyan-mode t))
(setq nyan-bar-length 12)
(setq nyan-wavy-trail t)

;; AUTO-COMPLETE SETTING
(require 'auto-complete)
(require 'auto-complete-config)
;; (ac-config-default)
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)
(add-to-list 'ac-modes 'LaTeX-mode)
(setq ac-sources '(ac-source-yasnippet
ac-source-abbrev 
ac-source-words-in-same-mode-buffers))
(defun ac-LaTeX-mode-setup()
  (setq ac-sources
        (append '(ac-source-math-unicode
                  ac-source-math-latex
                  ac-source-latex-commands)
                ac-sources))
  )
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(setq ac-math-unicode-in-math-p t)
;; (ac-flyspell-workaround)
;; (setq ac-delay 0.2)



;; ISPELL SETTING
(require 'ispell)
(add-to-list 'ispell-dictionary-alist '(
                                         ("english"
                                          "[[:alpha:]]"
                                           "[^[:alpha:]]"
                                           "[']"
                                            t
                                            ("-d" "en_US")
                                            nil
                                            utf-8)))
(setq-default ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
;; (setq ispell-local-dictionary-alist ispell-dictionary-alist)
;; (setq ispell-hunspell-dictionary-alist ispell-dictionary-alist)
(setq ispell-dictionary "american")
; FLYSPELL SETTING
(require 'flyspell-correct-ido)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)

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

;; Arduino Setting
(add-to-list 'ac-modes 'arduino-mode)
(add-hook 'arduino-mode-hook 'electric-pair-mode)
(add-hook 'arduino-mode-hook 'rainbow-delimiters-mode)

;; PYTHON CONFIGURATION
;; --------------------------------------
(require 'anaconda-mode)
(setq
     ;; python-shell-interpreter "C:/Users/saintnever/Anaconda2/python.exe"
     python-shell-interpreter "ipython"
     ;; python-shell-interpreter-args "-i C:/Users/saintnever/Anaconda2/Scripts/ipython-script.py"
     )

(require 'py-autopep8)			
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'company-mode)
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
;; (elpy-enable)
;; (elpy-use-ipython)
;; ;; (add-hook 'python-mode-hook 'jedi:setup)
;; ;; (setq jedi:complete-on-dot t)    
;; (setq elpy-rpc-backend "jedi")

;; (require 'request)
;; (require 'ein)        
;; (setq ein:use-auto-complete t)
;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; Emacs25 have issue with elpy, so disable warning
;; (setq python-shell-prompt-detect-failure-warning nil)

;; LaTeX SETTING
;; ----------------------------------------
;; reftex SETTING	
(require 'reftex)	
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
;; (setq reftex-external-file-finders
;; '(("tex" . "/path/to/kpsewhich -format=.tex %f")
;;   ("bib" . "/path/to/kpsewhich -format=.bib %f")))
(setq reftex-plug-into-AUCTeX t)


(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)

(add-hook 'latex-mode-hook 'company-mode)  
(add-hook 'LaTeX-mode-hook 'company-mode)
(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '((company-math-symbols-latex company-math-symbols-unicode company-auctex-macros company-auctex-environments))
                      company-backends)))
(company-auctex-init)
(add-hook 'LaTeX-mode-hook 'my-latex-mode-setup)

;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            ; (setq TeX-command-default "XeLaTeX")
             (setq TeX-save-query nil)
            ; (setq TeX-show-compilation t)
             ))

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
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(default-input-method "chinese-flypy")
 '(org-agenda-files (quote ("~/.emacs.d/org/thu.org")))
 '(package-selected-packages
   (quote
    (company-math company-shell company-auctex company-eshell-autosuggest anaconda-mode virtualenv arduino-mode auctex smex rainbow-delimiters py-autopep8 material-theme magit highlight-parentheses flycheck elpy ein better-defaults ace-jump-mode)))
 '(preview-gs-command "gswin64c")
 '(preview-image-type (quote png)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ORG-MODE SETTING
;; ----------------------------------------
(add-to-list 'ac-modes 'org-mode)
(setq org-latex-to-pdf-process '("PDFLATEX=\"pdflatex –shell-escape\" texi2dvi -p %f"))
(global-set-key (kbd "C-c a") 'org-agenda)

;; window navigation settings
;; (global-set-key (kbd "C-x <up>") 'windmove-up)
;; (global-set-key (kbd "C-x <down>") 'windmove-down)
;; (global-set-key (kbd "C-x <right>") 'windmove-right)
;; (global-set-key (kbd "C-x <left>") 'windmove-left)

(defun frame-bck()
  (interactive)
  (other-window-or-frame -1)
)
(define-key (current-global-map) (kbd "M-O") 'other-window)
;; (define-key (current-global-map) (kbd "M-O") 'frame-bck)

;; chinese XiaoHe input
(require 'flypy)
;; kite augmented coding
;;(require 'kite)
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

