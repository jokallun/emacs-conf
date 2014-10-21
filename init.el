;; ==== Environment
;; Unicode
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

;; (setq exec-path
;;       '("/usr/texbin"
;;         "/usr/local/share/python"
;;         "/usr/local/bin"
;;         "/usr/local/sbin"
;;         "/Users/jkallunk/bin"
;;         "/usr/bin"
;;         "/bin"
;;         "/usr/sbin"
;;         "/sbin"))

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; ==== Packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")  t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
				  elpy workgroups rainbow-delimiters
                                  color-theme-sanityinc-solarized color-theme-monokai ess
                                  clojure-mode clojure-test-mode cider
                                  autopair projectile)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ==== Behavior
(blink-cursor-mode t)
(autopair-global-mode)
(setenv "LC_CTYPE" "UTF-8")
(ido-mode 1)

;;==== OSX settings
(when (eq window-system 'ns) 
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


;; ==== Appearence 
(menu-bar-mode 1)
(global-auto-revert-mode t)
;; (blink-cursor-mode 0)
;; (show-paren-mode 1)
;; (tool-bar-mode 0)
(idle-highlight-mode nil)

(when window-system
  (load-theme 'solarized-dark t)
  (when (eq window-system 'w32) ;; windows colors
    (load-theme 'sanityinc-solarized-dark t))
  )
 

;; ==== Projectile
(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t)
;;(define-key projectile-mode-map "M-2" 'projectile-switch-to-buffer)
(eval-after-load 'projectile 
  '(define-key projectile-mode-map (kbd "M-2") 'projectile-switch-to-buffer))

;; ==== Python
(when (eq window-system 'ns)  ;; for OSX anaconda
  '(python-shell-interpreter "~/anaconda/bin/ipython"))
(elpy-enable)
(elpy-use-ipython)


;; ==== Clojure
;; Add clojurescript to clojure-mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)


;; ==== ESS
(require 'ess-site)


;; ==== Key-bindings
(global-set-key (kbd  "M-1") 'ido-switch-buffer)
(global-set-key (kbd  "C-x C-m") 'call-last-kbd-macro)
(global-set-key (kbd  "<M-return>") 'hippie-expand)
(global-set-key (kbd "C-c C-h") 'idle-highlight-mode)
(global-set-key (kbd  "M-n") 'forward-paragraph)
(global-set-key (kbd  "M-p") 'backward-paragraph)



(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(js-indent-level 2)
 '(nxml-child-indent 4 t)
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" default))))
 
(server-start)

