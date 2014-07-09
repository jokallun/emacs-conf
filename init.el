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
				  elpy workgroups rainbow-delimiters rainbow-mode
                                  solarized-theme zenburn-theme ess
                                  clojure-mode clojure-test-mode cider
                                  autopair)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ==== Behavior
(blink-cursor-mode t)
(autopair-global-mode)
(setenv "LC_CTYPE" "UTF-8")


;; OSX settings
(when (eq window-system 'ns) 
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;; (require 'ido-vertical-mode)
(ido-mode 1)
;; (ido-vertical-mode 0)

;; ==== Line numbers
;; (setq linum-format "%4d\u2502")
;; (global-linum-mode 1)


;; ==== Appearence 
(menu-bar-mode 1)
(global-auto-revert-mode t)
;; (blink-cursor-mode 0)
;; (show-paren-mode 1)
;; (tool-bar-mode 0)
(idle-highlight-mode nil)

(when window-system
  (load-theme 'solarized-dark t)
  ;; (load-theme 'zenburn t)
  )
 
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)


;; ==== Python
'(python-shell-interpreter "~/anaconda/bin/ipython")
(elpy-enable)
(elpy-use-ipython)


;; ==== Clojure
;; Add clojurescript to clojure-mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
;; (setq nrepl-hide-special-buffers t)
;; (setq nrepl-popup-stacktraces nil)
;; (setq nrepl-popup-stacktraces-in-repl t)
;; (add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

;; ugly hack fix
;; (custom-set-faces
;;  '(slime-repl-output-face ((t (:foreground "#cb4b16" :italic t :bold t)))))


;; ==== ESS
(require 'ess-site)

;; ==== Workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-z"))
(workgroups-mode 1)


;; ==== Key-bindings
;;(require 'starter-kit-bindings)
;; Set my keybindings so that they're ALWAYS on
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "M-;") 'comment-or-uncomment-region)
(define-key my-keys-minor-mode-map (kbd "C-c C-v") 'mark-sexp)

(define-key my-keys-minor-mode-map (kbd "C-c s") 'shell)


;;(define-key my-keys-minor-mode-map (kbd "C-c C-s") 'slime-connect)
(define-key my-keys-minor-mode-map (kbd "C-c C-h") 'idle-highlight-mode)
;;(define-key my-keys-minor-mode-map (kbd  "M-.") 'find-tag)

(define-key my-keys-minor-mode-map [f2] 'other-window)
(define-key my-keys-minor-mode-map [f1] 'previous-multiframe-window)
(define-key my-keys-minor-mode-map [(f3)] 'javadoc-lookup)  
(define-key my-keys-minor-mode-map [(shift f3)] 'javadoc-help)    

(define-key my-keys-minor-mode-map  (kbd  "C-c C-m") 'execute-extended-command)
;; (define-key my-keys-minor-mode-map (kbd  "C-q") 'backward-kill-word)
;; (define-key my-keys-minor-mode-map (kbd  "M-n") 'forward-paragraph)
;; (define-key my-keys-minor-mode-map (kbd  "M-p") 'backward-paragraph)

;;(define-key my-keys-minor-mode-map (kbd  "C-x C-m") 'call-last-kbd-macro)

(define-key my-keys-minor-mode-map (kbd  "M-1") 'ido-switch-buffer)
(define-key my-keys-minor-mode-map (kbd  "M-2") 'ido-find-file)

(define-key my-keys-minor-mode-map (kbd  "C-z C-h") 'ess-display-help-on-object)
(define-key my-keys-minor-mode-map (kbd  "<M-return>") 'hippie-expand)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

(defun my-keys-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-keys-minibuffer-setup-hook)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" default)))
 '(nxml-child-indent 4 t))
 

;; ====Window dedication
;; (defun toggle-current-window-dedication ()
;;  (interactive)
;;  (let* ((window    (selected-window))
;;         (dedicated (window-dedicated-p window)))
;;    (set-window-dedicated-p window (not dedicated))
;;    (message "Window %sdedicated to %s"
;;             (if dedicated "no longer " "")
;;             (buffer-name))))
;;(global-set-key [pause] 'toggle-current-window-dedication)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
