# decl

decl is an emacs lisp library for that enables you to organize your code in
a declarative manner.

## Sample Usage

The following example assumes that you have installed this package via a package
manager

    ;;; init.el --- sample init.el using library
    
    (package-initialize)
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
    
    (add-hook 
     'after-init-hook
     (lambda () "Your init file"
       (require 'decl)
       (decl-block :init)
       (decl-node :mac-osx :init (lambda () (eq system-type 'darwin)))
       (decl-node :windows :init (lambda () (eq system-type 'windows-nt)))
       (decl-node :linux :init (lambda () (eq system-type 'gnu/linux)))
       (decl-node :gui :init (lambda () (if window-system t nil)))
       (decl-node :mac-osx-fullscreen-supprt :init
                  (lambda ()
                    (defun toggle-fullscreen (&optional f)
                      (interactive)
                      (let ((current-value (frame-parameter nil 'fullscreen)))
                        (set-frame-parameter nil 'fullscreen
                                             (if (equal 'fullboth current-value)
                                                 (if (boundp 'old-fullscreen) old-fullscreen nil)
                                               (progn (setq old-fullscreen current-value)
                                                      'fullboth)))))
                    
                    (base-lib-assign-keybindings '("C-s-<268632070>" toggle-fullscreen)) ; command-control-f
                    t)
                  '(:gui :mac-osx))
       (decl-node :windows-consolas :init
                  (lambda ()
                    (set-face-attribute 'default nil :font "consolas-14:antialias=natural"))
                  '(:windows :gui))
       (decl-solve :init)))
    
    (provide 'init)
    
    ;;; init.el sample ends here
