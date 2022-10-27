[![MELPA](http://melpa.org/packages/decl-badge.svg)](http://melpa.org/#/decl)
# decl

Library for organizing code declaratively.

## Background

Configuring Emacs can get complicated especially when you want your configuration to be cross-platform (to run without any issues on FreeBSD, Linux, OpenBSD, OS X and Windows) and work in both terminals and graphical user interfaces. If your Emacs Lisp code is long and complicated, you might become reluctant to modify or add functionality to Emacs due to the possibility of breaking your existing configuration and ultimately wasting your time. decl.el is an attempt to alleviate some of these problems when configuring Emacs using Emacs Lisp.

## Instructions

### User Steps
1. Logically separate Emacs Lisp code portions in your configuration files by the functionality that they add or modify within Emacs. Keep note of any dependencies that those portions of code may have.

2. Wrap all of these separate portions of Emacs Lisp code in lambda functions that return a truthy value (non-nil) upon successful execution and return nil upon failure. If any lambda function throws an error upon execution, it is considered to have not executed successfully, equivalent to if it were to return nil.

3. Create a decl-block and assign it a keyword name.

4. For each lambda function, create a decl-node to place it in. Give each decl-node a keyword name, the keyword name of the decl-block that it belongs to, the lambda function to be placed within it, and optionally a list of the keyword names of the decl-nodes housing the lambda function's dependencies, all in the stated order. The dependencies of a lambda function are other lambda functions which must execute successfully in order for the lambda function to be executed successfully.

5. Solve the decl-block.

_Optionally, use the interactive function `decl-report` for a report in an Org-mode buffer that describes how the decl-block was solved._

### Solver Steps *(Disregard if you are not interested in how this library works. These steps occur when a decl-block is solved for those who are interested.)*
1. Disregard all non-executable decl-nodes within decl-block. First find and disregard all nodes with non-existent nodes within their constraint lists. Second, taking into account all nodes with non-existing constraints, use Tarjan's Strongly Connected Components Algorithm to find and disregard all nodes involved in circular relationships of size greater than 1. Thirdly, disregard all remaining nodes which have themselves as one of their own constraints (circular relationships of size 1).

2. Place all decl-nodes which have not been disregarded in a list.

3. Keep on iterating over the list of decl-nodes, executing all nodes which have no remaining constraints. If the execution of a decl-node is successful, remove the decl-node's keyword name from all of the constraint lists of the remaining decl-nodes in the list. If the execution of a decl-node results in failure, remove all the decl-nodes in the list which have the executed decl-node's keyword name within their constraint list. Regardless of the success of a decl-node's execution, remove the executed decl-node from the list of nodes which have not been disregarded. If during one iteration over the list no decl-nodes are executed, stop iterating over the list.

## Code

Refer to decl.el

## Tests

Refer to decl-tests.el

## Sample Usage

The following example assumes that you have installed this package via [http://melpa.org/](MELPA) (alternatively just download and include the `decl.el` file manually from [https://github.com/preetpalS/decl.el/](github)):

``` elisp
;;; init.el --- sample init.el using library

(package-initialize)
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
                t)
              '(:gui :mac-osx))
   (decl-node :windows-consolas :init
              (lambda ()
                (set-face-attribute 'default nil :font "consolas-14:antialias=natural"))
              '(:windows :gui))
   (decl-solve :init)
   ; Optionally execute for a report on the solver's execution: (decl-report :init)
   ))

(provide 'init)

;;; init.el sample ends here
```

## Possible future improvements

The follow Emacs lisp comment has some ideas that I initially had but
never got around to implementing yet. Contributions are welcome
(although the existing solver implementation may need refactoring to
implement some of these features).

``` emacs lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; decl.el TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  1. Add the ability to have targets so that the library can actually be used as a      ;;    ;;
;;     make alternative.                                                                  ;;    ;;
;;                                                                                        ;;    ;;
;;  2. Make org-mode buffer for decl-report read-only.                                    ;;    ;;
;;                                                                                        ;;    ;;
;;  3. Have proper testing of this package (that does not depend on visual                ;;    ;;
;;     verification...)                                                                   ;;    ;;
;;                                                                                        ;;    ;;
;;  4. Make better use of naming (especially for private function and variables).         ;;    ;;
;;                                                                                        ;;    ;;
;;  5. Allow for execution of `decl-solve' to be paused and/or interrupted.               ;;    ;;
;;                                                                                        ;;    ;;
;;  6. Negative dependencies (execute decl-node only if another decl-node's execution     ;;    ;;
;;     fails).                                                                            ;;    ;;
;;                                                                                        ;;    ;;
;;  7. Concurrency. Deal with the ability to execute nodes in parallel. Add the ability   ;;    ;;
;;     to specify whether the default is to specify whether nodes should be executed in   ;;    ;;
;;     parallel by default (i.e. independent blocks of code) or whether the default       ;;    ;;
;;     should be serial execution unless specified otherwise. There should be a way to    ;;    ;;
;;     combine commands through an API that allows for specific commands to be combined   ;;    ;;
;;     and fed to something else to be parallelized externally (like independent source   ;;    ;;
;;     files being compiled in parallel by a single compiler instance instead of invoking ;;    ;;
;;     multiple instances of a compiler (via running shell commands in parallel)).        ;;    ;;
;;                                                                                        ;;    ;;
;;  8. Track source file locations of `decl-node' definitions (unless in mini-buffer?) so ;;    ;;
;;     that the org-mode buffer generated by `decl-report' can have links back to source. ;;    ;;
;;                                                                                        ;;    ;;
;;  9. Investigate how to appropriately add instrumentation to track performance- and     ;;    ;;
;;     usage-related metrics.                                                             ;;    ;;
;;                                                                                        ;;    ;;
;; 10. Investigate if this library would benefit from having direct support for           ;;    ;;
;;     asynchronous execution.                                                            ;;    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```
