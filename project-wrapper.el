;; -*- lexical-binding: t -*-

;; Wrapper for project.el to get some extra functionality

;; TODO: extra struct containing information on the project
;;  - TAGS information to send to etags-wrapper
;;  - Project environment: function to get the project environment.
;;    Most modarn build systems setsup the environment when it runs the commands. therfor emacs is not started in the project env.
;;    Also I dont restart emacs for new projects so it will change over time. There are normally
;;    Debug files or the posebility to start shells with the correct env and that way you can extract the project env when
;;    you change project.

(require 'cl-lib)

(defgroup project-wrapper nil
  "wrapper for project.el to extend functionality"
  :prefix "project-wrapper")

(defvar project-wrapper-project-info nil
  "alist of project info linked to project")

(defun project-wrapper-expand-with-env (path env)
  "expand environment variables based on the given env.
env is the same format as process-environment.
env defaults to the project env set in project-wrapper-project-env"
  (let ((process-environment (or env process-environment)))
    (substitute-in-file-name path)))

(defun project-wrapper-env-0 (env-string)
  "convert env string from the command env -0 to the format used in process-environment"
  (butlast (split-string tt "\0")))

(cl-defstruct project-wrapper-info root exclutions external-roots env get-env-func)
; project information struct. this need more information going forward.
; root: the project root
; external-roots: project dependencies
; env: The shell environment for the project
; get-env-func: function to retrive the project environment

(cl-defstruct project-wrapper-exroot-info root exclutions static)
; root is the external root
; exclutions are dictionaris under root to be excluded
; statis, if repo will not change

; This needs to be reduced into a single operation not make struct and then add it
(defun project-wrapper-add-info (info)
  "add a project to the project wrapper list"
  ; TODO: make sure the project are written to the project-file
  ; TODO: make sure that root is absolute
  (add-to-list 'project-wrapper-project-info (cons (project-wrapper-info-root info) info)))

(when (require 'etags-wrapper nil t)
  (defun project-wrapper-etags-paths-to-repos ()
    "return a etags-wrapper repo paths"
    (let* ((pr-root (project-root (project-current)))
           (info (cdr (assoc pr-root project-wrapper-project-info)))
           (root (project-wrapper-info-root info))
           (exclutions (project-wrapper-info-exclutions info))
           (repos (project-wrapper-info-external-roots info))
           (env (project-wrapper-info-env info))
           repo
           (ret (list (cons root (cons exclutions nil)))))
      (dolist (repo repos ret)
        (let ((etw-repo (project-wrapper-expand-with-env (project-wrapper-exroot-info-root repo) env))
              (etw-exclutions (project-wrapper-exroot-info-exclutions repo))
              (etw-static (project-wrapper-exroot-info-static repo)))
          (add-to-list 'ret (cons etw-repo (cons etw-exclutions etw-static))))))))

; todo make sure that it is pr-root is alwais absolute
(defun project-wrapper--get-project-env (pr-root)
  (let ((pr-info (cdr (assoc pr-root project-wrapper-project-info))))
    (project-wrapper-info-env pr-info)))

(defun project-wrapper-initialize (pr-root &optional force-env-update)
  "initialize the project envirnment. Should this also set etags variables if possible"
  (when (or (not (project-wrapper--get-project-env pr-root)) force-env-update)
    (let* ((info (cdr (assoc pr-root project-wrapper-project-info)))
           (envf (project-wrapper-info-get-env-func info))
           env)
      (if (null envf)
          (setf (project-wrapper-info-env info) process-environment) ; this needs to be done different
        (setq env (funcall envf pr-root))
        (setf (project-wrapper-info-env info) env)))))

(provide 'project-wrapper)
