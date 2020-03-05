(use-package! org-drill)

(setq user-full-name "Sonny Jansson")
(setq user-mail-address "sonnyjansson01@gmail.com")

(server-start)

(map! :leader :desc "Switch to last buffer" "DEL" #'evil-switch-to-windows-last-buffer)

(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

(map! :leader :desc "Find file in project" ":" #'+ivy/projectile-find-file)

(setq org-directory "~/org/")
(setq org-projects (concat org-directory "Projects/"))

(setq org-agenda-files '(org-directory org-projects))

(setq org-default-notes-file (concat org-directory "inbox.org"))

(defun all-project-files ()
  "Returns a list of all project files"
    (interactive)
    (directory-files org-projects nil "\\.org$"))

(defun project-files-to-choices ()
  "Make choice string out of project files"
  (let (
    (prepend-all (lambda (x xs) (if xs (concat (concat x (car xs)) (funcall prepend-all x (cdr xs))) ""))))
    (concat "%^{Project" (funcall prepend-all "|" (all-project-files)) "}")))

(setq org-capture-templates
      '(("p" "Project idea" entry (file+headline "inbox.org" "Projects")
         "* Project idea: %(project-files-to-choices)")))
