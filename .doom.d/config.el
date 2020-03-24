(use-package! org-drill)

(setq user-full-name "Sonny Jansson")
(setq user-mail-address "sonnyjansson01@gmail.com")

(server-start)

(map! :leader :desc "Switch to last buffer" "DEL" #'evil-switch-to-windows-last-buffer)

(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

(map! :leader :desc "Find file in project" ":" #'+ivy/projectile-find-file)

(setq org-directory "~/org/")
(setq org-projects (concat org-directory "Projects/"))

(setq org-agenda-files (list (concat org-directory "main.org") org-projects))

(setq org-default-notes-file (concat org-directory "inbox.org"))

(setq org-tags-exclude-from-inheritance '("PROJECT"))

(defun org-find-projects ()
  "Produces a list of all headlines with the :PROJECT: tag"
  (interactive)
  (setq files (org-agenda-files nil 'ifmode))
  (setq rtnall nil)
  (setq matcher (org-make-tags-matcher "+PROJECT"))
  (setq match (cdr matcher))
  (while (setq file (pop files))
    (setq buffer (org-get-agenda-file-buffer file))
    (with-current-buffer buffer
      (setq rtn (org-scan-tags '(org-get-heading t t t t)
                               match
                               nil))
      (prin1 rtn)
      (setq rtnall (append rtnall rtn))))
  rtnall)

(defun all-project-files ()
  "Returns a list of all project files"
    (directory-files org-projects nil "\\.org$"))
; (make-obsolete all-project-files "This function has been replaced by org-find-projects")

(defun project-files-to-choices ()
  "Make choice string out of project files"
  (let (
    (prepend-all (lambda (x xs) (if xs (concat (concat x (car xs)) (funcall prepend-all x (cdr xs))) ""))))
    (concat "%^{Project" (funcall prepend-all "|" (org-find-projects)) "}")))

(setq org-capture-templates
      '(("t" "To do" entry (file+headline "inbox.org" "To do")
         "* TODO %? %^G \n")
        ("s" "Scheduled" entry (file+headline "inbox.org" "To do")
         "* TODO %? %^G \nSCHEDULED: %^T")
        ("d" "Deadline" entry (file+headline "inbox.org" "To do")
         "* TODO %? %^G \nDEADLINE: %^T")
        ("p" "Project idea" entry (file+headline "inbox.org" "Projects")
         "* Project idea: %(project-files-to-choices) %T\n%?")
        ("n" "Loose note" entry (file+headline "inbox.org" "Loose notes")
         "* %T\n%?")
        ("i" "Random idea" entry (file+headline "inbox.org" "Ideas")
         "* %T\n%?")
        ("w" "Workout" entry (file+headline "main.org" "Workout journal")
         "* %^t\n%?")
        ("l" "Ledger")
        ("lc" "Cash" plain (file "money")
         "%(org-read-date) * %^{Payee}
Expenses:Cash
Expenses:%^{Account} %^{Amount}")))
