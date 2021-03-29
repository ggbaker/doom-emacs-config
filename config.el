~/.emacs.d/bin/doom clean

(setq user-full-name "Gary Baker"
      user-mail-address "gary.baker@wisc.edu")

(setq doom-theme 'doom-nord)

(setq display-line-numbers-type t)

(setq fill-column 79)
(setq visual-fill-column-width 85)

(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("     _/\\/\\/\\/\\/\\/\\____/\\/\\______/\\/\\________/\\/\\__________/\\/\\/\\/\\/\\______/\\/\\/\\/\\/\\_"
            "    _/\\______________/\\/\\/\\__/\\/\\/\\______/\\/\\/\\/\\______/\\/\\____________/\\/\\_________ "
            "   _/\\/\\/\\/\\/\\______/\\/\\/\\/\\/\\/\\/\\____/\\/\\____/\\/\\____/\\/\\______________/\\/\\/\\/\\___  "
            "  _/\\/\\____________/\\/\\__/\\__/\\/\\____/\\/\\/\\/\\/\\/\\____/\\/\\____________________/\\/\\_   "
            " _/\\/\\/\\/\\/\\/\\____/\\/\\______/\\/\\____/\\/\\____/\\/\\______/\\/\\/\\/\\/\\____/\\/\\/\\/\\/\\___    "
            "________________________________________________________________________________     "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq-default delete-by-moving-to-trash t)

(map! :leader
      :desc "Undo tree" "U" #'undo-tree-visualize)

(map! :leader
      (:prefix ("c" . "code")
      :desc "Comment line/region" ";" #'comment-line)
      )

(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))
(map! :leader
      (:prefix ("w" . "window")
       :desc "Close window and kill buffer" "D" #'close-and-kill-this-pane))

(map! :leader
      (:prefix ("s" . "search")
       :desc "Query replace" "r" #'query-replace
       :desc "Regexp query replace" "R" #'query-replace-regexp))

(add-hook! org-mode #'visual-fill-column-mode)

(use-package! org-ref
  :after helm-bibtex
  :custom
  (org-ref-default-bibliography '("~/Dropbox/Documents/working/library.bib"))
  (org-ref-pdf-directory "~/Dropbox/Documents/working/papers/")
  )

(general-define-key
 :states '(normal insert)
 :keymaps 'org-mode-map
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "]" 'helm-bibtex)

(setq org-directory "~/Dropbox/Documents/working/org")

(after! org-roam
  (setq org-roam-directory "~/Dropbox/Documents/working/roam/")
  (setq org-roam-capture-templates '(("d" "default" plain (function org-roam-capture--get-point) "%?" :file-name "${slug}" :head "#+title: ${title}\n" :unnarrowed t)))
)

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-templates
   '(("r" "ref" plain #'org-roam-capture--get-point "" :file-name "${citekey}" :head "#+title: ${title}\n#+roam_alias: ${citekey}\n#+roam_key: ${ref}\n"
      :unnarrowed t))))

(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("r" . "roam")
       :desc "Roam index" "RET" #'org-roam-jump-to-index
       :desc "Toggle Roam display" "'" #'org-roam-buffer-toggle-display))
      )

(use-package! org-download
  :after org
  :custom
  (org-download-image-dir "images")
  :config
  (map! :map org-mode-map
        :localleader
        (:prefix ("a" . "attachments")
         "c" #'org-download-screenshot))
  )

(add-hook! LaTeX-mode #'visual-fill-column-mode)

(defun TeX-italic()
  (interactive)
  (TeX-font nil ?\C-i))

(defun TeX-typewriter()
  (interactive)
  (TeX-font nil ?\C-t))

(defun TeX-bold()
  (interactive)
  (TeX-font nil ?\C-b))

(defun TeX-smallcaps()
  (interactive)
  (TeX-font nil ?\C-c))

(map! :map latex-mode-map
      ;; :leader
      :localleader
      (:prefix ("f" . "Font faces")
      :desc "Italic" "i" #'TeX-italic
      :desc "Monospace" "t" #'TeX-typewriter
      :desc "Bold" "b" #'TeX-bold
      :desc "Smallcaps" "c" #'TeX-smallcaps))
(map! :map LaTeX-mode-map
      ;; :leader
      :localleader
      (:prefix ("f" . "Font faces")
      :desc "Italic" "i" #'TeX-italic
      :desc "Monospace" "t" #'TeX-typewriter
      :desc "Bold" "b" #'TeX-bold
      :desc "Smallcaps" "c" #'TeX-smallcaps))

(defun LaTeX-change-environment ()
  (interactive)
  (LaTeX-environment 1))

(map! :map latex-mode-map
      :localleader
      (:prefix ("e" . "Environments")
       :desc "Insert environment" "e" #'LaTeX-environment
       :desc "Change environment" "u" #'LaTeX-change-environment
       :desc "Toggle starred environment" "*" #'evil-tex-toggle-env))
(map! :map LaTeX-mode-map
      :localleader
      (:prefix ("e" . "Environments")
       :desc "Insert environment" "e" #'LaTeX-environment
       :desc "Change environment" "u" #'LaTeX-change-environment
       :desc "Toggle starred environment" "*" #'evil-tex-toggle-env))

(setq-default TeX-engine 'xetex)

(map! :map latex-mode-map
      :localleader
      :desc "Compile" "c" #'TeX-command-run-all
      :desc "Compile command" "C" #'TeX-command-master
      :desc "View" "v" #'TeX-view
      :desc "Next error" "'" #'TeX-next-error
      :desc "Show log" "l" #'TeX-recenter-output-buffer
      )
(map! :map LaTeX-mode-map
      :localleader
      :desc "Compile" "c" #'TeX-command-run-all
      :desc "Compile" "C" #'TeX-command-master
      :desc "View" "v" #'TeX-view
      :desc "Next error" "'" #'TeX-next-error
      :desc "Show log" "l" #'TeX-recenter-output-buffer
      )

;; First create a command that does nothing in the LaTeX command list
(after! latex
  ;; Function to display a message when compilation is done
  (defun TeX-view-message (a b c)
    (message "Compilation finished"))
  (setq TeX-command-list (append TeX-command-list
                                 '(("Do nothing" "" TeX-view-message))))
  )
;; The following needs to be set separately for some reason
;; I assume this again has something to do with the many mode names of AUCTeX
(add-hook! 'LaTeX-mode-hook
  (setq TeX-command-Show "Do nothing")
  )

(setq +latex-viewers '(pdf-tools))

(general-define-key
 :states '(normal insert)
 :keymaps 'LaTeX-mode-map
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "]" 'helm-bibtex)

(use-package! helm-bibtex
  :defer t
  :custom
  (bibtex-completion-bibliography
        '("~/Dropbox/Documents/working/library.bib"))
  (bibtex-completion-library-path
   '("~/Dropbox/Documents/working/papers"))
  )

(map! :map pdf-view-mode-map
      :leader
      :localleader
      "s" #'pdf-view-set-slice-from-bounding-box)
