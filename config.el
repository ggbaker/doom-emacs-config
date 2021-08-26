(setq user-full-name "Gary Baker"
      user-mail-address "gary.baker@wisc.edu")

(setq doom-theme 'doom-palenight)

(setq doom-font (font-spec
                 :family "Hack Nerd Font"
                 :size 13
                 :weight 'light))

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

(setq bibtex-completion-bibliography
      '("~/Dropbox/Documents/working/library.bib"))
(setq bibtex-completion-library-path
      '("~/Dropbox/Documents/working/papers"))

(setq  ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

(add-hook! org-mode #'visual-fill-column-mode)

(add-hook! org-mode #'org-superstar-mode)

(setq org-directory "~/Dropbox/Documents/working/org")

(after! org-roam
  (setq org-roam-directory "~/Dropbox/Documents/working/roam/")
)

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  ;; :custom
  ;; (orb-templates
  ;;  '(("r" "ref" plain #'org-roam-capture--get-point "" :file-name "${citekey}" :head "#+title: ${title}\n#+roam_alias: ${citekey}\n#+roam_key: ${ref}\n"
  ;;     :unnarrowed t)))
  ;; (org-roam-capture-templates
  ;;  '(("r" "ref" plain #'org-roam-capture--get-point "" :file-name "${citekey}" :head "#+title: ${title}\n#+roam_alias: ${citekey}\n#+roam_key: ${ref}\n"
  ;;      :unnarrowed t)))
  )

(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("r" . "roam")
       :desc "Toggle Roam display" "'" #'org-roam-buffer-toggle-display))
      )

(use-package! org-ref
  :after ivy-bibtex
  :custom
  (org-ref-default-bibliography '("~/Dropbox/Documents/working/library.bib"))
  (org-ref-pdf-directory "~/Dropbox/Documents/working/papers/")
  )

(setq org-roam-capture-templates
      '(;; default template
        ("d" "default" plain "%?" :if-new
         (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ;; bibliography note template
        ("r" "bibliography reference" plain "%?"
        :if-new
        (file+head "${citekey}.org" "#+title: (${citekey}) ${title}\n")
        :unnarrowed t)))

(general-define-key
 :states '(normal insert)
 :keymaps 'org-mode-map
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "]" 'ivy-bibtex)

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

(map! :map org-mode-map
      :localleader
      :desc "Toggle LaTeX fragments" "v" #'org-latex-preview)

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

(map! :map cdlatex-mode-map
    :i "TAB" #'cdlatex-tab)

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

(setq +latex-viewers '(pdf-tools))
(when (not window-system)
  (setq +latex-viewers '())
  (add-hook! LaTeX-mode
    (setq TeX-view-program-selection (remove '(output-pdf "Evince") TeX-view-program-selection))
    (setq TeX-view-program-selection (remove '(output-pdf "preview-pane") TeX-view-program-selection))))

(general-define-key
 :states '(normal insert)
 :keymaps 'LaTeX-mode-map
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "]" 'ivy-bibtex)

(add-hook! reftex-mode
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))

(map! :map pdf-view-mode-map
      :leader
      :localleader
      "s" #'pdf-view-set-slice-from-bounding-box)
