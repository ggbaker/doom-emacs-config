(setq user-full-name "Gary Baker"
      user-mail-address "gary.baker@wisc.edu")

;; Set light and dark themes
(defvar dark-theme 'doom-dracula)
(defvar light-theme 'doom-solarized-light)
;; Load default dark theme
(setq doom-theme dark-theme)
;; Toggle theme between light and dark
(defun toggle-dark-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) dark-theme)
      (load-theme light-theme)
    (load-theme dark-theme))
  )
;; keybinding to toggle theme
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle dark theme" "t" #'toggle-dark-theme))

(setq doom-font (font-spec
                 :family "Hack Nerd Font"
                 :size 13
                 :weight 'light))
;; Fix issue with powerline fonts not rendering properly
(setq doom-emoji-fallback-font-families nil)

(setq display-line-numbers-type t)

(setq fill-column 79)
(setq visual-fill-column-width 85)

(setq +zen-text-scale 0.6)

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

;; (setq telephone-line-lhs
;;       '((evil   . (telephone-line-evil-tag-segment))
;;         (accent . (telephone-line-vc-segment
;;                    telephone-line-process-segment))
;;         (nil    . (telephone-line-buffer-modified-segment
;;                    telephone-line-buffer-name-segment))))
;; (setq telephone-line-rhs
;;       '((nil    . (telephone-line-flycheck-segment))
;;         (accent . (telephone-line-major-mode-segment))
;;         (evil   . (telephone-line-airline-position-segment))))

;; (telephone-line-mode t)

(setq-default delete-by-moving-to-trash t)

(setq langtool-language-tool-jar "/home/gbaker/.local/LanguageTool/languagetool-commandline.jar")

;; (use-package lsp-ltex
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-ltex)
;;                        (lsp)))
;;   :init
;;   (setq lsp-ltex-version "15.2.0")
;;   ;; disable spell-checking. Leave that to flyspell
;;   ;; (Can't get lsp-ltex to work with custom dict words)
;;   (setq lsp-ltex-disabled-rules
;;               #s(hash-table size 30 data
;;                         ("en-US" ["MORFOLOGIK_RULE_EN_US"]
;; 			  "es" ["MORFOLOGIK_RULE_ES"])
;; 			))
;;   )

;; (setq flycheck-checker-error-threshold 1500)

;; (after! lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(org-mode . "org")))

(map! :m "<up>" #'evil-previous-visual-line)
(map! :m "<down>" #'evil-next-visual-line)

(map! :leader
      :desc "Undo tree" "U" #'undo-tree-visualize)

(map! :leader
      :desc "vterm" "v" #'vterm)

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
      (:prefix ("TAB" . "workspace")
       :desc "Rename workspace" ","
       #'+workspace/rename))

;; create new frame
(map! :leader
      (:prefix ("w" . "window")
       :desc "Create new frame" "F" #'make-frame))
;; Switch to next frame
(map! :leader
      (:prefix ("w" . "window")
       :desc "Next frame" "f" #'+evil/next-frame))

(map! :leader
     (:prefix ("f" . "file")
      :desc "Toggle neotree" "t" #'neotree-toggle
      :desc "Current directory neotree" "T" #'neotree-dir))

(after! company
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") nil)
  )

(after! undo-tree
    (setq undo-tree-auto-save-history nil))

;; (setq undo-tree-history-directory-alist '(("." . ".undo-hist/")))

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

(setq LaTeX-electric-left-right-brace t)

(setq-default TeX-engine 'xetex)

(defun run-LatexMk ()
  (interactive)
  (TeX-command "LatexMk" 'TeX-master-file))

(defun latex-compile-bindings ()
  (interactive)
  (map! :map latex-mode-map
        :localleader
        :desc "Compile" "m" #'run-LatexMk
        :desc "Run a command" "c" #'TeX-command-master
        :desc "Next error" "'" #'TeX-next-error
        :desc "Show log" "l" #'TeX-recenter-output-buffer
        )
  (map! :map LaTeX-mode-map
        :localleader
        :desc "Compile" "m" #'run-LatexMk
        :desc "Run a command" "c" #'TeX-command-master
        :desc "Next error" "'" #'TeX-next-error
        :desc "Show log" "l" #'TeX-recenter-output-buffer
        )
  )
;; Above bindings partially conflict with defaults.
;; Load after latex to avoid being overwritten
(add-hook! LaTeX-mode #'latex-compile-bindings)

(setq +latex-viewers '(pdf-tools))

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
