;; -*- mode: emacs-lisp; lexical-binding: t ; coding: utf-8 -*-
(setq lexical-binding t)

;;; Commentary: enabled lexical-binding. Please pay attention to using setq.

;;; Packages Setting Start
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))
;; Marmaladeを追加
;; (add-to-list 'package-archives  '("marmalade" . "https://marmalade-repo.org/packages/"))
;; 初期化
(package-initialize)
(require 'use-package)
;;; Packages Setting End

;;; GC
(setq gc-cons-threshold (* 512 1024 1024))

;;; Code:
(setq debug-on-error nil)
;; tab width
(setq-default indent-tabs-mode nil) ; tab => SPC
(setq-default tab-width 2)
;; 行末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; patch start
(setq text-quoting-style 'straight)
;;; patch end

;;; key translation start
;; Hyper
;; Backspace
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; kill back word
(define-key key-translation-map (kbd "M-h") (kbd "<M-DEL>"))
;; C-c y
(define-key key-translation-map (kbd "<muhenkan>") (kbd "C-c y"))
;; C-x
(define-key key-translation-map (kbd "<hiragana-katakana>") (kbd "C-x"))
(global-set-key (kbd "M-w") 'easy-kill)

;;; key translation end

;;; Screen Settings Start
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

;;; マウスカーソルを消す設定
(setq w32-hide-mouse-on-key t)
(setq w32-hide-mouse-timeout 5000)

;;; hide welcome screen
(setq inhibit-startup-message t)

;;; font-lockの設定
(global-font-lock-mode t)

;; 領域削除:領域選択状態でCtrl+Dで削除
(delete-selection-mode 1)

;; スクリーンの最大化
(set-frame-parameter nil 'fullscreen 'maximized)
;; 初期フレームの設定 Meadow風
(setq default-frame-alist
      (append (list '(foreground-color . "black")
		    '(background-color . "lemonchiffon")
		    '(border-color . "black")
		    '(mouse-color . "white")
		    '(cursor-color . "black")
		    '(width . 150)
		    '(height . 36)
		    '(top . 0)
		    '(left . 0)
		    )
	      default-frame-alist))

;; active/in-active mode-line color
(set-face-foreground 'mode-line "gray20")
(set-face-background 'mode-line "pink")
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "gray85")

;;; linum-mode
(global-linum-mode t)
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))
(line-number-mode 1)
(column-number-mode 1)
(setq linum-format "%3d ")

;; 括弧のハイライト
(show-paren-mode nil) ; これを設定しないとmultiple cursorsで順にハイライトされて重い
(setq show-paren-style 'mixed) ; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-delay 1)
(set-face-attribute 'show-paren-match nil
      :background "#DCC1A3")

;;; Screen Settings End

;;; command start
(setq grep-command "rg ")

;;; command end

;;;utils Start
(defun dateAdd(ymd add &optional pad)
  (interactive )
  (format-time-string
   (or pad "%Y-%m-%d")
   (time-add (date-to-time (concat ymd "T00:00:00"))
	     (days-to-time add))))

(defun counter-factory(&optional init inc)
  (let ((count (or init 0)) (increase (or inc 1)))
    #'(lambda (&optional cmd)
        (case cmd
          ((peek :peek) count)
          ((reset :reset) (setq count (or init 0)))
          (t (setq count (+ count increase)) count)))))

(defun string-factory(&optional head &optional sep)
  (let ((stored (or head ""))(mysep (or sep "\n")))
    #'(lambda(&optional adder)
	  (setq stored (concat stored mysep (or adder ""))))))

(defun insert-current-ymd(&optional stl)
  (interactive)
  (insert
  (format-time-string
   (or stl "%Y-%m-%d")
   (current-time))))

(defun getfilename ()
  (interactive)
  (insert
   (file-name-nondirectory
    (file-name-sans-extension
     (buffer-file-name)))))

(defun delete-word()
  (interactive)
  (let ((beg (point)))
    (forward-word 1)
    (delete-region beg (point))))

(defun mark-pre-word()
  (interactive)
  (set-mark (point))
  (backward-word 1))
(global-set-key (kbd "C-@") 'mark-pre-word)

(defun snake-to-camel (s &optional capitalize)
  ""
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "_\\([a-zA-Z]\\)"
     (lambda (s2)(upcase (substring s2 1)))
     (concat (if capitalize "_" "") s))))

(defun capitalze-first-char (&optional string)
  "Capitalize only the first char of the input"
  (when (and string (> (length string) 0))
    (let ((first-char (substring string 0 1))
          (rest-str (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun downcase-first-char (&optional string)
  "down fitst char"
  (when (and string (> length string) 0)
    (let ((first-char (substring string 0 1))
          (rest-str (substring string 1)))
      (concat (downcase first-char) rest-str))))

(defun space-to-camel (s)
  ""
  (snake-to-camel (replace-regexp-in-string " " "_" s)))

(defun space-to-snake (s)
  ""
  (replace-regexp-in-string " " "_" s))

(defun camel-to-snake (s)
  (save-match-data
    (let ((case-fold-search nil))
      (camel-to-snake-recursively s))))

(defun camel-to-snake-recursively (s)
  (let ((ss (downcase-first s)))
    (if (string-match "[A-Z]" ss)
        (concat (substring ss 0 (match-beginning 0)) "_" (camel-to-snake-recursively (substring ss (match-beginning 0) (length ss))))
      ss)))

(defun downcase-first (s)
  (if (zerop (length s))
      s
      (concat
       (downcase (substring s 0 1))
       (substring s 1 (length s)))))

(defun toggle-camelcase-underscores ()
  "Toggle between camcelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start end)))))

(defun dateAdd(ymd add &optional pad)
  (interactive)
  (format-time-string
   (or pad "%Y-%m-%d")
   (time-ad (date-to-time (concat ymd "T00:00:00"))
            (days-to-time add))))

(defun my/filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (let* ((no-kill-buffer-names
          ;; 消さないバッファ名を指定
          (list (buffer-name (current-buffer))
                "*Messages*" "*Compile-Log*" "*Help*"
                "*init log*" "*Ibuffer*" "*scratch*"
                "*MULTI-TERM-DEDICATED*"))
         (interested-buffers
          (my/filter
           '(lambda (buffer)
              (and
               ;; donk kill buffers who has the windows displayed in
               (not (get-buffer-window (buffer-name buffer)))
               ;; dont kill hidden buffers (hidden buffers' name starts with SPACE)
               (not (string-match "^ " (buffer-name buffer)))
               ;; dont kill buffers who have running processes
               (let ((proc (get-buffer-process buffer)))
                 (if proc
                     (equal 'exit
                            (process-status
                             (get-buffer-process buffer)))
                   t))))
           (buffer-list)))
         (buffers-to-kill
          (set-difference interested-buffers
                          (mapcar '(lambda (buffer-name)
                                     (get-buffer buffer-name))
                                  no-kill-buffer-names))))
    (mapc 'kill-buffer buffers-to-kill)))

(defun my/get-current-path ()
  (if (equal major-mode 'dired-mode-map)
      default-directory
    (or buffer-file-name (buffer-name))))

(defun my/copy-current-path ()
  (interactive)
  ""
  (let ((fPath (my/get-current-path)))
    (when fPath
      (kill-new (file-truename fPath)))))

(global-set-key (kbd "C-c f") 'my/copy-current-path)

(defun my/copy-filename ()
  (interactive)
  (kill-new
   (file-name-nondirectory
    (buffer-file-name))))
(global-set-key (kbd "C-c b") 'my/copy-filename)

(defun insert-current-time ()
  (interactive)
  (inset (format-time-string "%Y-%m-%d" (current-time))))
(global-set-key (kbd "C-c d") 'insert-current-time)

(defun string-to-number-1(s)
  ""
  (- (string-to-number s) 1))

;; repeat yank
(defun repeat-yank (num)
  (interactive "NRepeat Count > ")
  (dotimes (i num)
    (yank)
    (insert "\n")))
;; repeat yank settings
(global-set-key (kbd "C-x C-y ") 'repeat-yank)

;; scroll
(defun scroll-down-with-cursor(n)
  (interactive "p")
  (forward-line n)
  (scroll-up n))
(defun scroll-up-with-cursor(n)
  (interactive "p")
  (forward-line (- n))
  (scroll-down n))
;; (global-set-key (kbd "M-n") 'scroll-down-with-cursor)
;; (global-set-key (kbd "M-p")   'scroll-up-with-cursor)
(global-set-key (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda() (interactive) (scroll-down 1)))

;;; utils End

;; japanese input 二重のnn で ん
(setq quail-japanese-use-double-n t)

;; ;;; shell Settings Start

;; ;;; Cygwin の bash を使う場合
;; (setq explicit-shell-file-name "bash")
;; (setq shell-file-name "sh")
;; (setq shell-command-switch "-c")

;; ;;; Virtually UN*X!にある tcsh.exe を使う場合
;; (setq explicit-shell-file-name "tcsh.exe")
;; (setq shell-file-name "tcsh.exe")
;; (setq shell-command-switch "-c")

;; ;;; WindowsNT に付属の CMD.EXE を使う場合。
;; (setq explicit-shell-file-name "CMD.EXE")
;; (setq shell-file-name "CMD.EXE")
;; (setq shell-command-switch "\\/c")

;;; Powershell を使用する場合
;;(setq shell-file-name "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe")
;;(setq shell-command-switch "-Command"  )

;; ;;; grep := Powershell Select-String
;(setq grep-command "grep  ")
;(setq grep-find-command "grep_find  ")

;; ;;; shell Settings End

;; my file diff
;; (require 'cl)
;; (defun* ediff(ff fs &optional (f_char "Default") &optional s_char)
;;   (interactive "f Input comparing 1st:\nf 2nd: \ns 1st encode: \ns 2nd encode: ")
;;   (shell-command (concat "diff $(get-content -Encoding "
;; 			 (if(= (length f_char) 0)
;; 			    "Default"
;; 			   f_char)
;; 			 " " ff ") $(get-content -Encoding "
;; 			 (if(= (length s_char) 0)
;; 			    "Default"
;; 			   s_char)
;; 			 " " fs ")")))

;; (defun* ediff-buffers(buf0 buf1)
;;   (interactive "b Input 1st Buffer:\nb 2nd:")
;;   (set-buffer buf0)
;;   (setq firstBuffer (buffer-string))
;;   (set-buffer buf1)
;;   (setq secondBuffer (buffer-string))
;;   (shell-command (concat "$firstBuffer=$(Convert-HString @\"\n" firstBuffer "\n\"@); $secondBuffer=$(Convert-HString @\"\n" secondBuffer "\n\"@); diff $firstBuffer $secondBuffer" ))
;;   )

;;; Dired settings Start

(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "C-s") 'phi-search-dired)
;;; 表示を整えるアドバイス
(defun phi-search-dired-restrict-to-matches--show-all ()
  (with-selected-window (minibuffer-selected-window)
    (when (re-search-backward " \\.\\./?$" nil t)
      (forward-line 1)
      (recenter nil))))
(advice-add 'phi-search-dired-restrict-to-matches :after
            'phi-search-dired-restrict-to-matches--show-all)

;; ;; diredでWindowsに関連付けられたAppを起動する
;; (defun uenox-dired-winstart ()
;;  "Type '[uenox-dired-winstart]': win-start the current line's file."
;;  (interactive)
;;  (if (eq major-mode 'dired-mode)
;;  (let ((fname (dired-get-filename)))
;;  (w32-shell-execute "open" fname)
;;  (message "win-started %s" fname))))

;; (defun explorer (&optional path)
;;   "引数があればそのパスの、引数が省略されていれば現在のバッファのファイルを、explorerで開きます。"
;;   (interactive)
;;   (setq path (expand-file-name (or path (buffer-file-name))))
;;   (cond
;;     ((not (file-exists-p path))
;;      (message "path %s isn't exist" path))
;;     (t
;;      (let ((dos-path (replace-regexp-in-string "/" "\\\\" path)))
;;        (w32-shell-execute "open" "explorer.exe" dos-path)))));;(concat "/select," dos-path) => dos-pathを選択した状態で開く
;; (defun dired-exec-explorer ()
;;   "In dired, execute Explorer"
;;   (interactive)
;;   (message "%s" (dired-current-directory))
;;   (explorer (dired-current-directory)))
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)
;;; dired のkey割り当て追加
(add-hook 'dired-mode-hook
 (lambda ()
   (linum-mode 0)
   ;; (define-key dired-mode-map (kbd "C-c w") 'uenox-dired-winstart)
   ;; (define-key dired-mode-map (kbd "C-c e") 'dired-exec-explorer)
   (dired-hide-details-mode)
;;; これでdired-launch-modeが有効になり[J]が使える
   (dired-launch-enable)
))

; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)
;; wdired : enable change permissions
(setq wdired-allow-to-change-permissions t)

(global-set-key (kbd "C-x C-j ") 'dired-jump)

;;; Dired Settings End

;; emacs-lisp
(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))

(global-unset-key (kbd "C-x C-c"))
;; key-bind_s Settings End

;; IME
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")

;; migemo Settintg Start
(require 'migemo)
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8)
(load-library "migemo")
(migemo-init)

;; migemo Settintg End

;;
;; anythingの設定
;;
;(global-set-key (kbd "C-x C-b") 'anything-for-files)
;(global-set-key (kbd "M-y") 'anything-show-kill-ring)
;(global-set-key (kbd "C-x M-x") 'anything-M-x)

;; helm settings
(require 'helm-anything nil t)
(require 'helm)

(setq helm-buffer-max-length 100)
(helm-migemo-mode 1)
;; ;;; この前にmigemoの設定が必要
;; (require 'helm-migemo)
;; ;;; この修正が必要
;; (with-eval-after-load "helm-migemo"
;;   (defun helm-compile-source--candidates-in-buffer (source)
;;     (helm-aif (assoc 'candidates-in-buffer source)
;;         (append source
;;                 `((candidates
;;                    . ,(or (cdr it)
;;                           (lambda ()
;;                             ;; Do not use `source' because other plugins
;;                             ;; (such as helm-migemo) may change it
;;                             (helm-candidates-in-buffer (helm-get-current-source)))))
;;                   (volatile) (match identity)))
;;       source))
;;   ;; [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
;;   (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
;;   (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))

(require 'helm-swoop)
;;; isearchからの連携を考えるとC-r/C-sにも割り当て推奨
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)

;;; [2015-03-23 Mon]C-u C-s / C-u C-u C-s
 (defun isearch-forward-or-helm-swoop (use-helm-swoop)
   (interactive "p")
   (let (current-prefix-arg
         (helm-swoop-pre-input-function 'ignore))
     (call-interactively
      (case use-helm-swoop
	(1 'isearch-forward)
        (4 'helm-swoop)
        (16 'helm-swoop-nomigemo)))))
(global-set-key (kbd "C-s") 'isearch-forward-or-helm-swoop)
(setq helm-swoop-move-to-line-cycle t)

(cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
  "シンボル検索用Migemo無効版helm-swoop"
  (interactive)
  (let ((helm-swoop-pre-input-function
         (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol)))))
    (helm-swoop :$source (delete '(migemo) (copy-sequence (helm-c-source-swoop)))
                :$query $query :$multiline $multiline)))

;;; [2014-11-25 Tue]
;; (when (featurep 'helm-anything)
;;   (defadvice helm-resume (around helm-swoop-resume activate)
;;     "helm-anything-resumeで復元できないのでその場合に限定して無効化"
;;     ad-do-it))

;;; ace-isearch
(global-ace-isearch-mode t)
(setq ace-isearch-input-length 6)

;;; resumable helm/anything buffers
(defvar helm-resume-goto-buffer-regexp
  (rx (or (regexp "Helm Swoop") "helm imenu" (regexp "helm.+grep") "helm-ag"
          "occur"
          "*anything grep" "anything current buffer")))
(defvar helm-resume-goto-function nil)
(defun helm-initialize--resume-goto (resume &rest _)
  (when (and (not (eq resume 'noresume))
             (ignore-errors
               (string-match helm-resume-goto-buffer-regexp helm-last-buffer)))
    (setq helm-resume-goto-function
          (list 'helm-resume helm-last-buffer))))
(advice-add 'helm-initialize :after 'helm-initialize--resume-goto)
(defun anything-initialize--resume-goto (resume &rest _)
  (when (and (not (eq resume 'noresume))
             (ignore-errors
               (string-match helm-resume-goto-buffer-regexp anything-last-buffer)))
    (setq helm-resume-goto-function
          (list 'anything-resume anything-last-buffer))))
(advice-add 'anything-initialize :after 'anything-initialize--resume-goto)

;;; next-error/previous-error
(defun compilation-start--resume-goto (&rest _)
  (setq helm-resume-goto-function 'next-error))
(advice-add 'compilation-start :after 'compilation-start--resume-goto)
(advice-add 'occur-mode :after 'compilation-start--resume-goto)
(advice-add 'occur-mode-goto-occurrence :after 'compilation-start--resume-goto)
(advice-add 'compile-goto-error :after 'compilation-start--resume-goto)

;; (defun helm-resume-and- (key)
;;   (unless (eq helm-resume-goto-function 'next-error)
;;     (if (fboundp 'helm-anything-resume)
;;         (setq helm-anything-resume-function helm-resume-goto-function)
;;       (setq helm-last-buffer (cadr helm-resume-goto-function)))
;;     (execute-kbd-macro
;;      (kbd (format "%s %s RET"
;;                   (key-description (car (where-is-internal
;;                                          (if (fboundp 'helm-anything-resume)
;;                                              'helm-anything-resume
;;                                            'helm-resume))))
;;                   key)))
;;     (message "Resuming %s" (cadr helm-resume-goto-function))
;;     t))
(defun helm-resume-and-previous ()
  "Relacement of `previous-error'"
  (interactive)
  (or (helm-resume-and- "C-p")
      (call-interactively 'previous-error)))
(defun helm-resume-and-next ()
  "Relacement of `next-error'"
  (interactive)
  (or (helm-resume-and- "C-n")
      (call-interactively 'next-error)))

;;; Replace: next-error / previous-error
(require 'helm-config)
;; (ignore-errors (helm-anything-set-keys))
(global-set-key (kbd "M-g M-n") 'helm-resume-and-next)
(global-set-key (kbd "M-g M-p") 'helm-resume-and-previous)


;; helm key settings
(global-set-key (kbd "C-c q") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c m") 'helm-imenu)
(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-h") 'delete-backward-char)))

(eval-after-load 'helm-files
  '(progn
     (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
     (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
     (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))  )
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))
;; 表示するファイルの数
(setq recentf-max-menu-items 30)
;; 保存するファイルの数
(setq recentf-max-saved-items 50)
;; kill-ring で保存される最大値
(setq kill-ring-max 100)

;;; helm Settings End

;; markdown-mode Settings Start
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mkdn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode) )
;; hooking outline-minor-mode
(add-hook 'markdown-mode-hook
          '(lambda () (outline-minor-mode t)))

;; markdown-mode Settings End

;; Scala deprecated
;; (setq use-package-always-defer t
;;       use-package-always-ensure t
;;       backup-directory-alist `((".*" . ,temporary-file-directory))
;;       auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; ;; Enable scala-mode and sbt-mode
;; (use-package scala-mode
;;   :mode "\\.s\\(cala\\|bt\\)$")

;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map)
;;    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
;;    (setq sbt:program-options '("-Dsbt.supershell=false"))
;; )

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

;; Add company-lsp backend for metals
(use-package company-lsp)

;; color-moccur
;;(when (require 'color-moccur nil t)
;;  (define-key global-map (kbd "M-o") 'occur-by-moccur)
;;  (setq moccur-split-word t)
;;  (add-to-list 'dmoccur-exclusion-mask "//.DS_Store")
;;  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
;;)

;; redo+
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-?") 'redo))
;; undo-hist
(when (require 'undohist nil t)
  (undohist-initialize))
(require 'undohist)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("/tmp/" "COMMIT_EDITMSG"))
;;; NTEmacsだと動かないらしいので再定義
;;; http://d.hatena.ne.jp/Lian/20120420/1334856445
(when (eq system-type 'windows-nt)
  (defun make-undohist-file-name (file)
    (setq file (convert-standard-filename (expand-file-name file)))
    (if (eq (aref file 1) ?:)
        (setq file (concat "/"
                           "drive_"
                           (char-to-string (downcase (aref file 0)))
                           (if (eq (aref file 2) ?/)
                               ""
                             (if (eq (aref file 2) ?\\)
                                 ""
                               "/"))
                           (substring file 2))))
    (setq file (expand-file-name
                (subst-char-in-string
                 ?/ ?!
                 (subst-char-in-string
                  ?\\ ?!
                  (replace-regexp-in-string "!" "!!"  file)))
                undohist-directory))))
;; undo-tree
(when (require 'undo-tree nil t)
   (global-undo-tree-mode))
;; point-undo
;; (when (require 'point-undo nil t)
;;   (global-set-key (kbd "M-[") 'point-undo)
;;   (global-set-key (kbd "M-]") 'point-redo))


;;; Org Settings Start
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (org-remember-insinuate)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "memo.org"))
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline nil "Inbox")
         "** TODO %?\n   %i\n   %a\n   %t")
        ("b" "Bug" entry
         (file+headline nil "Inbox")
         "** TODO %?   :bug:\n   %i\n   %a\n   %t")
        ("i" "Idea" entry
         (file+headline nil "New Ideas")
         "** %?\n   %i\n   %a\n   %t")
	("p" "ProjectTask" entry(file+headline(expand-file-name "~/Dropbox/org/project/project.org") "Inbox")
         "** TODO %?\n   %i\n   %a\n   %t")
	))
;;agendaを使用
(require 'org-agenda)
(dolist (file '("agenda.org" "archive.org" "memo.org" "mobilepull.org"))
  (add-to-list 'org-agenda-files (concat org-directory file)))
(add-to-list 'org-agenda-files "~/Dropbox/org/project/project.org")
;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;; DONEの時刻を記録
(setq org-log-done 'time)
;; TODO をCalendarに出力
(setq org-icalendar-include-todo t)
;; Archive
(setq org-archive-location "~/Dropbox/org/archive.org::")
(defun my:org-archive-done-tasks ()
  (interactive)
  ;; ARCHIVE タグを付けるだけなら以下
  ;;   (org-map-entries 'org-archive-set-tag "/DONE" 'file))
  ;; org-archive-location に refile したいなら以下
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
(add-hook 'org-todo-statistics-hook 'my:org-archive-done-tasks)
(add-hook 'org-todo-after-statistics-hook 'my:org-archive-done-tasks)

(setq org-mobile-inbox-for-pull "~/Dropbox/org/mobilepull.org") ;pullしたデータの置き場
(setq org-mobile-directory "~/Dropbox/アプリ/MobileOrg") ;Androidのディレクトリ
(setq org-mobile-files (list "~/Dropbox/org/agenda.org"
			     "~/Dropbox/org/memo.org"))

;; org project
(setq org-publish-project-alist
      '(("etc"
	 :base-directory "~/draft/"
	 :base-extension "org"
	 :publishing-directory "~/html/"
	 :publishing-function org-html-publish-to-html
	 :recursive t)))
(setq org-publish-use-timestamps-flag t)

;; html
(setq org-html-html5-fancy t)
(setq org-html-doctype "html5")
(setq org-html-use-infojs nil)
(setq org-html-allow-name-attribute-in-anchors nil)
(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "htmlized-")
(setq org-html-text-markup-alist
      '((bold           . "<strong>%s</strong>")
        (italic         . "<em>%s</em>")
        (code           . "<code>%s</code>")
        (strike-through . "<span style=\"strike;\">%s</span>")
        (underline      . "<span class=\"underline\">%s</span>")
        (verbatim       . "<code>%s</code>")))

;; OpenDocument
(require 'ox-odt)

;; hilight
(setq org-src-fontify-natively t)

;; ショートカットキー
(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c o" 'org-capture)
(global-set-key "\C-c a" 'org-agenda)
; x(global-set-key "\C-c b" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)
;; org-modeではExcelへの貼り付けやすさのため､階層は全てTabインデント
(add-hook 'org-mode-hook
          (function (lambda ()
		      (setq org-list-indent-offset 1)
                      (setq tab-width 2)
                      (setq indent-tabs-mode t))))

;; iCalendar
(setq org-icalendar-include-todo t)
(setq org-combined-agenda-icalendar-file "~/Dropbox/アプリ/MobileOrg/calendar/my_schedule.ics")
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))

(require 'holidays)
(eval-after-load "holidays"
  '(progn
     (require 'japanese-holidays)
     (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
           (append japanese-holidays holiday-local-holidays holiday-other-holidays))
     (setq mark-holidays-in-calendar t) ; 祝日をカレンダーに表示
     ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
     ;; デフォルトで設定済み
     (setq japanese-holiday-weekend '(0 6)     ; 土日を祝日として表示
           japanese-holiday-weekend-marker     ; 土曜日を水色で表示
           '(holiday nil nil nil nil nil japanese-holiday-saturday))
     (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
     (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)))

;;“きょう”をマークするには以下の設定を追加します。
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(require 'org-habit)

(defun my-org-reload ()
  (interactive)
  (switch-to-buffer "agenda.org")
  (revert-buffer nil 1)
  (org-agenda nil "a")
  (delete-other-windows))

(setq browse-url-browser-function 'browse-url-msie)
(setq browse-url-msie-program "IEXPLORE.EXE")
(defun browse-url-msie (url)
  (interactive (browse-url-interactive-arg "URL: "))
  (if browse-url-msie-program
      (let ((w32-start-process-show-window t))
	(start-process (concat browse-url-msie-program url)
		       nil browse-url-msie-program url))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (scala . t)
   ))
;;; Org Settings End

;; Region
(require 'region-bindings-mode)
(region-bindings-mode-enable)

;; Multiple Cursors Settings Start
(require 'phi-search-migemo)
(require 'multiple-cursors)
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map "u" 'mc/unmark-next-like-this)
(define-key region-bindings-mode-map "U" 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map "j" 'mc/skip-to-next-like-this)
(define-key region-bindings-mode-map "S" 'mc/skip-to-previous-like-this)
(define-key region-bindings-mode-map "c" 'my/mc/insert-chars)
(define-key region-bindings-mode-map "i" 'my/mc/insert-numbers)
(define-key region-bindings-mode-map "y" 'my/start-insert-ymd)
(define-key region-bindings-mode-map "s" 'phi-search-migemo)
(define-key region-bindings-mode-map "r" 'phi-search-migemo-backward)
(define-key region-bindings-mode-map "k" 'my/multi-copy)
(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "C-s") 'phi-search-migemo)
  (define-key mc/keymap (kbd "C-r") 'phi-search-migemo-backward))

(defun mc/edit-lines-or-string-rectangle (s e)
  "C-x r tで同じ桁の場合にmc/edit-lines (C-u M-x mc/mark-all-dwim)"
  (interactive "r")
  (if (eq (save-excursion (goto-char s) (current-column))
          (save-excursion (goto-char e) (current-column)))
      (call-interactively 'mc/edit-lines)
    (call-interactively 'string-rectangle)))
(global-set-key (kbd "C-x r t") 'mc/edit-lines-or-string-rectangle)

(defun mc/mark-all-dwim-or-mark-sexp (arg)
  "C-u C-M-SPCでmc/mark-all-dwim, C-u C-u C-M-SPCでC-u M-x mc/mark-all-dwim"
  (interactive "p")
  (cl-case arg
    (16 (mc/mark-all-dwim t))
    (4 (mc/mark-all-dwim nil))
    (1 (mark-sexp 1))))
(global-set-key (kbd "C-M-SPC") 'mc/mark-all-dwim-or-mark-sexp)

;; insert specific serial number
(defun my/mc/insert-numbers (start inc pad)
  "Insert increasing numbers for each cursor specifically."
  (interactive
   (list (read-number "Start from: " 0)
	 (read-number "Increment by: " 1)
	 (read-string "Padding (%01d): "  "%01d")))
  (let ((start_num start))
    (setq counter (counter-factory (- start_num inc) inc))
    (fset 'tmp_increase (lambda () (interactive)(insert (format pad (funcall counter)))))
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor
      'tmp_increase
      cursor))))

(defun my/mc/insert-chars (start inc)
  ""
  (interactive
   (list (read-string "Start from: " "a")
	 (read-number "Increment by: " 1)))
  (let ((start_char (string-to-char start))(base_char (string-to-char "a")))
    (setq counter (counter-factory -1 inc))
    (fset 'tmp_increase
	  (lambda () (interactive)
	    (insert-char (+
			 (% (+ (funcall counter)(- start_char base_char)) 26) base_char))))
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor
      'tmp_increase
      cursor))))

(defun my/start-insert-ymd(ymd inc)
  (interactive
   (list (read-string "ymd : " "2015-01-01")
	 (read-number "inc : " 1)))
  (let ((yyyymmdd ymd))
    (setq counter (counter-factory (- inc) inc))
    (fset 'tmp_date_add	(lambda () (interactive)(insert (dateAdd ymd (funcall counter)))))
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor
      'tmp_date_add
      cursor))))

(defun my/multi-copy()
  (interactive)
  (setq stored (string-factory))
  (fset 'tmp_kill(lambda()(interactive)(funcall stored (buffer-substring (region-beginning) (region-end)))))
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor
    'tmp_kill
    cursor))
  (message "%s" "multi copy")
  (kill-new (funcall stored)))

(defun start-multiple-cursors(num start)
  (interactive "NCursors num > \nsStart word > ")
  (set-mark-command nil)
  (let ((prepos (current-column)) (loop (- num 1)))
    (message "prepos : %s" prepos)
    (dotimes (i loop)
      (insert start )
      (if (equal (line-number-at-pos)  (count-lines (window-start) (window-end)))
	  (insert "\n") (progn (forward-line) (forward-char prepos))
	  )
      )
    (insert start )
    (end-of-line)
    (exchange-point-and-mark)
    (mc/edit-ends-of-lines)
    )
  )


;; multiple insert s
(global-set-key (kbd "C-c i n") 'my/mc/insert-numbers)
(global-set-key (kbd "C-c i y") 'my/start-insert-ymd)
(global-set-key (kbd "C-c i c") 'my/mc/insert-chars)
(global-set-key (kbd "C-M-SPC") 'mc/mark-all-dwim-or-mark-sexp)
(global-set-key (kbd "C-x r t") 'mc/edit-lines-or-string-rectangle)
(global-set-key (kbd "C-c c") 'mc/edit-lines)
(define-key mc/keymap (kbd "C-:") 'nil)
(define-key mc/keymap (kbd "C-,") 'mc/repeat-command)
(global-set-key (kbd "C-c s") 'start-multiple-cursors)

;;; multiple cursor end

;; eww
;;; my scroll key bindings
(add-hook 'eww-mode-hook
	  (lambda()
	    (local-unset-key (kbd "M-n"))
	    (local-unset-key (kbd "M-p"))))

;; yasnippet
(setq warning-suppress-types nil)
(require 'yasnippet)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-x i e") 'yas-visit-snippet-file)

(require 'popup)
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "C-n") 'popup-next)
(define-key popup-menu-keymap (kbd "C-p") 'popup-previous)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
;; flycheck popup
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; rainbow mode ;色指定を実際に見せてくれるrgba形式も対応してほすぃ
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;; 括弧の色をネスト順位に従って色づけ
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty" :foundry "unknown" :slant normal :weight normal :height 135 :width normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#000000" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#FF0010" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FF8D10" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#FFD700" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#5BfF3F" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#1BaF1F" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#66bbff" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#0C00CC" :weight bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8E00CC" :weight bold))))
 '(web-mode-current-element-highlight-face ((t (:background "#eeeeee")))))

;; making unmatched parens stand out more
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
        :foreground "#000000"
        :inherit 'error
        :strike-through t)

;;; Web-mode Settings Start
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml$'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp$'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (define-key web-mode-map (kbd "C-c /") 'web-mode-element-close)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("html" . (ac-sources))
	  ))
  (setq web-mode-enable-current-element-highlight t)
  (custom-set-faces '(web-mode-current-element-highlight-face ((t (:background "#eeeeee" )))))
  (defun web-mode-indent (num)
    (interactive "nIndent: ")
    (setq web-mode-markup-indent-offset num)
    (setq web-mode-css-indent-offset num)
    (setq web-mode-style-padding num)
    (setq web-mode-code-indent-offset num)
    (setq web-mode-script-padding num)
    (setq web-mode-block-padding num)
    )
  (web-mode-indent 2)
)

(add-hook 'web-mode-hook  'my-web-mode-hook)
;;; Web-mode Settings End

;;; js2 mode
(autoload 'js2-mode "js2-mode" nil t)
(add-hook 'js2-mode-hook (lambda()
			   (setq js2-basic-offset 2)
			   (setq tab-width 2)
			   (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . js2-mode))

;; ;;; tern mode
;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-company)
;;       (tern-ac-setup)))

;; (require 'popwin)
;; ;; helm bufferをpopupする
;; (setq helm-display-function #'display-buffer)
;; (when (require 'popwin)
;;   (setq display-buffer-function 'popwin:display-buffer)
;;   (setq popwin:special-display-config
;;     '(("*complitation*" :noselect t)
;;       ("helm" :regexp t :height 0.4))))

(require 'shackle)
(setq shackle-rules
      '(;; *compilation*は下部に2割の大きさで表示
        (compilation-mode :align below :ratio 0.2)
        ;; ヘルプバッファは右側に表示
        ("*Help*" :align right)
        ;; 補完バッファは下部に3割の大きさで表示
        ("*Completions*" :align below :ratio 0.3)
        ;; M-x helm-miniは下部に7割の大きさで表示
        ("*helm mini*" :align below :ratio 0.7)
        ;; 他のhelmコマンドは右側に表示 (バッファ名の正規表現マッチ)
        ("\*helm" :regexp t :align below)
        ))
(shackle-mode 1)
(setq shackle-lighter "")

;;; C-zで直前のウィンドウ構成に戻す
(winner-mode 1)
(global-set-key (kbd "C-c w b") 'winner-undo)
(global-set-key (kbd "C-c w f") 'winner-redo)

;;; company start
(require 'company)
;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-h") 'nil)
(define-key company-search-map (kbd "C-h") 'company-complete-common-or-cycle)
(define-key company-search-map (kbd "M-d") 'company-show-doc-buffer)
(setq company-mimimum-prefix-length 1)
(setq company-selection-wrap-around t)
(define-key company-mode-map (kbd "C-:") 'company-complete)

;;; company end

;;; persp mode Setting Start
(setq persp-keymap-prefix (kbd "C-c p")) ;prefix
(setq persp-add-on-switch-or-display t) ;バッファを切り替えたら見えるようにする
(persp-mode 1)
(defun persp-register-buffers-on-create ()
  (interactive)
  (dolist (bufname (condition-case _
                       (helm-comp-read
                        "Buffers: "
                        (mapcar 'buffer-name (buffer-list))
                        :must-match t
                        :marked-candidates t)
                     (quit nil)))
    (persp-add-buffer (get-buffer bufname))))
(add-hook 'persp-activated-hook 'persp-register-buffers-on-create)
;;; persp mode Setting End

;;; zoom-window start
(require 'zoom-window)
(setq zoom-window-use-persp t)
(zoom-window-setup)

(global-set-key (kbd "C-x 1") 'zoom-window-zoom)
(setq zoom-window-mode-line-color "DarkGreen")
;;; zoom-window end

;;;
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;; emmet Setting Start
(add-hook 'emmet-mode-hook
	  (lambda()
	    (local-unset-key (kbd "C-j"))
	    (setq emmet-indentation 1)
	    (setq tab-width 2)
	    (setq indent-tabs-mode nil)
	    (define-key emmet-mode-keymap (kbd "C-:") 'emmet-expand-line)))
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; keep default : newline
;;; emmet Setting End

;;; magit
(require 'magit)
(add-hook 'git-commit-mode-hook
          '(lambda ()
             (set-buffer-file-coding-system 'utf-8)))
(setenv "GIT_ASKPASS" "git-gui--askpass")
(setq magit-last-seen-setup-instructions "1.4.0")

;;; Display Time
;; 以下の書式に従ってモードラインに日付・時刻を表示する
(setq display-time-string-forms
      '((format "%s/%s/%s(%s) %s:%s" year month day dayname 24-hours minutes)
        load
        (if mail " Mail" "")))
;; 時刻表示の左隣に日付を追加。
(setq display-time-kawakami-form t)
;; 24時間制
(setq display-time-24hr-format t)
;; 時間を表示
(display-time)

;; isearch-dabbrev
(define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand)

;; visual-regexp
(global-set-key (kbd "M-%") 'vr/query-replace)

;; swipper
;; (defun isearch-forward-or-swiper (use-swiper)
;;   (interactive "P")
;;   (let (current-prefix-arg)
;;     (call-interactively (if use-swiper 'swiper 'isearch-forward))))
;;(global-set-key (kbd "C-s") 'isearch-forward-or-swiper)
;;; バックエンドのivyがスペースを".*"に置換してしまうため、無効にする
;;; これをしないと純粋に正規表現isearchの置き換えにならない
(require 'ivy)
(fset 'ivy--regex 'identity)

;;; 選択範囲をisearch
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;;; helm-ag
(require 'helm-config)
(require 'helm-files)
(require 'helm-ag)
;; helm-ag
;;; (setq helm-ag-base-command "pt --nocolor --nogroup")
(setq helm-ag-base-command "rg --no-heading -S -e")
;;; 現在のシンボルをデフォルトのクエリにする
(setq helm-ag-insert-at-point 'symbol)
;;; C-M-gはちょうどあいてる
(global-set-key (kbd "C-M-g") 'helm-ag)
(global-set-key (kbd "C-M-m") 'helm-do-ag)
(global-set-key (kbd "C-M-k") 'backward-kill-sexp) ;推奨

(defun helm-ag-dot-emacs ()
  ".emacs.d以下を検索"
  (interactive)
  (helm-ag "~/.emacs.d/"))
(require 'projectile nil t)
(defun helm-projectile-ag ()
  "Projectileと連携"
  (interactive)
  (helm-ag (projectile-project-root)))
;; (helm-ag "~/.emacs.d/")

;;; ace-window
;;(global-set-key (kbd "C-x o") 'ace-window)
;;(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;; win-switch
(require 'win-switch)
;;; 受け付けタイマー
(setq win-switch-idle-time 1.5)
;;; 好きなキーを複数割り当てられる
;; ウィンドウ切り替え
(win-switch-set-keys '("p") 'up)
(win-switch-set-keys '("n") 'down)
(win-switch-set-keys '("b") 'left)
(win-switch-set-keys '("f") 'right)
(win-switch-set-keys '("o") 'next-window)
(win-switch-set-keys '("a") 'previous-window)
;; リサイズ
(win-switch-set-keys '("P") 'enlarge-vertically)
(win-switch-set-keys '("N") 'shrink-vertically)
(win-switch-set-keys '("B") 'shrink-horizontally)
(win-switch-set-keys '("F") 'enlarge-horizontally)
;; 分割
(win-switch-set-keys '("3") 'split-horizontally)
(win-switch-set-keys '("2") 'split-vertically)
(win-switch-set-keys '("0") 'delete-window)
;; その他
(win-switch-set-keys '(" ") 'other-frame)
(win-switch-set-keys '("u" [return]) 'exit)
(win-switch-set-keys '("\M-\C-g") 'emergency-exit)
;; C-x oを置き換える
(global-set-key (kbd "C-x o") 'win-switch-dispatch)

;;; winner-mode
;(multicolumn-global-mode 1)
;(setq multicolumn-min-width 72)
;(define-key multicolumn-map (kbd "C-x 4 4")
; 'multicolumn-delete-other-windows-and-split-with-follow-mode)

(setq ido-enable-flex-matching t)
(ido-vertical-mode 1)

(require 'imenus)

;;; エラー対策
(defun imenu-find-default--or-current-symbol (&rest them)
  (condition-case nil
      (apply them)
    (error (thing-at-point 'symbol))))
(advice-add 'imenu-find-default :around 'imenu-find-default--or-current-symbol)
;;; なぜか現在のシンボルを取ってくれないから
(defun imenus-exit-minibuffer ()
  (exit-minibuffer))

;;; ido化: imenus/with-ido imenus-mode-buffers/with-idoを定義
(with-ido-completion imenus)
;; C-M-s C-M-sで現在のシンボルをhelm-multi-swoopできるよ！
(global-set-key (kbd "C-M-s") (with-ido-completion imenus-mode-buffers))


;;; M-oでのmulti-occurをシンボル正規表現にするよう改良
(push '(occur . imenus-ido-multi-occur) imenus-actions)
(defun imenus-ido-multi-occur (buffers input)
  (multi-occur buffers
               (format "\\_<%s\\_>"
                       (regexp-quote (replace-regexp-in-string "^.*|" "" input)))))

;;; C-M-sで関数呼び出しをhelm-multi-swoopできるようにした
(push '(helm-multi-swoop . imenus-helm-multi-swoop) imenus-actions)
(defun imenus-helm-multi-swoop (buffers input)
  (helm-multi-swoop (replace-regexp-in-string "^.*|" "" input)
                    (mapcar 'buffer-name buffers)))
(define-key imenus-minibuffer-map (kbd "C-M-s") 'imenus-exit-to-helm-multi-swoop)
(defun imenus-exit-to-helm-multi-swoop ()
  "Exit from imenu prompt; start `helm-multi-swoop' with the current input."
  (interactive)
  (setq imenus-exit-status 'helm-multi-swoop)
  (imenus-exit-minibuffer))

;; ;;; hiwin
;; (require 'hiwin)
;; (hiwin-activate)
;; (set-face-background 'hiwin-face "#ffdbb7")

;;; electric-operator
(require 'electric-operator)
(electric-operator-add-rules-for-mode
 'java-mode (cons "," ", ") (cons "=" " = ") (cons "==" " == ") (cons "->" " -> ") (cons "<" "<>") (cons ">" "> ") (cons ">(" ">()"))
(electric-operator-add-rules-for-mode
 'scala-mode (cons "=" " = ") (cons ":=" " := ") (cons "<=" " <= ") (cons ">=" " >= ") (cons "==" " == ") (cons "=>" " => ") (cons "," ", ") (cons "=:=" " =:= ")
 (cons "<:<" " <:< ") (cons ">:>" " >:> ") (cons ">:" " >: ") (cons "<:" " <: ") (cons "->" " -> ")
)
(electric-operator-add-rules-for-mode
 'web-mode (cons "," ", ") (cons "==" " === ") (cons "===" " === ") (cons "=>" " => "))
(electric-operator-add-rules-for-mode
 'js2-mode (cons "," ", ") (cons "==" " === ") (cons "===" " === ") (cons "=>" " => ") (cons "!=" " !== ") (cons ".." "."))
;; hooking
(add-hook 'java-mode-hook #'electric-operator-mode)
(add-hook 'scala-mode-hook #'electric-operator-mode)
(add-hook 'web-mode-hook #'electric-operator-mode)
(add-hook 'js2-mode-hook #'electric-operator-mode)

(add-hook 'scala-mode-hook
          (function (lambda ()
                      (setq tab-width 2)
                      (setq indent-tabs-mode nil)
		      (auto-revert-mode))))

(add-hook 'java-mode-hook
          (function (lambda ()
                      (setq tab-width 2)
                      (setq indent-tabs-mode nil)
		      (auto-revert-mode))))

;;; suceeding shell's path
(exec-path-from-shell-initialize)

;;; 括弧
(require 'smartparens-config)
(smartparens-global-mode t)
;;(sp-pair "<?" "?>")

;; (require 'tramp)
(setq tramp-default-method "scp")
(setq ange-ftp-try-passive-mode t)
;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '("localhost" nil nil))
;; (add-to-list 'tramp-default-proxies-alist
;;              '((regexp-quote (system-name)) nil nil))


;; tab jump mode
(setq yas-fallback-behavior '(apply tab-jump-out 1))

;; C++
;; (eval-after-load "irony"
;;   '(progn
;;      (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
;;      (add-to-list 'company-backends 'company-irony)
;;      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;     (add-hook 'c-mode-common-hook 'irony-mode)))

(eval-after-load "flycheck"
  '(progn
     (when (locate-library "flycheck-irony")
       (flycheck-irony-setup))))

;; flyspell
(setq flyspell-issue-message-flag nil)

(when (require 'rtags nil 'noerror)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (rtags-is-indexed)
                (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
                (local-set-key (kbd "M-;") 'rtags-find-symbol)
                (local-set-key (kbd "M-@") 'rtags-find-references)
                (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))
;; C++ end

(setq load-path
      (append '(
                "~/.emacs.d/conf"
                ) load-path))
;; ;; slack start
;; (load "slack-init")
;; ;; slack end

;; dropbox start
;; (load "dropbox-init")
;; dropbox end


;; webkit start

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
               (when (equal major-mode 'xwidget-webkit-mode)
		 (linum-mode 0)
                 (xwidget-webkit-adjust-size-dispatch))))

(add-hook 'xwidget-webkit-mode-hook
	  (function (lambda ()
		      (linum-mode 0)
		      ;; make these keys behave like normal browser
		      (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
		      (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
		      (define-key xwidget-webkit-mode-map (kbd "C-n") 'xwidget-webkit-scroll-down)
		      (define-key xwidget-webkit-mode-map (kbd "C-p") 'xwidget-webkit-scroll-up)
		      (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
		      (define-key xwidget-webkit-mode-map (kbd "C-/") 'xwidget-webkit-back)
		      (define-key xwidget-webkit-mode-map (kbd "C-l") 'xwidget-webkit-reload)
		      (define-key xwidget-webkit-mode-map (kbd "C-c C-s") 'xwidget-webkit-bookmark-make-record)
)))

;; by default, xwidget reuses previous xwidget window,
;; thus overriding your current website, unless a prefix argument
;; is supplied
;;
;; This function always opens a new website in a new window
(defun xwidget-browse-url-no-reuse (url &optional sessoin)
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             )))
  (xwidget-webkit-browse-url url t))

;; make xwidget default browser
(setq browse-url-browser-function (lambda (url session)
                    (other-window 1)
                    (xwidget-browse-url-no-reuse url)))
;; webkit end

;; mpv
(require 'mpv)
;;; Wiki(https://github.com/kljohann/mpv.el/wiki)より
;;; C-c C-lでmpv:を選択したらmvpのリンクを補完付きで入力できる
(org-add-link-type "mpv" #'mpv-play-and-prepare-memo)
(defun mpv-play-and-prepare-memo (path)
  (mpv-play path)
  (kill-new "- 0:00:00 :: start\n"))
(defun org-mpv-complete-link (&optional arg)
  (replace-regexp-in-string
   "file:" "mpv:"
   (org-file-complete-link arg)
   t t))

;;; 再生位置をM-RETで挿入させる
(defun org-timer-item--mpv-insert-playback-position (fun &rest args)
  "When no org timer is running but mpv is alive, insert playback position."
  (if (and
       (not org-timer-start-time)
       (mpv-live-p))
      (mpv-insert-playback-position t)
    (apply fun args)))
(advice-add 'org-timer-item :around
            #'org-timer-item--mpv-insert-playback-position)

;;; 0:01:02のような文字列でC-c C-oしたらその位置にジャンプさせる
(add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)

;;; 表示されてる時間の3秒前に飛ぶように再定義
(defun mpv-seek-to-position-at-point ()
  "Jump to playback position as inserted by `mpv-insert-playback-position'.

This can be used with the `org-open-at-point-functions' hook."
  (interactive)
  (save-excursion
    (skip-chars-backward ":[:digit:]" (point-at-bol))
    (when (looking-at "[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}")
      (let ((secs (max 0 (- (org-timer-hms-to-secs (match-string 0)) 3))))
        (when (>= secs 0)
          (mpv--enqueue `("seek" ,secs "absolute") #'ignore)
)))))

;;(global-set-key (kbd "H-SPC") 'mpv-pause)
;;(global-set-key (kbd "H-b") 'mpv-seek-backward)

(require 'page-ext)


;; paren close delimiter
(global-set-key (kbd "M-]") 'paren-completer-add-single-delimiter)

;; font
(add-to-list 'default-frame-alist '(font . "ricty-13.5"))

;; emacsclientを使えるようにする
(server-start)

;;; 日本語環境設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(eclim-eclipse-dirs (quote ("/home/elseorand/eclipses/eclipse4.5/")))
 '(eclim-executable "/home/elseorand/eclipses/eclipse4.5/eclim")
 '(eclimd-wait-for-process nil)
 '(package-selected-packages
   (quote
    (flyspell-correct flyspell-correct-helm tramp shackle meghanada flycheck-pos-tip company-lsp lsp-mode hiwin async-await helm-tramp dired-launch use-package undo-tree swiper projectile powershell org magit json-mode js2-mode java-snippets japanese-holidays imenus ido-vertical-mode ido-occasional helm-google helm-descbinds helm-anything helm-ag flycheck expand-region exec-path-from-shell emmet-mode electric-operator el-get easy-kill dired+ company-irony clojure-mode clipmon annotate ace-isearch ac-php)))
 '(rtags-use-helm t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(provide '.emacs)
;;; .emacs.el ends here
