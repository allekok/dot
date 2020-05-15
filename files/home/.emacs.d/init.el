(setq gc-cons-threshold (* 150 1024 1024))

(defun mapcar* (function &rest args)
  (if (not (memq nil args))
      (cons (apply function (mapcar 'car args))
            (apply 'mapcar* function
                   (mapcar 'cdr args)))))

(defun replace-string* (from-string to-string)
  (replace-string from-string to-string nil 0 (buffer-size)))

(defun escape-string (string)
  "Backslash escaping escape-worth characters."
  (let* ((escape-worth '(" " "(" ")" "'" "&"))
         (to (mapcar #'(lambda (o) (format "\\%s" o))
                     escape-worth)))
    (with-temp-buffer
      (insert string)
      (mapcar* 'replace-string* escape-worth to)
      (buffer-string))))

;; `string-trim' function
(require 'subr-x)

;; On bad signature error un-comment the line below.
;; (setq package-check-signature nil)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(defun pkg (pkg-name)
  (unless (package-installed-p pkg-name)
    (package-install pkg-name)))

(pkg 'exwm)
(when (eq 'x (window-system))
  (require 'exwm)
  (exwm-input-set-simulation-keys
   '(
     ([?\C-b] . left)
     ([?\M-b] . C-left)
     ([?\C-f] . right)
     ([?\M-f] . C-right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))
     ([?\C-w] . ?\C-x)
     ([?\M-w] . ?\C-c)
     ([?\C-y] . ?\C-v)
     ([?\C-s] . ?\C-f)))
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86AudioMute
               XF86KbdBrightnessUp
               XF86KbdBrightnessDown
               XF86MonBrightnessUp
               XF86MonBrightnessDown))
    (cl-pushnew k exwm-input-prefix-keys))
  (setq exwm-input-global-keys
        `(([?\s-!] . keyboard-english)
          ([?\s-@] . keyboard-kurdish)
          ([?\s-#] . keyboard-latin-kurdish)))
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer
               (format "#%s#" exwm-class-name))))
  (exwm-enable))

(pkg 'async)
(require 'async)
(dired-async-mode)

(pkg 'markdown-mode)

(pkg 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Hook
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'c-compile-run-current-file)
            (local-set-key (kbd "C-c C-r")
                           (lambda () (interactive)
                             (c-compile-run-current-file t)))))

;; Function
(defun buffer-exists-p (buffer-name)
  (defun buffer-exists-rec (buffer-name buffer-list)
    (if (consp buffer-list)
        (if (string= buffer-name (buffer-name (car buffer-list)))
            buffer-name
          (buffer-exists-rec buffer-name (cdr buffer-list)))))
  (buffer-exists-rec buffer-name (buffer-list)))

(defun c-compile-run-current-file (&optional run)
  (interactive)
  (save-buffer)
  (let* ((in (buffer-file-name))
         (out (substring in 0 -2))
         (compiled? (c-compile-file in out "" "*c-compilation*")))
    (if (and compiled? run) (c-run-file out "" "*c-run*"))))

(defun c-compile-file (in &optional out opts buffer)
  (let* ((out (or out (substring in 0 -2)))
         (command (format "cc %s -o '%s' '%s'" opts out in))
         (result (shell-command-to-string command)))
    (if (string= "" result)
        (progn (message "Compilation finished.") t)
      (if buffer
          (progn
            (unless (buffer-exists-p buffer)
              (generate-new-buffer buffer))
            (with-current-buffer buffer
              (read-only-mode -1)
              (erase-buffer) (insert result)
              (compilation-mode))
            (display-buffer buffer))))))

(defun c-run-file (o &optional opts buffer)
  (let ((command (format "'%s' %s" o opts)))
    (shell-command command buffer)
    (message "") (display-buffer buffer)))

;; Hook
(add-hook 'web-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'php-IA)
            (local-set-key (kbd "C-c C-r") 'php-IA-rtl)))

;; Function  
(defun php-IA (&optional rtl)
  (interactive)
  (let* ((f (buffer-file-name))
         (tr (term "/bin/bash"))
         (rq (format "require('%s');\n" f)))

    (setq bidi-display-reordering rtl)
    (term-send-string tr "php -a\n")
    (term-send-string tr rq)))

(defun php-IA-rtl () (interactive) (php-IA t))

(add-to-list 'load-path
             (expand-file-name "langs" user-emacs-directory))
;; Input-methods
(require 'kurdish-sorani)
(require 'kurdish-kurmanci)

;; Key-bindings
(global-set-key (kbd "s-1")
                (lambda () (interactive)
                  (change-input-method nil "English")))
(global-set-key (kbd "s-2")
                (lambda () (interactive)
                  (change-input-method 'kurdish-sorani "کوردی")))
(global-set-key (kbd "s-3")
                (lambda () (interactive)
                  (change-input-method 'kurdish-kurmanci "Kurdî")))

;; Function
(defun change-input-method (method &optional message)
  (set-input-method method)
  (message message))

(defun memory-free ()
  (format "%.1fG"
          (/ (nth 1 (memory-info)) 1000000.0)))

(defun memory-drop-caches ()
  (interactive)
  (shell-command "sudo su -c 'echo 2 > /proc/sys/vm/drop_caches'")
  (setq memory-free (memory-free))
  (mode-line-refresh)
  (message "Memory cleared. (%s)" memory-free))

(defun local-ip-address ()
  "Private IP Address"
  (string-trim (shell-command-to-string "hostname -i")))

(defun internet? ()
  "Check Internet Connection"
  (let ((connection (car (last (split-string
                                (string-trim
                                 (shell-command-to-string
                                  "nmcli connect|head -2|tail -1")))))))
    (if (not (string= "--" connection))
        (local-ip-address)
      connection)))

;;; Remove bars
(set-frame-parameter nil 'vertical-scroll-bars nil)
(fringe-mode '(0 . 0))

;;; Theme
(global-set-key [XF86LaunchA] 'theme-toggle)

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-safe-themes 'allekok-light)
(add-to-list 'custom-safe-themes 'allekok-dark)

(defun theme-load* (theme)
  "Disable all enabled themes and load `theme'."
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun theme-toggle ()
  (interactive)
  (theme-load* (if (memq 'allekok-light
			 custom-enabled-themes)
		   'allekok-dark 'allekok-light)))

(defun theme-now ()
  (interactive)
  (let ((h (string-to-number
	    (format-time-string "%H")))
	(theme (if (string= (get-light) "light")
		   'allekok-light 'allekok-dark)))
    (theme-load* theme)))

(defun get-light ()
  (interactive)
  (getenv "COLORNOW"))

(theme-now)

;;; Mode-line
(defun mode-line-refresh ()
  (interactive)
  (let ((| " | "))
    (setq-default
     mode-line-format
     (list
      " " battery | datetime |
      ;; Buffer name
      '(:eval (propertize "%b" 'face
			  (when (buffer-modified-p)
			    'font-lock-warning-face)))
      | "%m" | "%l,%02c" | "%p-%I" |
      internet? | (when (volume-mute?) "MUTE ")
      (volume-level) | memory-free))))

(defun mode-line-refresh-variables ()
  (setq datetime (format-time-string "%H:%M %a-%d-%b")
	battery battery-mode-line-string
	internet? (internet?)
	memory-free (memory-free)))

(setq mode-line-refresh-variables-timer
      (run-with-timer 0 20
		      (lambda ()
			(mode-line-refresh-variables)
			(mode-line-refresh))))

(setq display-time-24hr-format t)

(setq battery-mode-line-format "%p")
(display-battery-mode 1)

;; Key-bindings
(global-set-key [XF86AudioMute] 'volume-mute)
(global-set-key [XF86AudioRaiseVolume] 'volume-raise)
(global-set-key [XF86AudioLowerVolume] 'volume-lower)

;; Functions
(defun volume-mute ()
  (interactive)
  (shell-command-to-string
   "amixer set Master toggle")
  (message (if (volume-mute?) "MUTE" "UNMUTE"))
  (mode-line-refresh))

(defun volume-set (v &optional message-format)
  (let ((message-format (or message-format "* volume: %s"))
        (command (concat "amixer set Master "
                         (number-to-string v) "%")))
    (start-process-shell-command command nil command)
    (mode-line-refresh)
    (message message-format (volume-level))))

(cl-defun volume-raise (&optional (step 2))
  (interactive)
  (let ((nv (+ step (string-to-number (volume-level)))))
    (volume-set nv "+ volume: %s")))

(cl-defun volume-lower (&optional (step -2))
  (interactive)
  (let ((nv (+ step (string-to-number (volume-level)))))
    (volume-set nv "- volume: %s")))

(defun volume-level ()
  (let ((vl (string-trim
             (shell-command-to-string
              "awk -F '[][]' '{print $2}' <(amixer get Master | tail -1)"))))
    (unless (string= vl "amixer: Unable to find simple control 'Master',0")
      vl)))

(defun volume-mute? ()
  (when (string= (string-trim
                  (shell-command-to-string
                   "awk -F '[][]' '{print $6}' <(amixer get Master | tail -1)"))
                 "off")
    t))

;; Key-bindings
(global-set-key [XF86MonBrightnessUp] 'screen-brighter)
(global-set-key [XF86MonBrightnessDown] 'screen-darker)

;; Functions
(setq screen-brightness-file
      "/sudo::/sys/class/backlight/acpi_video0/brightness")
(setq screen-brightness-max-file
      "/sudo::/sys/class/backlight/acpi_video0/max_brightness")

(defun screen-brightness-max ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents screen-brightness-max-file)
    (string-to-number (buffer-string))))

(defun screen-brightness-current ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents screen-brightness-file)
    (string-to-number (buffer-string))))

(defun screen-brightness-set (v &optional message-format)
  (interactive "nbrightness: ")
  (let ((message-format (or message-format "* brightness: %d")))
    (when (and (<= v (screen-brightness-max)) (>= v 0))
      (with-temp-file screen-brightness-file
        (insert (number-to-string v)))
      (message message-format v))))

(defun screen-brighter (&optional step)
  (interactive)
  (unless step (setq step +1))
  (let ((v (+ (screen-brightness-current) step)))
    (screen-brightness-set v "+ brightness: +%d")))

(defun screen-darker (&optional step)
  (interactive)
  (unless step (setq step -1))
  (let ((v (+ (screen-brightness-current) step)))
    (screen-brightness-set v "- brightness: -%d")))

;; Key-bindings
(global-set-key [XF86KbdBrightnessUp] 'kbd-brighter)
(global-set-key [XF86KbdBrightnessDown] 'kbd-darker)

;; Functions
(setq kbd-brightness-file
      "/sudo::/sys/class/leds/smc::kbd_backlight/brightness")
(setq kbd-brightness-max-file
      "/sudo::/sys/class/leds/smc::kbd_backlight/max_brightness")

(defun kbd-brightness-max ()
  (with-temp-buffer
    (insert-file-contents kbd-brightness-max-file)
    (string-to-number (buffer-string))))

(defun kbd-brightness-current ()
  (with-temp-buffer
    (insert-file-contents kbd-brightness-file)
    (string-to-number (buffer-string))))

(defun kbd-brightness-set (v &optional message-format)
  (interactive "nkbd backlight: ")
  (let ((message-format (or message-format "* kbd backlight: %d")))
    (when (and (<= v (kbd-brightness-max)) (>= v 0))
      (with-temp-file kbd-brightness-file
        (insert (number-to-string v)))
      (message message-format v))))

(defun kbd-brighter (&optional step)
  (interactive)
  (unless step (setq step +1))
  (let ((v (+ (kbd-brightness-current) step)))
    (kbd-brightness-set v "+ kbd backlight: +%d")))

(defun kbd-darker (&optional step)
  (interactive)
  (unless step (setq step -1))
  (let ((v (+ (kbd-brightness-current) step)))
    (kbd-brightness-set v "- kbd backlight: -%d")))

(setq inhibit-startup-screen t
      initial-scratch-message "")
(defun display-startup-echo-area-message ()
  (message "Hi"))

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(defun keyboard-language (layout &optional variant message)
  (start-process-shell-command
   "keyboard-language" nil
   (format "setxkbmap -layout %s -variant %s"
           layout variant))
  (message message))

(defun keyboard-english () (interactive)
       (keyboard-language "us" "" "English"))

(defun keyboard-kurdish () (interactive)
       (keyboard-language "ir" "ku_ara" "کوردی"))

(defun keyboard-latin-kurdish () (interactive)
       (keyboard-language "ir" "ku" "Kurdî"))

(when (eq 'x (window-system))
  (let ((spec (font-spec :family "NotoNaskhArabicUI")))
    (set-fontset-font nil 'arabic spec)
    (set-fontset-font nil #x200c spec)))

;; Functions

(defun desktop-app-open (app &optional args escape)
  (when (and escape args)
    (setq args (escape-string args)))
  (start-process-shell-command
   app nil (concat app " " args)))

(defmacro desktop-app (app &optional escape prompt)
  (let* ((app-str (symbol-name app))
         (prompt (and prompt (format "%s%s: " prompt app-str))))
    `(defun ,app (&optional args)
       (interactive ,prompt)
       (desktop-app-open ,app-str args ,escape))))

;; Apps

(desktop-app telegram)
(desktop-app firefox)
(desktop-app chromium)
(desktop-app surf t "s")
(desktop-app st)
(desktop-app mupdf t "f")
(desktop-app vlc t "f")
(desktop-app gimp t "f")

(defun tor-browser (&optional args)
  (interactive)
  (shell-command
   "cd ~/projects/tor-browser_en-US/ && ./start-tor-browser.desktop"))

(defun tchromium (&optional args)
  (interactive)
  (chromium (concat "--proxy-server=socks://127.0.0.1:9150 " args)))

(defun desktop-app-query (program)
  (interactive
   (list (read-shell-command "Program: ")))
  (start-process-shell-command
   program nil program))
(global-set-key (kbd "M-!") 'desktop-app-query)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Key-bindings
(global-set-key (kbd "C-x C-k") 'kill-buffer)
;; Kill all buffers
(global-set-key (kbd "C-x C-z") 'kill-buffers-all)
(global-set-key (kbd "C-x z") 'kill-buffers-all)
;; Unset key-binding
(global-unset-key (kbd "C-z"))

;; Functions

(defun kill-buffers-all () (interactive)  
       (mapc 'kill-buffer (buffer-list))
       (cd "~/")
       (message "All buffers killed."))

(global-set-key (kbd "C-x f") 'find-file)

;; Hooks
(setq dired-listing-switches "-alh --group-directories-first")
(global-set-key (kbd "C-x C-d") 'dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook
          #'(lambda ()
              (local-set-key
               (kbd "!") #'(lambda (program)
                             (interactive
                              (list (read-shell-command "Program: ")))
                             (my-dired-shell-command program)))
              (local-set-key
               (kbd "@") 'my-dired-run-http-server)
              (local-set-key
               (kbd "<return>") 'my-dired-uni-open)))

;; Functions

(defun my-dired-uni-open ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (cond
     ((file-directory-p file) (dired-find-file))
     ((string-suffix-p ".avi" file t) (vlc file))
     ((string-suffix-p ".mp4" file t) (vlc file))
     ((string-suffix-p ".mp3" file t) (vlc file))
     ((string-suffix-p ".wav" file t) (vlc file))
     ((string-suffix-p ".m4v" file t) (vlc file))
     ((string-suffix-p ".m4a" file t) (vlc file))
     ((string-suffix-p ".mkv" file t) (vlc file))
     ((string-suffix-p ".webm" file t) (vlc file))
     ((string-suffix-p ".pdf" file t) (mupdf file))
     ((string-suffix-p ".xcf" file t) (gimp file))
     (t (dired-find-file)))))

(defun my-dired-shell-command (program)
  (let ((file (dired-get-file-for-visit)))
    (start-process-shell-command
     "my-dired-shell-command" nil
     (concat program " " (escape-string file)))))

(defun my-dired-run-http-server ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (st (concat "php -S localhost:8081 -t "
                    (escape-string file)
                    " & chromium --app=http://localhost:8081")))))

(setq make-backup-files nil
      auto-save-interval 100)

(setq scroll-step 1
      scroll-conservatively 5)

(setq tramp-default-method "ssh"
      tramp-verbose -1)

(fset 'yes-or-no-p 'y-or-n-p)

;;; allekok-website
;; Open website
(global-set-key (kbd "C-x a")
                (lambda () (interactive)
                  (chromium "--app=https://allekok.ir/")))
;; Test server
(global-set-key (kbd "C-x A")
                (lambda () (interactive)
                  (chromium "--app=http://localhost/")))
;; Open radio
(global-set-key (kbd "C-x j")
                (lambda () (interactive)
                  (chromium "--app=https://allekok.github.io/radio/")))
;; Show allekok/status
(global-set-key (kbd "C-x !")
                #'(lambda () (interactive)
                    (switch-to-buffer "allekok/status")
                    (erase-buffer)
                    (url-insert-file-contents
                     "https://allekok.ir/status.php")
                    (message "'allekok/status' Done!")
                    (org-mode)
                    (setq bidi-paragraph-direction 'right-to-left)))

(global-set-key (kbd "s-<tab>") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(setq show-paren-delay .1)
(show-paren-mode)

(global-set-key (kbd "C-x C-o") 'other-window)

;; Hooks
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'hs-minor-mode-hook
          #'(lambda ()
              (local-set-key (kbd "s-~") 'hs-toggle-all)))

;; Functions
(setq hs-status-all 'show)

(defun hs-toggle-all ()
  (interactive)
  (if (eq 'show hs-status-all)
      (progn (hs-hide-all)
             (setq hs-status-all 'hide))
    (progn (hs-show-all)
           (setq hs-status-all 'show))))

(define-key ctl-x-map [?+] 'text-scale-adjust)
(define-key ctl-x-map [?=] 'text-scale-adjust)
(define-key ctl-x-map [?-] 'text-scale-adjust)

;; Key-bindings
(global-set-key [XF86LaunchB] 'bidi-toggle)

;; Functions

(defun bidi-toggle ()
  (interactive)
  (setq bidi-paragraph-direction
        (if (eq bidi-paragraph-direction
                'right-to-left)
            'left-to-right 'right-to-left)))

;; Key bindings
(global-set-key (kbd "s-`")
                (lambda () (interactive)
                  (git-dir default-directory "status" t)))

;; Functions

(defun git-dir (dir command &optional rtl)
  (interactive)
  (let ((o (term "/bin/bash")))
    (term-send-string o (format "git %s\n" command))
    (setq bidi-display-reordering rtl)))

(electric-indent-mode 1)
(electric-pair-mode 1)

(blink-cursor-mode -1)
(setq-default fill-column 80
              line-spacing 7)
(auto-image-file-mode)
(global-set-key (kbd "C-x e") 'eval-last-sexp)
(global-set-key (kbd "C-<return>") 'calculator)
(when (boundp 'image-map)
  (define-key image-map "=" 'image-increase-size))
(setq safe-local-variable-values
      '((bidi-paragraph-direction . right-to-left))
      shr-use-colors nil)
(setq user-full-name "Payam"
      user-mail-address "payambapiri.97@gmail.com")

(defun kurdish-numbers ()
  (interactive)
  (let ((en '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
        (fa '("۰" "۱" "۲" "۳" "۴" "۵" "۶" "۷" "۸" "۹"))
        (ck '("٠" "١" "٢" "٣" "٤" "٥" "٦" "٧" "٨" "٩")))
    (defun iter (from to)
      (if from
          (progn (replace-string* (car from) (car to))
                 (iter (cdr from) (cdr to)))))
    (iter fa ck)
    (iter en ck)))
(global-set-key [XF86AudioPrev] 'kurdish-numbers)
(setq inferior-lisp-program "/usr/local/bin/scm")
(setq gnus-select-method '(nntp "news.gwene.org"))
(with-eval-after-load 'gnutls
  (setq
   gnutls-verify-error t
   gnutls-min-prime-bits 2048
   gnutls-trustfiles '("/etc/ssl/cert.pem")))
(setq-default tab-width 8
              standard-indent tab-width
              c-basic-offset tab-width
              sgml-basic-offset tab-width
              js-indent-level tab-width
              css-indent-offset tab-width
              nxml-child-indent tab-width
              nxml-outline-child-indent tab-width
	      python-indent-offset tab-width)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(unless (fboundp 'file-attribute-modification-time)
  ;;; From 'files.el'
  (defsubst file-attribute-modification-time (attributes)
    "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a list of integers (HIGH LOW USEC PSEC) in the same style
as (current-time)."
    (nth 5 attributes)))

(defun modif-time (f)
  (let ((m (file-attribute-modification-time
            (file-attributes f))))
    (and m (+ (nth 0 m)
              (/ (nth 1 m) (expt 2.0 16))))))

(defun modif-time-more-recent (f1 f2)
  (let ((m1 (modif-time f1))
        (m2 (modif-time f2)))
    (or (not m2) (> m1 m2))))

(defun compile-if-necessary (f)
  (let* ((org? (string-suffix-p ".org" f t))
         (el? (string-suffix-p ".el" f t))
         (o (concat (substring f 0 (if org? -3 -2)) "elc"))
         (compile? (modif-time-more-recent f o)))
    (if compile?
        (progn (setq byte-compile-warnings nil)
               (if org?
                   (byte-compile-file
                    (car (org-babel-tangle-file
                          f (concat (substring f 0 -3) "el"))))
                 (byte-compile-file f))))))

(defun my-compile-all ()
  (interactive)
  (mapcar 'compile-if-necessary
          (list
           (expand-file-name
            "init.el" user-emacs-directory)
           (expand-file-name
            "themes/allekok-core-theme.el" user-emacs-directory)
           (expand-file-name
            "themes/allekok-dark-theme.el" user-emacs-directory)
           (expand-file-name
            "themes/allekok-light-theme.el" user-emacs-directory)
           (expand-file-name
            "langs/kurdish-sorani.el" user-emacs-directory)
           (expand-file-name
            "langs/kurdish-kurmanci.el" user-emacs-directory))))

(global-set-key [XF86AudioPlay] 'my-compile-all)
(add-hook 'kill-emacs-hook 'my-compile-all)

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(package-selected-packages '(haskell-mode exwm xelb web-mode markdown-mode async))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
