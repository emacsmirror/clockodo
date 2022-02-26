;;; clockodo.el --- Integrate the clockodo timer into emacs. -*- lexical-binding: t -*-

;; Copyright (c) Henrik Jürges

;; Author: Henrik Jürges <juerges.henrik@gmail.com>
;; Version: 0.9
;; Package-Require: ((example "1.0"))
;; Keywords: time, organization
;; URL: https://github.com/santifa/clockodo-el

;;; Commentary:

;; This package provides a minor mode to interact with the clockodo api
;; with simple commands.

;;; Code:

(require 'auth-source)
(require 'apiwrapper)

;; Provide customizations
(defgroup clockodo nil
  "Customize the clockodo integration."
  :group 'tools)

(defcustom clockodo-api-url "https://my.clockodo.com/api/"
  "The url to the api endpoint of clockodo."
  :type 'string
  :group 'clockodo)

(defcustom clockodo-store-api-credential t
  "Whether to store the api credentials or not."
  :type 'boolean
  :group 'clockodo)

(defcustom clockodo-keymap-prefix "C-c C-#"
  "The prefix for the clockodo mode key bindings."
  :type 'string
  :group 'clockodo)

(defun clockodo--key (key)
  "Convert a key into a prefixed one.

KEY The key which should be prefixed."
  (kbd (concat clockodo-keymap-prefix " " key)))

;;;###autoload
(define-minor-mode clockodo-mode
  "Define the mode for interacting with clockodo."
  :init-value nil
  :lighter " clockodo"
  :group 'clockodo
  :global t
  :keymap
  (list (cons (clockodo--key "s") #'clockodo-start-clock))

  (if clockodo-mode
      (message "clockodo mode enabled")
    (message "clockodo mode disabled")))

(defun clockodo-get-credentials ()
  "Return the stored api credentials for clockodo.
The user is asked for credentials if none exists for the api url."
  (let* ((auth-source-creation-prompts
          '((api-user . "Clockodo user: ")
            (api-token . "Clockodo api token: ")))
         (api-creds (nth 0 (auth-source-search :max 1
                                               :host clockodo-api-url
                                               :require '(:user :secret)
                                               :create t))))
    (if api-creds
        (list (plist-get api-creds :user)
              (let ((api-pass (plist-get api-creds :secret)))
                (if (functionp api-pass)
                    (funcall api-pass)
                  api-pass))
              (plist-get api-creds :save-function))
      nil)))

(defun clockode-get-and-update-credentials ()
  "A small wrapper which takes long-time storing of credentials into account.
UPDATE Set to t if the saved credentials should be updated."
  (let ((api-credentials (clockodo-get-credentials)))
    (when clockodo-store-api-credential
        (when (functionp (nth 2 api-credentials))
          (progn
            (funcall (nth 2 api-credentials)))))
    api-credentials))



(setq auth-source-debug "trivia")
(clockodo-get-credentials)
(clockode-get-and-update-credentials)



(provide 'clockodo)
;;; clockodo.el ends here
