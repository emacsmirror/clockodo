;;; clockodo.el --- Integrate the clockodo timer into emacs. -*- lexical-binding: t -*-

;; Copyright (c) Henrik Jürges

;; Author: Henrik Jürges <juerges.henrik@gmail.com>
;; Version: 0.9
;; Package-Require: ((emacs "26.1"))
;; Keywords: time, organization
;; URL: https://github.com/santifa/clockodo-el

;;; Commentary:

;; This package provides a minor mode to interact with the clockodo api
;; with simple commands.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install the packages:
;; + apiwrapper

;; Then put this file into your load-path and put this into your init file:
;; (require 'package-name)

;;;; Usage

;; Run (global-clockodo-mode) to keep the mode active across buffers.

;; Run one of these commands:

;; `clockodo-start-clock': Start the default clockodo timer.


;;;; Tips

;; + You can customize settings in the `clockodo' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el


;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'request)

;; Provide customizations
(defgroup clockodo nil
  "Customize the clockodo integration."
  :group 'tools)

(defcustom clockodo-api-url "https://my.clockodo.com/api"
  "The url to the api endpoint of clockodo.

Use %s to mark the end of the url."
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

(defvar clockodo-user-id nil
  "The user id needed for most requests.")

(defvar clockodo-default-service-id nil
  "The service id defined by the company.")

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
  :keymap (list (cons (clockodo--key "s") #'clockodo-start-clock))

  (if clockodo-mode
      (progn
        (message "clockodo mode enabled")
        (clockodo--initialize)
        (clockodo--toggle-mode-line))
    (progn
      (message "clockodo mode disabled")
      (setq global-mode-string '("")))))

;; Handle credentials
(defun clockodo--get-credentials ()
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

(defun clockodo-get-credentials ()
  "A small wrapper which takes long-time storing of credentials into account.

It knocks the clockodo api to test if the credentials are valid before storing them."
  (let ((api-credentials (clockodo--get-credentials)))
    (when clockodo-store-api-credential
      (when (functionp (nth 2 api-credentials))
        (let ((return-code (request-response-status-code
                            (clockodo--get-user
                             (nth 0 api-credentials)
                             (nth 1 api-credentials)))))
          (when (eq return-code 200)
            (funcall (nth 2 api-credentials))))))
    api-credentials))

(defun clockodo--create-header (user token)
  "This create the header for requests against the clockodo api.

USER The username used to authenticate the request.
TOKEN The token used to authenticate the request."
  `(("X-ClockodoApiUser" . ,user)
        ("X-ClockodoApiKey" . ,token)
        ("X-Clockodo-External-Application" . ,(concat "clockodo-el;" user))))

(defun clockodo--get-request(user token url-part)
  "This function abstracts a simple get requet to the clockodo api."
  (let ((request-header (clockodo--create-header user token)))
    (request (concat clockodo-api-url url-part)
      :sync t
      :headers request-header
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown))))))

(defun clockodo--get-all-services (user token)
  "Request the list of services defined within the company.

USER The username from clockodo.
TOKEN The clockodo api token."
  (clockodo--get-request user token "/services"))

(defun clockodo--get-user-services (user token)
  "Request the access rights for user.

USER The username from clockodo.
TOKEN The clockodo api token."
  (clockodo--get-request user token
                         (format "/v2/users/%s/access/services" clockodo-user-id)))

(defun clockodo--get-user (user token)
  "Request the map of user settings defined within clockodo.

USER The username from clockodo.
TOKEN The clockodo api token."
  (clockodo--get-request user token "/v2/aggregates/users/me"))

(defun clockodo--get-customer-projects (user token)
  "Request the list of customer projects and the defined access rights for the user.

USER The username from clockodo.
TOKEN The clockodo api token."
  (clockodo--get-request user token
                         (format "/v2/users/%s/access/customers-projects" clockodo-user-id)))

(defun clockodo--get-users-reports (user token &optional year type all)
  "Request a time report for the user.

USER The username from clockodo.
TOKEN The clockodo api token.
&YEAR The report year (default current-year)
&TYPE One of:
      - 0 Only numbers to the current year (default).
      - 1 Print additional informations about every month.
      - 2 Print additional informations about month and weeks.
      - 3 Print also additional informations about days.
      - 4 Print everything
&ALL if set to true request all reports the user has access to."
  (let* ((request-year (if (null year)
                          (nth 5 (decode-time))
                        year))
        (request-type (if (null type)
                         0
                        type))
        (url (if (null all)
                 (format "/userreports/%s?year=%s&type=%s"
                         clockodo-user-id
                         request-year
                         request-type)
                (format "/userreports?year=%s&type=%s"
                        request-year
                        request-type))))
    (clockodo--get-request user token url)))

(defun clockodo--get-non-business-days (user token &optional group_id year)
  "Request the map of user settings defined within clockodo.

USER The username from clockodo.
TOKEN The clockodo api token."
  (let* ((request-year (if (null year)
                          (nth 5 (decode-time))
                         year)))
    (if (null group_id)
        (clockodo--get-request user token "/nonbusinessgroups")
      (clockodo--get-request user token
                             (format "/nonbusinessdays?nonbusinessgroups_id=%s&year=%s"
                                     group_id request-year)))))

(defun clockodo--get-lumpsum-services (user token)
  "Request the map of user settings defined within clockodo.

USER The username from clockodo.
TOKEN The clockodo api token."
  (clockodo--get-request user token "/lumpsumservices"))

(defun clockodo--get-customers (user token &optional customer-id)
  "Request the map of user settings defined within clockodo.

USER The username from clockodo.
TOKEN The clockodo api token."
  (if (null customer-id)
      (clockodo--get-request user token "/customers")
    (clockodo--get-request user token (format "/customers/%s" customer-id))))

(defun clockodo--get-abscence (user token &optional year all abscence-id)
  "Request the map of user settings defined within clockodo.

USER The username from clockodo.
TOKEN The clockodo api token."
  (let* ((request-year (if (null year)
                          (nth 5 (decode-time))
                        year))
        (url (if (null all)
                 (format "/absences?year=%s" request-year)
               (format "/absences?year=%s&user_id=%s" request-year clockodo-user-id))))
    (if (null abscence-id)
        (clockodo--get-request user token url)
      (clockodo--get-request user token (format "/abscences/%s" abscence-id)))))


(defun clockodo--initialize ()
  "A thin wrapper which set the user id for the next requests."
  (let* ((creds (clockodo-get-credentials))
        (response (request-response-data
                   (clockodo--get-user (nth 0 creds) (nth 1 creds)))))
    (setq clockodo-user-id
          (assoc-default 'id
                         (assoc-default 'user
                                        response))
          clockodo-default-service-id
          (assoc-default 'default_services_id
                         (assoc-default 'company
                                        response)))))

(defun clockodo--toggle-mode-line ()
  ""
  (setq clockodo-display-string "C[X]")
  (or global-mode-string (setq global-mode-string '("")))
  ;; setup the clockodo mode-line
  (when clockodo-mode
          (progn
	          (or (memq 'clockodo-display-string global-mode-string)
	              (setq global-mode-string
		                  (append global-mode-string '(clockodo-display-string)))))))

(defun clockodo--show-informations (api-part &optional name)
  "A thin wrapper to show json information raw but pretty printed.

API-PART The api request which to show prettyfied.
&NAME Optional the name of the temp buffer."
  (with-output-to-temp-buffer (if (null name)
                                  "*clockodo*"
                                name)
    (let* ((credentials (clockodo-get-credentials))
          (response (funcall  (intern api-part) (nth 0 credentials) (nth 1 credentials))))
      (print (pp-to-string (request-response-data response))))
    t))

(defun clockodo-show-informations ()
  "This function let a user choose a api-call which json result is shown."
  (interactive)
  (let* ((api-calls '(clockodo--get-all-services
                      clockodo--get-user-services
                      clockodo--get-user
                      clockodo--get-users-reports
                      clockodo--get-customer-projects
                      clockodo--get-non-business-days
                      clockodo--get-lumpsum-services
                      clockodo--get-customers
                      clockodo--get-abscence))
          (user-selection (completing-read "Select an api call: " api-calls)))
     (clockodo--show-informations user-selection)))

(let* ((creds (clockodo-get-credentials))
      (api-req 'clockodo--get-service-rights))
  (clockodo--get-non-business-days (nth 0 creds) (nth 1 creds)))




;; (let ((creds (clockode-get-and-update-credentials))
;;       (response (clockodo--get-user (nth 0 creds) (nth 1 creds))))
;;   (message (request-response-data response))
;;   (message (request-response-status-code response))
;;   )

(provide 'clockodo)
;;; clockodo.el ends here
