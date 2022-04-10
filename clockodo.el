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

;; At the moment, the minor mode provides only basic
;; api interaction from an employer perspective.
;; This means no higher adjustments or access to priviliged
;; spaces is provided.

;; There are two main components. First a set of functions
;; to query the api and secondly an integration of the timer functionality
;; which can be shown in the modebar.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install the packages:
;; + request
;; + s
;; + ts

;; Then put this file into your load-path and put this into your init file:
;; (require 'package-name)

;;;; Usage

;; To only query the api run `clockodo--initialize' and the call one of:
;; + `clockodo-show-informations' - Perform a get request against the clockodo api

;; Check the api part about possible return objects[2].

;; To integrate the clockodo-timer run `clockodo-mode'.
;; This creates a background timer which prints to the modeline.

;; Afterwards, run one of these commands:
;; + `clockodo-start-clock': Start the default clockodo timer.

;;;; Tips

;; + You can customize settings in the `clockodo' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: request.el[1]
;;
;;  [1] https://github.com/tkf/emacs-request/tree/master/tests
;;  [2] https://www.clockodo.com/de/api

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'request)
(require 's)
(require 'ts)

;; Provide customizations
(defgroup clockodo nil
  "Customize the clockodo integration."
  :group 'tools)

(defcustom clockodo-api-url "https://my.clockodo.com/api"
  "The url to the api endpoint of clockodo.

Use %s to mark the end of the url."
  :type 'string
  :group 'clockodo)

(defcustom clockodo-keymap-prefix "C-c C-#"
  "The prefix for the clockodo mode key bindings."
  :type 'string
  :group 'clockodo)

(defcustom clockodo-mode-line-pair '("☞" . "☜")
  "The characters used to identify the clockodo part in modeline."
  :type 'list
  :group 'clockodo)

(defcustom clockodo-store-api-credential t
  "Whether to store the api credentails or not."
  :type 'boolean
  :group 'clockodo)

(defcustom clockodo-minus-color "indian red"
  "The color used for negative hours."
  :type 'color
  :group 'clockodo)

(defcustom clockodo-plus-color "light green"
  "The color for positive hours."
  :type 'color
  :group'clockodo)

(defcustom clockodo-headline-background "dodger blue"
  "The color for the report headline background."
  :type 'color
  :group 'clockodo)

(defcustom clockodo-break-color "light blue"
  "The color used for break times."
  :type 'color
  :group 'clockodo)

(defcustom clockodo-max-work-hours 8
  "The maximum hours of work used for each day."
  :type 'number
  :group 'clockodo)

(defvar clockodo-debug t
  "Shows more informations in the message buffer.")

(defvar clockodo-user-id nil
  "The user id needed for most requests.")

(defvar clockodo-default-service-id nil
  "The default service id defined by the company.")

(defvar clockodo-timer nil
  "The timer object which interacts with the clockodo api `/v2/clock'.")

(defun clockodo--key (key)
  "Convert a key into a prefixed one.

KEY The key which should be prefixed."
  (kbd (concat clockodo-keymap-prefix " " key)))

(defun clockodo--with-face (str &rest face-plist)
  "Enclose a string with a face list.

STR The string which gets a face.
FACE-PLIST The list of faces."
  (propertize str 'face face-plist))

;;;###autoload
(define-minor-mode clockodo-mode
  "Define the mode for interacting with clockodo."
  :init-value nil
  :lighter " clockodo"
  :group 'clockodo
  :global t
  :keymap (list (cons (clockodo--key "s") #'clockodo-start-clock))

  (setq clockodo-timer nil)
  (setq clockodo-display-string (format "%sclock%s" (car clockodo-mode-line-pair) (cdr clockodo-mode-line-pair)))
  (or global-mode-string (setq global-mode-string '("")))
  (if clockodo-mode
      (progn
        (message "Enable clockodo mode")
        (clockodo--initialize)
	      (or (memq 'clockodo-display-string global-mode-string)
	          (setq global-mode-string
		              (append global-mode-string '(clockodo-display-string)))))
    (progn
      (message "Disable clockodo mode")
      (setq clockodo-display-string ""))))
  
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

(defun clockodo--initialize (api-creds)
  "A thin wrapper which set user variables for the next requests.

API-CREDS The credentials for the clockodo api."
  (when (or (null clockodo-user-id) (null clockodo-default-service-id))
    (let* ((response (request-response-data
                      (clockodo--get-user (nth 0 api-creds) (nth 1 api-creds)))))
      (setq clockodo-user-id
            (assoc-default 'id
                           (assoc-default 'user
                                          response))
            clockodo-default-service-id
            (assoc-default 'default_services_id
                           (assoc-default 'company
                                          response))))))

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
    (clockodo--initialize api-credentials)
    api-credentials))

(defun clockodo--get-time-range (time &optional week month)
  "Convert a point in time to a time range.

TIME The point in time to generate the time range.
&WEEK Set to t to get a range for the week surrounding the point in time.
&MONTH Set to t to get a range for the month surrounding the point in time."
  (let ((start (concat (ts-format "%FT" time) "00:00:00Z"))
        (end (concat (ts-format "%FT" time) "23:59:59Z")))
    (cons start end)))

(defun clockodo--create-header (user token)
  "This create the header for requests against the clockodo api.

USER The username used to authenticate the request.
TOKEN The token used to authenticate the request."
  `(("X-ClockodoApiUser" . ,user)
        ("X-ClockodoApiKey" . ,token)
        ("X-Clockodo-External-Application" . ,(concat "clockodo-el;" user))))

(defun clockodo--get-request(user token url-part)
  "This function abstracts a simple get requet to the clockodo api.

The result is a parsed json object.
USER The username used for the api request.
TOKEN The clockodo api token for the user.
URL-PART The full api part for the get request."
  (when clockodo-debug
    (message (concat "API-URL: " clockodo-api-url url-part)))
  (let ((request-header (clockodo--create-header user token)))
    (request (concat clockodo-api-url url-part)
      :sync t
      :headers request-header
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown))))))

(defun clockodo--get-all-services (user token)
  "Request the list of services defined within the company.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--get-request user token "/services"))

(defun clockodo--get-user-services (user token)
  "Request the access rights for user.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--get-request user token
                         (format "/v2/users/%s/access/services" clockodo-user-id)))

(defun clockodo--get-user (user token)
  "Request the map of user settings defined within clockodo.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--get-request user token "/v2/aggregates/users/me"))

(defun clockodo--get-customer-projects (user token)
  "Request the customer projects and the defined access rights for the user.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--get-request user token
                         (format "/v2/users/%s/access/customers-projects" clockodo-user-id)))

(defun clockodo--get-entries (user token &optional user-id start-time end-time less-text)
  "Request time entries from clockodo.

The default is that the current day is requested and the entries are limited
to the current users.

USER The username used for the api request.
TOKEN The clockodo api token for the user.
&USER-ID Limit the result to the specific user.
         If nothing is provided the current user is taken.
         Provide t to get all entries regardless of the user.
&START-TIME The time in ISO 8601 UTC, default start of today.
&END-TIME The time in ISO 8601 UTC, default the current time.
&LESS-TEXT Set to t to get only the basic information."
  (let* ((time-range (clockodo--get-time-range (ts-now)))
         (time-str (format "time_since=%s&time_until=%s"
                           (or start-time (car time-range))
                           (or end-time (cdr time-range))))
         (user-str (format "&users_id=%s" (or user-id clockodo-user-id))))
  (clockodo--get-request user token
                         (concat
                          (format "/v2/entries?%s" time-str)
                          (unless less-text
                            "&enhanced_list=true")
                          (when (or (symbolp user-id) (not user-id))
                            user-str)))))

(defun clockodo--get-users-reports (user token &optional year type all)
  "Request a time report for the user.

USER The username used for the api request.
TOKEN The clockodo api token for the user.
&YEAR The report year (default current-year)
&TYPE One of:
      - 0 Only numbers to the current year (default).
      - 1 Print additional informations about every month.
      - 2 Print additional informations about month and weeks.
      - 3 Print also additional informations about days.
      - 4 Print everything
&ALL if set to true request all reports the user has access to."
  (let* ((request-year (or year (nth 5 (decode-time))))
        (request-type (or type 0))
        (url (if (null all)
                 (format "/userreports/%s?year=%s&type=%s"
                         clockodo-user-id
                         request-year
                         request-type)
                (format "/userreports?year=%s&type=%s"
                        request-year
                        request-type))))
    (clockodo--get-request user token url)))

(defun clockodo--get-non-business-days (user token &optional group-id year)
  "Request a list buisness groups or days which are free.

USER The username from clockodo.
TOKEN The clockodo api token.
&GROUP-ID If the group id is provided a list of free days for a year
          (default: the current year) is returned.
&YEAR Specify the year you want the free days for."
  (let* ((request-year (or year (nth 5 (decode-time)))))
    (if (null group-id)
        (clockodo--get-request user token "/nonbusinessgroups")
      (clockodo--get-request user token
                             (format "/nonbusinessdays?nonbusinessgroups_id=%s&year=%s"
                                     group-id request-year)))))

(defun clockodo--get-lumpsum-services (user token)
  "Request the list of lumpsum services which are defined in clockodo.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--get-request user token "/lumpsumservices"))

(defun clockodo--get-customers (user token &optional customer-id)
  "Request the list of customers or an individual one.

USER The username used for the api request.
TOKEN The clockodo api token for the user.
&CUSTOMER-ID This the informations about a single customer instead of all."
  (if (null customer-id)
      (clockodo--get-request user token "/customers")
    (clockodo--get-request user token (format "/customers/%s" customer-id))))

(defun clockodo--get-abscence (user token &optional year all abscence-id)
  "Request a list of all abscence entries or a detailed one.

USER The username used for the api request.
TOKEN The clockodo api token for the user.
&YEAR The year which the abscence list should be created.
&ALL Whether to list all abscences or just for the current user.
&ABSCENCE-ID Show the informations about a single abscence entry."
  (let* ((request-year (or year (nth 5 (decode-time))))
        (url (if (null all)
                 (format "/absences?year=%s" request-year)
               (format "/absences?year=%s&user_id=%s" request-year clockodo-user-id))))
    (if (null abscence-id)
        (clockodo--get-request user token url)
      (clockodo--get-request user token (format "/abscences/%s" abscence-id)))))

(defun clockodo--table-line (pair &optional key)
  "Convert a pair into a table line or subcall if the pair value is alist.

PAIR The pair which is inserted in the table line.
&KEY The prefix key used for the table line."
  (let ((subkey (car pair))
        (val (cdr pair)))
    (if (listp val)
        (clockodo--table-convert-alist val subkey)
      (format "| %s | %s | %s |\n" key subkey val))))

(defun clockodo--table-convert-alist (alist &optional key)
  "Convert a alist into a table chunk.

ALIST The alist which gets converted.
&KEY The prefix key for the table."
  (mapconcat (lambda (x) (clockodo--table-line x key)) alist ""))

(defun clockodo--table-convert-vec (vec &optional key)
  "Convert a vector into a table chunk.

VEC The vector which gets converted.
&KEY The prefix key for the table."
  (mapconcat (lambda (x) (clockodo--table-convert-alist x key)) vec ""))

(defun clockodo--table-convert-object (tab)
  "Convert a json object into a table.

TAB The json data which gets converted into a plain table."
  (let ((key (car tab))
         (val (cdr tab)))
    (concat (format "*%s*\n" key)
            (if (or (listp val) (vectorp val))
                (if (vectorp val)
                    (clockodo--table-convert-vec val key)
                  (clockodo--table-convert-alist val key))
              (format "| %s | %s |\n" key val))
            "\n")))

(defun clockodo--convert-table (call data)
  "This prints an api requests as a table.

CALL The api call which gets parsed.
DATA The json response converted into the table."
  (concat (format "Request %s\n\n" call)
          (cl-loop for object in data concat
                   (clockodo--table-convert-object object))))

(defun clockodo--show-informations (api-part &optional name raw)
  "A thin wrapper to show json information raw but pretty printed.

API-PART The api request which to show prettyfied.
&NAME Optional the name of the temp buffer.
&RAW Optional flag to print raw response data."
  (with-output-to-temp-buffer (if (null name)
                                  "*clockodo*"
                                name)
    (let* ((credentials (clockodo-get-credentials))
           (response (funcall (intern api-part) (nth 0 credentials) (nth 1 credentials)))
           (data (request-response-data response)))
      (if raw
          (print (pp-to-string data))
        (princ (clockodo--convert-table api-part data))))))

(defun clockodo-show-informations (prefix)
  "This function let a user choose a api-call which json result is shown.

PREFIX The prefix keys used to modify the calls.
       - \\[universal-argument] -> show raw results"
  (interactive "P")
  (let* ((api-calls '(clockodo--get-all-services
                      clockodo--get-user-services
                      clockodo--get-user
                      clockodo--get-users-reports
                      clockodo--get-customer-projects
                      clockodo--get-non-business-days
                      clockodo--get-lumpsum-services
                      clockodo--get-customers
                      clockodo--get-entries
                      clockodo--get-abscence))
         (user-selection (completing-read "Select an api call: " api-calls)))
    (or clockodo-user-id
       (clockodo--initialize))
    (clockodo--show-informations user-selection nil (when prefix t))))

(defun clockodo--convert-second (secs)
  "Convert seconds the a readable hours and minutes format.

SECS The soconds to convert into HH:mm:ss"
  (let* ((hours (/ secs 3600))
         (minutes (/ (% secs 3600) 60))
         (seconds (% secs 60)))
    (format "%s%s%s"
            (if (> hours 0)
                (format "%02d" hours) "")
            (if (> minutes 0)
                (format ":%02d" minutes) "")
            (if (> seconds 0)
                (format ":%02d" seconds) ""))))

(defun clockodo--get-report-header (report-name time username pass)
  "Fetch user informations and generate the report header.

REPORT-NAME The name of the report.
TIME The formated string used for the report time.
USERNAME The username used for the request.
PASS The password used for the request."
  (let* ((header-line (clockodo--with-face
                       (format " Clockodo %s Overview - %s "
                               report-name time)
                       :background clockodo-headline-background
                       :weight 'bold
                       :width 'semi-expanded))
        (response (clockodo--get-users-reports username pass)))
    (let-alist (request-response-data response)
      (cons header-line
            (concat
             (clockodo--with-face
              (concat
               (format "\nName: %s (%s)\n" .userreport.users_name .userreport.users_id)
               (format "E-mail: %s\n" .userreport.users_email)
               (format "Vacation days: %s from %s\n"
                       .userreport.sum_absence.regular_holidays
                       (+ .userreport.holidays_quota .userreport.holidays_carry))
               (format "Sick days: %s\n"
                       (+ .userreport.sum_absence.sick_self .userreport.sum_absence.sick_child))
               (format "Hour account: %s from %s -> "
                       (clockodo--convert-second .userreport.sum_hours)
                       (clockodo--convert-second .userreport.sum_target)))
               :weight 'light)
             (clockodo--with-face
              (clockodo--convert-second (abs .userreport.diff))
              :foreground
              (if (> .userreport.diff 0)
                  clockodo-plus-color
                clockodo-minus-color))
             "\n")))))

(defun clockodo--build-report-buffer (name header body &rest args)
  "Generate a new report buffer and insert the return of body.

NAME The name of the report buffer.
HEADER A two element list with a header-line and a page heading.
BODY A function that fills the buffer.
ARGS The arguments for the body function."
  (let* ((buffer (get-buffer-create (format "*clockodo-%s*" name)))
         (inhibit-read-only t))
    (if (get-buffer-window buffer)
        (pop-to-buffer-same-window buffer)
      (switch-to-buffer-other-window buffer))
    (with-current-buffer buffer
      (erase-buffer)
      (special-mode)
      (setq header-line-format (car header))
      (insert (cdr header))
      (apply body args))))

(defun clockodo--format-daily-entries (entries)
  "Generate the entries for the daily report.

ENTRIES A alist of time entries."
  (setq-local sum-time 0)
  (concat
   (cl-loop for entry across entries concat
            (let-alist entry
              (let ((start (ts-parse .time_insert))
                    (duration (if (null .duration)
                                  (ts-difference (ts-now) (ts-parse .time_insert))
                                .duration)))
                (setq-local
                 sum-time (+ sum-time (string-to-number (format "%s" duration))))
                (concat
                 (clockodo--with-face
                  (ts-format "%T" start)
                  :underline t) "\n"
                 (clockodo--with-face
                  (concat (ts-human-format-duration duration)
                          (format "\n(%s)" .services_name) "\n")
                  :foreground clockodo-plus-color)
                 (clockodo--with-face
                  (if (null .time_until)
                      "Running"
                    (ts-format "%T" (ts-parse .time_until)))
                  :underline t) "\n\n"))))
   (format "Summery: %s / %s hours"
           (ts-human-format-duration sum-time)
           clockodo-max-work-hours)))

(defun clockodo--print-daily-overview (time)
  "Print the daily overview sheet.

TIME The day for which the report should be genereated."
  (let* ((creds (clockodo-get-credentials))
         (time-range (clockodo--get-time-range time))
         (response (clockodo--get-entries
                    (nth 0 creds) (nth 1 creds)
                    clockodo-user-id
                    (car time-range) (cdr time-range)))
         (header (clockodo--get-report-header
                  "Daily" (ts-format "%F" time) (nth 0 creds) (nth 1 creds)))
         (data (request-response-data response))
         (fun #'(lambda (data)
                  (let-alist data
                    (progn
                      (local-set-key
                       (kbd "p")
                       #'(lambda () (interactive)
                          (clockodo--print-daily-overview (ts-dec 'day 1 time))))
                      (local-set-key
                       (kbd "n")
                       #'(lambda () (interactive)
                          (clockodo--print-daily-overview (ts-inc 'day 1 time))))
                      (insert (clockodo--with-face
                               (format "Entries: %s\n" .paging.count_items)
                               :weight 'light))
                      (insert (clockodo--with-face
                               (s-repeat 30 " ")
                               :underline t) "\n\n")
                      (if (> .paging.count_items 0)
                          (insert (clockodo--format-daily-entries .entries))
                        (insert (clockodo--with-face
                                 "No time entries today"
                                 :weight 'semi-bold
                                 :underline t))))))))
    (clockodo--build-report-buffer "daily" header fun data)))
    
(defun clockodo-print-daily-overview (prefix)
  "
PREFIX The prefix keys or arguments to this call.
       - \\[universal-argument] -> Show short results"
  (interactive "P")
  (clockodo--print-daily-overview (ts-now)))

;; test function
;; (let* ((creds (clockodo-get-credentials))
;;       (api-req 'clockodo--get-service-rights))
;;   (clockodo--get-non-business-days (nth 0 creds) (nth 1 creds)))

(provide 'clockodo)
;;; clockodo.el ends here
