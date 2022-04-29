;;; clockodo.el --- A small integration for the clockodo api -*- lexical-binding: t -*-

;; Copyright (c) 2022 Henrik Jürges <ratzeputz@rtzptz.xyz>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Henrik Jürges <juerges.henrik@gmail.com>
;; URL: https://github.com/santifa/clockodo-el
;; Version: 0.7
;; Package-Requires: ((emacs "26.1") (request "0.3.2") (ts "0.2.2") (org "8"))
;; Keywords: tools, clockodo

;;; Commentary:

;; This package provides a minor mode to interact with the clockodo api
;; with simple commands and a simple clock.

;; At the moment, the minor mode provides only basic
;; api interaction from an employee perspective.
;; This means no higher adjustments or access to priviliged
;; spaces is provided.

;; There are two main components. First a set of functions
;; to query the api and secondly an integration of the timer functionality
;; which can be shown in the mode-line.

;;;; Installation

;;;;; MELPA

;; If you installed the package  from MELPA add (require 'clockodo) and you're done.

;;;;; Manual

;; Install the packages:
;; + request
;; + ts
;; + org

;; Then put this file into your load-path and put this into your init file:
;; (require 'clockodo)

;;;; Usage

;; This package provides to components for the user. The first one generates
;; reports from the user perspective.

;; + `clockodo-print-daily-overview' - Generates a daily report
;; + `clockodo-print-weekly-overview' - Generates a table for the weekly report
;; + `clockodo-print-monthly-overview' - Generates a monthly overview report
;; + `clockodo-print-overall-overview' - Generates a report from the beginning of recording

;; Within the report buffers use \\[n] and \\[p] to view the previous or next buffer.

;; To integrate the clockodo-timer run `clockodo-mode'.
;; This creates a background timer which prints to the modeline.

;; Afterwards, run one of these commands:
;; + `clockodo-toggle-clock': Start the default clockodo timer.

;; The mode-line updater and the clock can be used independently which
;; means one can enable the clock from another application and just show
;; the current duration within Emacs.

;;;; Tips

;; + You can customize settings in the `clockodo' group.
;; + Use `clockodo-show-information' to see the raw api results.
;; + Use (setq clockodo-debug t) to get the api calls printed to messages.

;;;; Credits

;; This package would not have been possible without the following
;; packages: request.el[1] and ts.el[3]
;;
;;  [1] https://github.com/tkf/emacs-request/tree/master/tests
;;  [2] https://www.clockodo.com/de/api
;;  [3] https://github.com/alphapapa/ts.el

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'request)
(require 'ts)
(require 'org)

;; Provide customizations
(defgroup clockodo nil
  "Customize the clockodo integration."
  :group 'tools)

(defcustom clockodo-api-url "https://my.clockodo.com/api"
  "The url to the api endpoint of clockodo."
  :type 'string
  :group 'clockodo)

(defcustom clockodo-keymap-prefix "C-c C-#"
  "The prefix for the clockodo mode key bindings."
  :type 'string
  :group 'clockodo)

(defcustom clockodo-mode-line-pair '("「" . "」")
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

(defcustom clockodo-work-days-a-week 5
  "The number of days which count as working days in a week.
Only 7 days and 5 days are supported."
  :type 'number
  :group 'clockodo)

(defcustom clockodo-update-interval 60
  "The update interval in seconds used for query the clockodo clock if it is running."
  :type 'number
  :group 'clockodo)

;; Mode and API variables

(defvar clockodo-debug t
  "Shows more information in the message buffer.")

(defvar clockodo-user-id nil
  "The user id needed for most requests.")

(defvar clockodo-default-service-id nil
  "The default service id defined by the company.")

(defvar clockodo-default-customer-id nil
  "The default customer id defined by the company.")

(defvar clockodo-timer-id nil
  "The timer object id which is used for interaction with the clockodo api `/v2/clock'.")

(defvar clockodo-timer nil
  "The internal timer object used to update the display-mode.")

;; General methods used throughout the package

(defun lambda-key (keymap key def)
  "Wrap`define-key' to provide documentation.

KEYMAP The keymap the key gets bound to
KEY The key bound for the function
DEF The anonymous function for the key"
  (set 'sym (make-symbol (documentation def)))
  (fset sym def)
  (define-key keymap key sym))

(defun clockodo--key (key)
  "Convert a key into a prefixed one.

KEY The key which should be prefixed."
  (kbd (concat clockodo-keymap-prefix " " key)))

(defun clockodo--work-seconds ()
  "This function return the `clockodo-max-work-hours' in seconds."
  (* 60 (* 60 clockodo-max-work-hours)))

(defun clockodo--with-face (str &rest face-plist)
  "Enclose a string with a face list.

STR The string which gets a face.
FACE-PLIST The list of faces."
  (propertize str 'face face-plist))

(defun clockodo--color-time (time ref-time &optional abbrev)
  "This function wraps a time into a color if its smaller than a reference time.

TIME The time to show with a colored face.
REF-TIME The reference time which is compared.
&ABBREV Show short or long time format, non-nil for short."
  (clockodo--with-face
   (ts-human-format-duration (abs time) abbrev)
   :foreground
   (if (> time ref-time)
       clockodo-plus-color
     clockodo-minus-color)))

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
  (when (or (null clockodo-user-id)
           (null clockodo-default-service-id)
           (null clockodo-default-customer-id))
    (let* ((response (request-response-data
                      (clockodo--get-user (nth 0 api-creds) (nth 1 api-creds)))))
      (let-alist response
        (setq clockodo-user-id .user.id
              clockodo-default-service-id .company.default_services_id
              clockodo-default-customer-id .company.default_customers_id)))))

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
  (let* ((beg (if week
                     (+ 1 (- (ts-dow time)))
                (if month
                    (+ 1 (- (ts-dom time)))
                  0)))
         (end (if week
                  (+ 1 (- 7 (ts-dow time)))
                (if month
                    (- (ts-dom
                        (ts-adjust 'day (+ 1 (- (ts-dom time)))
                                   'month +1 'day -1 time))
                       (ts-dom time))
                  (+ 1 (- (ts-dom time))))
                0)))
    (cons (ts-format "%FT%TZ" (ts-apply :hour 0 :minute 0 :second 0 (ts-adjust 'day beg time)))
          (ts-format "%FT%TZ" (ts-apply :hour 23 :minute 59 :second 59 (ts-adjust 'day end time))))))

(defun clockodo--create-header (user token)
  "This create the header for requests against the clockodo api.

USER The username used to authenticate the request.
TOKEN The token used to authenticate the request."
  `(("X-ClockodoApiUser" . ,user)
        ("X-ClockodoApiKey" . ,token)
        ("X-Clockodo-External-Application" . ,(concat "clockodo-el;" user))))


(defun clockodo--delete-request (user token url-part)
  "This function abstracts a simple delete request to the clockodo api.

The result is a parsed json object.
USER The username used for the api request.
TOKEN The clockodo api token for the user.
URL-PART The full api part for the get request."
  (when clockodo-debug
    (message (concat "DELETE API-URL: " clockodo-api-url url-part)))
  (let ((delete-header (clockodo--create-header user token)))
    (request (concat clockodo-api-url url-part)
      :sync t
      :type "DELETE"
      :headers delete-header
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown))))))

(defun clockodo--stop-clock (user token)
  "This function stops the clockodo clock currently running.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--delete-request user token
                            (format "/v2/clock/%s" clockodo-timer-id)))

(defun clockodo--post-request (user token url-part data)
  "This function abstracts a simple post request to the clockodo api.

The result is a parsed json object.
USER The username used for the api request.
TOKEN The clockodo api token for the user.
URL-PART The full api part for the get request.
DATA The data for the post request."
  (when clockodo-debug
    (message (concat "POST API-URL: " clockodo-api-url url-part)))
  (let ((post-header (clockodo--create-header user token)))
    (request (concat clockodo-api-url url-part)
      :type "POST"
      :sync t
      :headers post-header
      :data data
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown))))))

(defun clockodo--start-clock (user token)
  "This function start the clockodo clock with default values and service.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--post-request user token "/v2/clock"
                          `(("customers_id" .  ,clockodo-default-customer-id)
                            ("services_id" . ,clockodo-default-service-id))))

(defun clockodo--get-request(user token url-part)
  "This function abstracts a simple get request to the clockodo api.

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

(defun clockodo--get-clock (user token)
  "Request the current state of the clockodo clock service.

USER The username used for the api request
TOKEN The clockodo api token for the user"
  (clockodo--get-request user token "/v2/clock"))

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
      - 1 Print additional information about every month.
      - 2 Print additional information about month and weeks.
      - 3 Print also additional information about days.
      - 4 Print everything
&ALL if set to true request all reports the user has access to."
  (let* ((request-year (or year (ts-year (ts-now))))
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
  (let* ((request-year (or year (ts-year (ts-now)))))
    (clockodo--get-request user token
                           (concat "/nonbusinessgroups"
                                   (unless (null group-id)
                                     (format "?nonbusinessgroups_id=%s&year=%s"
                                             group-id
                                             request-year))))))

(defun clockodo--get-lumpsum-services (user token)
  "Request the list of lumpsum services which are defined in clockodo.

USER The username used for the api request.
TOKEN The clockodo api token for the user."
  (clockodo--get-request user token "/lumpsumservices"))

(defun clockodo--get-customers (user token &optional customer-id)
  "Request the list of customers or an individual one.

USER The username used for the api request.
TOKEN The clockodo api token for the user.
&CUSTOMER-ID This the information about a single customer instead of all."
  (clockodo--get-request user token
                         (concat "/customers"
                                 (unless (null costomer-id)
                                   (format "/%s" customer-id)))))

(defun clockodo--get-abscence (user token &optional year all abscence-id)
  "Request a list of all abscence entries or a detailed one.

USER The username used for the api request.
TOKEN The clockodo api token for the user.
&YEAR The year which the abscence list should be created.
&ALL Whether to list all abscences or just for the current user.
&ABSCENCE-ID Show the information about a single abscence entry."
  (let* ((request-year (or year (ts-year (ts-now))))
         (url (concat (format "/absences?year=%s" request-year)
                      (unless (null all)
                        (format "&user_id=%s" clockodo-user-id)))))
    (clockodo--get-request user token
                           (if (null abscence-id)
                               url
                             (format "/abscences/%s" abscence-id)))))

;; Report functions

(defun clockodo--show-information (api-part &optional name)
  "A thin wrapper to show json information raw but pretty printed.

API-PART The api request which to show prettyfied.
&NAME Optional the name of the temp buffer."
  (with-output-to-temp-buffer (or  name "*clockodo*")
    (let* ((credentials (clockodo-get-credentials))
           (response (funcall (intern api-part) (nth 0 credentials) (nth 1 credentials)))
           (data (request-response-data response)))
      (princ (format "%s:\n%s"
                     api-part
                     (pp-to-string data))))))

(defun clockodo-show-information ()
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
                      clockodo--get-entries
                      clockodo--get-abscence
                      clockodo--get-clock))
         (user-selection (completing-read "Select an api call: " api-calls)))
    (clockodo--show-information user-selection nil)))

(defun clockodo--convert-second (secs)
  "Convert seconds the a readable hours and minutes format.

SECS The soconds to convert into HH:mm:ss"
  (let* ((hours (/ secs 3600))
         (minutes (/ (% secs 3600) 60))
         (seconds (% secs 60)))
    (format "%s:%s:%s"
            (if (> hours 0)
                (format "%02d" hours) "00")
            (if (> minutes 0)
                (format "%02d" minutes) "00")
            (if (> seconds 0)
                (format "%02d" seconds) "00"))))

(defun clockodo--get-report-header (report-name time username pass)
  "Fetch user information and generate the report header.

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
             (clockodo--color-time .userreport.diff 0)
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

(defun clockodo--daily-entries-as-list (entries &optional abbrev)
  "Generate a list of preformated daily entries.

The format is:
List (start_time1 duration1 end_time1) (next_entries) sum_time

ENTRIES A alist of time entries.
ABBREV Show times in short form. If non-nil use short forms."
  (let ((sum-time (seq-reduce
                   #'(lambda (init entry)
                       (let-alist entry
                         (+ init (string-to-number
                                  (format "%s"
                                          (or .duration
                                             (ts-difference
                                              (ts-now)
                                              (ts-parse .time_insert))))))))
                   entries 0)))
    (list
     :entries
     (seq-map #'(lambda (e)
                  (let-alist e
                    (let ((start (ts-format "%T" (ts-parse .time_insert)))
                          (duration (or .duration (ts-difference (ts-now) (ts-parse .time_insert))))
                          (end (if (null .time_until)
                                   "Running"
                                 (ts-format "%T" (ts-parse .time_until))))
                          (service (format "(%s)" .services_name)))
                      (list
                       (clockodo--with-face
                        start
                        :underline t)
                       (clockodo--color-time duration 0 abbrev)
                       (clockodo--with-face
                        service
                        :foreground clockodo-plus-color)
                       (clockodo--with-face
                        end
                        :underline t)))))
              entries)
     :sum sum-time)))

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
                      (local-set-key (kbd "g") #'clockodo-print-daily-overview)
                      (lambda-key (current-local-map) (kbd "c")
                                  #'(lambda ()
                                    "choose report day"
                                    (interactive)
                                    (clockodo-print-daily-overview t)))
                      (lambda-key (current-local-map) (kbd "p")
                                  #'(lambda ()
                                      "previous day"
                                      (interactive)
                                      (clockodo--print-daily-overview (ts-dec 'day 1 time))))
                      (lambda-key (current-local-map) (kbd "n")
                                  #'(lambda ()
                                      "next day"
                                      (interactive)
                                         (clockodo--print-daily-overview (ts-inc 'day 1 time))))
                    (insert (clockodo--with-face
                               (format "Entries: %s\n" .paging.count_items)
                               :weight 'light))
                    (insert (clockodo--with-face
                               (make-string 30 32)
                               :underline t) "\n\n")
                      (let ((entries (clockodo--daily-entries-as-list .entries)))
                        (if (not (null (plist-get entries :entries)))
                            (progn
                              (seq-do #'(lambda (e)
                                          (insert (string-join e "\n") "\n\n"))
                                      (plist-get entries :entries))
                              (insert (format "Summery: %s / %s hours"
                                              (clockodo--color-time (plist-get entries :sum)
                                                                    (clockodo--work-seconds))
                                              clockodo-max-work-hours)))
                          (insert (clockodo--with-face
                                   "No time entries today."
                                   :weight 'semi-bold
                                   :underline t)))))))))
    (clockodo--build-report-buffer "daily" header fun data)))

(defun clockodo-print-daily-overview (prefix)
  "Create a report for the day.

PREFIX Use the \\[universal-argument] to select the day."
  (interactive "P")
  (let* ((date (unless (null prefix)
                 (ts-parse-org (org-read-date))))
         (real-date (or date (ts-now))))
    (clockodo--print-daily-overview real-date)))

(defun clockodo--format-weekly-entries (entries start-day)
  "Generate the entries for the weekly report.

ENTRIES The alist data to format.
START-DAY The week which gets reported."
  (let* ((p (point))
         (days (mapcar
                #'(lambda (days)
                    (let ((time (ts-adjust 'day days start-day)))
                      (plist-put
                       (clockodo--daily-entries-as-list
                        (seq-filter
                         #'(lambda (e)
                             (let-alist e
                               (= (ts-dow time)
                                  (ts-dow (ts-parse .time_insert)))))
                         entries)
                        t)
                       :day (clockodo--with-face
                             (ts-format "%a %d.%m.%y" time)
                             :weight 'semi-bold
                             :background "thistle"
                             :foreground "black"))))
                (number-sequence 0 (1- clockodo-work-days-a-week))))
         (leng (seq-max (mapcar #'(lambda (e) (length (plist-get e :entries))) days)))
         (header (seq-map #'(lambda (e) (plist-get e :day)) days))
         (entries (seq-map #'(lambda (e) (plist-get e :entries)) days))
         (sums (seq-map
                #'(lambda (e)
                    (format "%s / %s hours"
                            (clockodo--color-time (plist-get e :sum)
                                                  (clockodo--work-seconds) t)
                            clockodo-max-work-hours))
                days)))
    (insert (mapconcat #'identity header ",") "\n")
    (cl-loop for i from 0 to (1- leng) do
             (cl-loop for j from 0 to 4 do
                      (insert
                       (string-remove-suffix ","
                        (string-join
                         (seq-map #'(lambda (e) (concat (nth j (nth i e)) ",")) entries) ""))
                       "\n")))
    (insert (mapconcat #'identity sums ","))
    (org-table-convert-region p (point))
    (goto-char p)
    (org-table-insert-hline)
    (goto-char (point-max))
    (insert "\n"
            (format "Summary: %s"
                    (ts-human-format-duration
                     (apply #'+ (seq-map (lambda (e) (plist-get e :sum)) days)))))))

(defun clockodo--print-weekly-overview (time)
  "Print the weekly overview sheet.

TIME The day for which the report should be genereated."
  (let* ((creds (clockodo-get-credentials))
         (time-range (clockodo--get-time-range time t))
         (response (clockodo--get-entries
                    (nth 0 creds) (nth 1 creds)
                    clockodo-user-id
                    (car time-range) (cdr time-range)))
         (header (clockodo--get-report-header
                  "Weekly" (ts-format "%+4Y-%m Week %W" time) (nth 0 creds) (nth 1 creds)))
         (data (request-response-data response))
         (fun #'(lambda (data)
                  (let-alist data
                    (progn
                      (local-set-key (kbd "g") #'clockodo-print-weekly-overview)
                      (local-set-key (kbd "c") '(clockodo-print-weekly-overview t))
                      (lambda-key (current-local-map) (kbd "c")
                                  #'(lambda ()
                                      "choose report week"
                                      (interactive)
                                      (clockodo-print-weekly-overview t)))
                      (lambda-key (current-local-map) (kbd "p")
                                  #'(lambda ()
                                      "previous week"
                                      (interactive)
                                      (clockodo--print-weekly-overview (ts-dec 'day -7 time))))
                      (lambda-key (current-local-map) (kbd "n")
                                  #'(lambda ()
                                      "next week"
                                      (interactive)
                                      (clockodo--print-weekly-overview (ts-inc 'day +7 time))))
                      (insert (clockodo--with-face
                               (format "Entries: %s\n" .paging.count_items)
                               :weight 'light))
                      (insert (clockodo--with-face
                               (make-string 30 32)
                               :underline t)
                              "\n\n")
                      (clockodo--format-weekly-entries .entries (ts-parse (car time-range))))))))
    (clockodo--build-report-buffer "weekly" header fun data)))

(defun clockodo-print-weekly-overview (prefix)
  "Create a report for the week.

PREFIX Use the \\[universal-argument] to select the week."
  (interactive "P")
  (let* ((date (unless (null prefix)
                 (ts-parse-org (org-read-date))))
         (real-date (or date (ts-now))))
    (clockodo--print-weekly-overview real-date)))

(defun clockodo--print-monthly-overview (time)
  "Print the weekly overview sheet.

TIME The day for which the report should be genereated."
  (let* ((creds (clockodo-get-credentials))
         (time-range (clockodo--get-time-range time nil t))
         (response (clockodo--get-entries
                    (nth 0 creds) (nth 1 creds)
                    clockodo-user-id
                    (car time-range) (cdr time-range)))
         (header (clockodo--get-report-header
                  "Mothly" (ts-format "%+4Y-%m" time) (nth 0 creds) (nth 1 creds)))
         (data (request-response-data response))
         (fun #'(lambda (data)
                  (let-alist data
                    (progn
                      (local-set-key (kbd "g") #'clockodo-print-monthly-overview)
                      (lambda-key (current-local-map) (kbd "c")
                                  #'(lambda ()
                                    "choose report month"
                                    (interactive)
                                    (clockodo-print-monthly-overview t)))
                      (lambda-key (current-local-map) (kbd "p")
                                  #'(lambda ()
                                      "previous month"
                                      (interactive)
                                      (clockodo--print-monthly-overview (ts-dec 'month 1 time))))
                      (lambda-key (current-local-map) (kbd "n")
                                  #'(lambda ()
                                      "next month"
                                      (interactive)
                                      (clockodo--print-monthly-overview (ts-inc 'month 1 time))))
                      (insert "monthly" )
                      (insert (format "\n%s" (clockodo--get-users-reports (nth 0 creds) (nth 1 creds) "2022" 1)) ))))))
    (clockodo--build-report-buffer "monthly" header fun data)))

(defun clockodo-print-monthly-overview (prefix)
  "Create a report for the month.

PREFIX Use the \\[universal-argument] to select the month."
  (interactive "P")
  (let* ((date (unless (null prefix)
                 (ts-parse-org (org-read-date))))
         (real-date (or date (ts-now))))
    (clockodo--print-monthly-overview real-date)))


(defun clockodo--print-overall-overview (time)
  "Print the weekly overview sheet.

TIME The day for which the report should be genereated."
  (let* ((creds (clockodo-get-credentials))
         (time-range (clockodo--get-time-range time))
         (response (clockodo--get-entries
                    (nth 0 creds) (nth 1 creds)
                    clockodo-user-id
                    (car time-range) (cdr time-range)))
         (header (clockodo--get-report-header
                  "Overall" "" (nth 0 creds) (nth 1 creds)))
         (data (request-response-data response))
         (fun #'(lambda (data)
                  (let-alist data
                    (progn
                      (insert "overall" ))))))
    (clockodo--build-report-buffer "overall" header fun data)))

(defun clockodo-print-overall-overview ()
  "Create a long overview report over the complete time."
  (interactive))

;;; The clock as minor mode

(defun clockodo-get-clock ()
  "This requests the current clock state from clockodo.

The function returns nil if the clock is not running or
the time duration since the clock was started in seconds."
  (let* ((creds (clockodo--get-credentials))
         (response (clockodo--get-clock (nth 0 creds) (nth 1 creds)))
         (data (request-response-data response)))
    (let-alist data
      (unless (null .running.id)
        (ts-human-format-duration
         (ts-difference (ts-now) (ts-parse .running.time_insert)) t)))))

(defun clockodo-start-clock ()
  "Start the clockodo clock service.

If the clock is already started the function sets the current id
for being able to stop the clock later."
  (let* ((creds (clockodo--get-credentials))
         (response (clockodo--get-clock (nth 0 creds) (nth 1 creds)))
         (data (request-response-data response)))
    (let-alist data
      (if (null .running.id)
          (let* ((post-resp (clockodo--start-clock (nth 0 creds) (nth 1 creds)))
                 (post-data (request-response-data post-resp)))
            (let-alist post-data
              (setq clockodo-timer-id .running.id)
              (message (format "Started clock at %s" (ts-format "%T" (ts-parse  .running.time_since))))))
        (progn
          (setq clockodo-timer-id .running.id)
          (message (format "Clock already started at %s" (ts-format "%T" (ts-parse  .running.time_since)))))))))

(defun clockodo-stop-clock ()
  "Stop the clockodo clock service.

If the clock is already stopped and a reference is stored it is only set
to nil instead of really stopping the clock."
  (let* ((creds (clockodo--get-credentials))
         (response (clockodo--get-clock (nth 0 creds) (nth 1 creds)))
         (data (request-response-data response)))
    (let-alist data
      (if (and clockodo-timer-id (not (null .running.id)))
          (let* ((del-resp (clockodo--stop-clock (nth 0 creds) (nth 1 creds)))
                 (del-data (request-response-data del-resp)))
            (let-alist del-data
             (setq clockodo-timer-id nil)
             (message (format "Stopped clock at %s" (ts-format "%T" (ts-parse .stopped.time_until))))))
        (progn
          (setq clockodo-timer-id nil)
          (message "Clock already stopped"))))))

(defun clockodo-toggle-clock ()
  "Toggle the state of the clockodo clock service."
  (interactive)
  (if clockodo-timer-id
      (clockodo-stop-clock)
    (clockodo-start-clock)))

;;;###autoload
(define-minor-mode clockodo-mode
  "Define the mode for interacting with clockodo."
  :init-value nil
  :lighter " clockodo"
  :group 'clockodo
  :global t
  :keymap (list (cons (clockodo--key "t") #'clockodo-toggle-clock)
                (cons (clockodo--key "d") #'clockodo-print-daily-overview)
                (cons (clockodo--key "w") #'clockodo-print-weekly-overview))
  ;; Set mode-line mouse bindings
  (global-set-key [mode-line mouse-2] 'clockodo-toggle-clock)
  
  ;; Set the empty mode-line
  (let ((clock (format "%sclock%s" (car clockodo-mode-line-pair) (cdr clockodo-mode-line-pair))))
    (setq clockodo-display-string clock)
    (or global-mode-string (setq global-mode-string '("")))
   (if clockodo-mode
       (progn
         (message "Enable clockodo mode")
         (or (memq 'clockodo-display-string global-mode-string)
	          (setq global-mode-string
		              (append global-mode-string '(clockodo-display-string))))
         (setq clockodo-timer
               (run-with-timer 0 clockodo-update-interval
                            #'(lambda ()
                                (let ((time (clockodo-get-clock)))
                                  (if (not (null time))
                                      (setq clockodo-display-string
                                            (format "%s%s%s"
                                                    (car clockodo-mode-line-pair)
                                                    time
                                                    (cdr clockodo-mode-line-pair)))
                                    (setq clockodo-display-string clock)))))))
     (progn
       (message "Disable clockodo mode")
       (unless (null clockodo-timer-id)
         (clockodo--stop-clock))
       (unless (null clockodo-timer)
         (cancel-timer clockodo-timer))
       (setq clockodo-display-string ""
             clockodo-timer nil)))))

(provide 'clockodo)
;;; clockodo.el ends here
