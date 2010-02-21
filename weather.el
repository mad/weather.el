;;; weather.el --- retrieve weather

;; Copyright (C) 2010  mad

;; Author: mad <owner.mad.epa@gmail.com>
;; Keywords: weather
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'xml)
(require 'url)
(require 'url-http)

(defvar weather-city "Санкт Петербург"
  "City for which you want to know the weather")

(defvar weather-last nil
  "Last retrieved weather (used for update titlebar")

(defvar weather-initial-titlebar frame-title-format
  "Save original titlebar")

(defvar weather-timer-interval 600
  "When the weather update")

(defvar weather-timer nil)

(defun weather ()
  (interactive)
  (message (weather-retrieve)))

(defun weather-retrieve ()
  "Retrieve weather for your city `weather-city' the current day"
  (ad-activate 'url-http-user-agent-string)
  (let ((weather-url (concat "http://www.google.com/ig/api?weather="
                             (url-hexify-string weather-city)))
        (url-request-method "GET")
        (url-mime-charset-string nil)
        (url-mime-accept-string "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
        (url-request-extra-headers '(("Accept-Language" . "ru,en;q=0.7,en-US;q=0.3")
                                     ("Accept-Charset" . "ISO-8859-1,utf-8;q=0.7,*;q=0.7"))))
    (switch-to-buffer (url-retrieve-synchronously weather-url))
    (goto-char (point-min))
    (delete-region (point-min) (re-search-forward "\n\n" nil t))
    (let* ((result-buffer (current-buffer))
           (xml-wather (xml-parse-region (point-min) (point-max)))
           (current-conditions
            (xml-node-name
             (xml-get-children
              (xml-node-name
               (xml-get-children (xml-node-name xml-wather) 'weather))
              'current_conditions))))
      (prog1
          (concat
           "["
           (weather-get-attribute-and-decode current-conditions 'temp_c)
           ", "
           (weather-get-attribute-and-decode current-conditions 'condition)
           ", "
           (weather-get-attribute-and-decode current-conditions 'wind_condition)
           "]")
        (kill-buffer result-buffer)
        (ad-deactivate 'url-http-user-agent-string)))))

(defun weather-titlebar ()
  "Update the titlebar with weather"
  (setq weather-last (weather-retrieve)))

(defun weather-titlebar-update (&optional last)
  (if (string-match "Ветер" (mapconcat '(lambda (x)
                                          (if (equal (type-of x) 'string)
                                              (concat x))) frame-title-format ""))
      (setq frame-title-format
            (list "" weather-initial-titlebar (if last weather-last (weather-titlebar))))
    (progn
      (setq weather-initial-titlebar frame-title-format)
      (setq frame-title-format
            (list "" weather-initial-titlebar (if last weather-last (weather-titlebar)))))))

(defun weather-update ()
  (interactive)
  (if (not weather-timer)
      (progn
        (ad-activate 'emms-mode-line-alter-titlebar)
        (setq weather-timer (run-at-time "0 sec" weather-timer-interval #'weather-titlebar-update))
        (message "weather: auto update enable"))
    (progn
      (cancel-timer weather-timer)
      (setq weather-timer nil)
      (setq frame-title-format weather-initial-titlebar)
      (ad-deactivate 'emms-mode-line-alter-titlebar)
      (message "weather: auto update disable"))))

(defun weather-get-attribute-and-decode (xml-data attribute)
  (decode-coding-string
   (xml-get-attribute
     (xml-node-name (xml-get-children xml-data attribute)) 'data) 'utf-8))

(defadvice emms-mode-line-alter-titlebar (around emms-mode-line-alter-titlebar-advice
                                                 () activate)
  "Used for update weather in titlebar"
  ad-do-it
  (weather-titlebar-update t))

;; Google not know what emacs supports utf-8 (even if Accept-Charset:
;; utf-8)
(defadvice url-http-user-agent-string (after url-http-user-agent-string-advice
                                              () activate)
  ""
  (setq ad-return-value "User-Agent: Mozilla/5.0 (X11; U; Linux i686; ru-RU; rv:1.9.1.7) Gecko/20091221 Firefox/3.5.7\n"))

(provide 'weather)
;;; weather.el ends here
