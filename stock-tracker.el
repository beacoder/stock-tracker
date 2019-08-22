;;; stock-tracker.el --- Track stock price -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/stock-tracker
;; Version: 0.1
;; Created: 2019-08-18
;; Keywords: convenience, chinese, stock
;; Package-Requires: ((emacs "24.3") (dash "2.16.0"))

;; This file is not part of GNU Emacs.

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
;;
;; Tool for tracking stock price in Emacs
;;
;; Below are commands you can use:
;; `stock-tracker-start'
;; Start stock-tracker and display stock information with buffer

;;; Code:
(require 'dash)
(require 'json)
(require 'org)
(require 'url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup stock-tracker nil
  "Track stock price in Emacs."
  :version "0.1"
  :group 'tools)

(defcustom stock-tracker-buffer-name "*stock-tracker*"
  "Result Buffer name."
  :type 'string
  :group 'stock-tracker)

(defcustom stock-tracker-refresh-interval 1
  "Refresh stock every N * 10 SECS."
  :type 'integer
  :group 'stock-tracker)

(defcustom stock-tracker-list-of-stocks nil
  "List of stock to moniter."
  :type 'list
  :group 'stock-tracker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst stock-tracker--api-url-chn
  "http://api.money.126.net/data/feed/%s"
  "Stock-Tracker API template for stocks listed in SS, SZ, HK of China.")

(defconst stock-tracker--api-url-us
  "http://api.money.netease.com/data/feed/%s"
  "Stock-Tracker API template for stocks listed in US.")

(defconst stock-tracker--result-prefix
  "_ntes_quote_callback("
  "Stock-Tracker result prefix.")

(defconst stock-tracker--result-header
  "|-\n| code | name | price | percent | updown | high | low | volume | open | yestclose |\n"
  "Stock-Tracker result header.")

(defconst stock-tracker--result-footer
  "|-\n"
  "Stock-Tracker result footer.")

(defconst stock-tracker--result-item-format
  "|-\n| %s | %s | %s | %.2f %% | %s | %s | %s | %s | %s | %s |\n"
  "Stock-Tracker result item format.")

(defvar stock-tracker--refresh-timer nil
  "Stock-Tracker refresh timer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; steal from ag/dwim-at-point
(defun stock-tracker--dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;;; improved version, based on ag/read-from-minibuffer
(defun stock-tracker--read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, use it instead of prompt."
  (let* ((suggested (stock-tracker--dwim-at-point))
         (final-prompt
          (if suggested (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt))))
    (if (or current-prefix-arg (string= "" suggested) (not suggested))
        (read-from-minibuffer final-prompt nil nil nil nil suggested)
      suggested)))

(defun stock-tracker--align-all-tables ()
  "Align all org tables."
  (org-table-map-tables 'org-table-align t))

;;; @see https://www.emacswiki.org/emacs/AddCommasToNumbers
(defun stock-tracker--add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
Optional SEPARATOR is the string to use to separate groups.
It defaults to a comma."
  (let ((num (number-to-string number))
        (op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat
                 (match-string 1 num) op
                 (match-string 2 num))))
    num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stock-tracker--format-request-url (stock)
  "Format STOCK as a HTTP request URL."
  (format stock-tracker--api-url-chn (url-hexify-string stock)))

(defun stock-tracker--request (stock)
  "Request STOCK, return JSON as an alist if successes."
  (let (json)
    (with-current-buffer
        (url-retrieve-synchronously
         (stock-tracker--format-request-url stock) t nil 5)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (re-search-forward stock-tracker--result-prefix nil 'move)
      (setq json (json-read-from-string
                  (buffer-substring-no-properties (point) (point-max))))
      (when json (setq json (cdr (-flatten-n 1 json))))
      (kill-buffer (current-buffer)))
    json))

(defun stock-tracker--format-result (stock)
  "Format resulted STOCK information."
  (let* ((json (stock-tracker--request stock))
         (code       (assoc-default 'code       json))
         (name       (assoc-default 'name       json)) ; chinese-word failed to align
         (price      (assoc-default 'price      json))
         (percent    (assoc-default 'percent    json))
         (updown     (assoc-default 'updown     json))
         (open       (assoc-default 'open       json))
         (yestclose  (assoc-default 'yestclose  json))
         (high       (assoc-default 'high       json))
         (low        (assoc-default 'low        json))
         (volume     (assoc-default 'volume     json)))

    ;; construct data for display
    (when code
      (format stock-tracker--result-item-format
              code name price (* 100 percent) updown high low
              (stock-tracker--add-number-grouping volume ",") open yestclose))))

(defun stock-tracker--refresh ()
  "Refresh list of stocks."
  (when stock-tracker-list-of-stocks
    (with-current-buffer (get-buffer-create stock-tracker-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (stock-tracker-mode)
        (insert (format "%s\n\n" (concat "Refresh list of stocks at: " (current-time-string))))
        (insert stock-tracker--result-header)
        (dolist (stock stock-tracker-list-of-stocks)
          (insert (or (stock-tracker--format-result stock) "")))
        (insert stock-tracker--result-footer)
        (stock-tracker--align-all-tables)))))

(defun stock-tracker--run-refresh-timer ()
  "Run stock tracker refresh timer."
  (setq stock-tracker--refresh-timer
        (run-with-timer 0 (* 10 stock-tracker-refresh-interval) 'stock-tracker--refresh)))

(defun stock-tracker--cancel-refresh-timer ()
  "Cancel stock tracker refresh timer."
  (when stock-tracker--refresh-timer
    (cancel-timer stock-tracker--refresh-timer)
    (setq stock-tracker--refresh-timer nil)))

(defun stock-tracker--cancel-timer-on-exit ()
  "Cancel timer when stock tracker buffer is being killed."
  (when (eq major-mode 'stock-tracker-mode)
    (stock-tracker--cancel-refresh-timer)))

(defun stock-tracker--search (stock)
  "Search STOCK and show result in `stock-tracker-buffer-name' buffer."
  (when (and stock (not (string= "" stock)))
    (with-current-buffer (get-buffer-create stock-tracker-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (stock-tracker-mode)
        (insert stock-tracker--result-header)
        (insert (or (stock-tracker--format-result stock) ""))
        (insert stock-tracker--result-footer)
        (stock-tracker--align-all-tables)))))

;;;###autoload
(defun stock-tracker-start (&optional stock)
  "Start stock-tracker, search STOCK and show result in `stock-tracker-buffer-name' buffer."
  (interactive
   (unless stock-tracker-list-of-stocks
     (let ((string (stock-tracker--read-from-minibuffer "stock? ")))
       (list (format "%s" string)))))
  (if stock-tracker-list-of-stocks
      (progn
        (stock-tracker--cancel-refresh-timer)
        (stock-tracker--run-refresh-timer))
    (and stock (stock-tracker--search stock)))
  (unless (get-buffer-window stock-tracker-buffer-name)
    (switch-to-buffer-other-window stock-tracker-buffer-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stock-tracker-add-stock ()
  "Add new stock in table."
  (interactive)
  (let ((orgin-read-only buffer-read-only)
        (stock (format "%s" (read-from-minibuffer "stock? "))))
    (unless (string= "" stock)
      (read-only-mode -1)
      (insert (stock-tracker--format-result stock))
      (stock-tracker--align-all-tables)
      (when orgin-read-only (read-only-mode 1)))))

(defun stock-tracker-remove-stock ()
  "Remove STOCK from table."
  (interactive)
  (let ((orgin-read-only buffer-read-only))
    (read-only-mode -1)
    (org-table-kill-row)
    (when orgin-read-only (read-only-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar stock-tracker-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "i") 'stock-tracker-add-stock)
    (define-key map (kbd "d") 'stock-tracker-remove-stock)
    (define-key map (kbd "g") 'stock-tracker-start)
    map)
  "Keymap for `stock-tracker' major mode.")

;;;###autoload
(define-derived-mode stock-tracker-mode org-mode "stock-tracker"
  "Major mode for viewing Stock-Tracker result.
\\{stock-tracker-mode-map}"
  :group 'stock-tracker
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t
        show-trailing-whitespace nil)
  (setq-local line-move-visual t)
  (setq-local view-read-only nil)
  (remove-hook 'kill-buffer-hook 'stock-tracker--cancel-timer-on-exit)
  (add-hook 'kill-buffer-hook 'stock-tracker--cancel-timer-on-exit)
  (run-mode-hooks))


(provide 'stock-tracker)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; stock-tracker.el ends here
