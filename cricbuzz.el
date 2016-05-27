;;; cricbuzz.el --- Cricket scores from cricbuzz in emacs

;; Copyright (c) 2016 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 0.1
;; Package-Requires ((enlive "0.0.1"))
;; Keywords: cricket, score
;; URL: https://github.com/lepisma/cricbuzz.el

;;; Commentary:

;; cricbuzz.el displays live cricket scores and scorecards from cricbuzz.com

;; TODO full doc

;;; Code:

(require 'enlive)

(defvar cricbuzz-base-url "http://cricbuzz.com")
(defvar cricbuzz-live-url (concat cricbuzz-base-url "/cricket-match/live-scores"))
(defvar cricbuzz-schedule-file (expand-file-name "~/cricket-schedule.org"))

;; Parse live scores

(defun cricbuzz-get-url (match-node)
  "Return complete match url"
  (replace-regexp-in-string
   (regexp-quote "scores")
   "scorecard"
   (concat cricbuzz-base-url (enlive-attr (enlive-query match-node [a]) 'href))
   nil 'literal))

(defun cricbuzz-get-time (match-node)
  "Return org time string"
  (format-time-string
   "<%Y-%m-%d %a %H:%M>"
   (seconds-to-time
    (/
     (string-to-int
      (enlive-attr (first (enlive-get-elements-by-class-name match-node "schedule-date")) 'timestamp))
     1000))))

(defun cricbuzz-get-title (match-node)
  "Return match title"
  (enlive-attr (enlive-query match-node [a]) 'title))

(defun cricbuzz-get-properties (match-node)
  "Return description and venue in a list"
  (let ((gray-nodes (enlive-query-all match-node [div.text-gray])))
    (list
     (enlive-text (first gray-nodes))
     (enlive-text (second gray-nodes)))))

(defun cricbuzz-parse-scores (details-node)
  "Return scores of both teams"
  (let ((score-node (enlive-direct-children
                     (first (enlive-query-all details-node [div.text-black])))))
    (list
     (concat
      (enlive-text (first score-node))
      " :: "
      (enlive-text (second score-node)))
     (concat
      (enlive-text (fifth score-node))
      " :: "
      (enlive-text (sixth score-node))))))

(defun cricbuzz-get-status (match-node)
  "Return status, status-text, score-one and score-two"
  (let* ((details-node (second (enlive-query-all match-node [a])))
         (complete-node (first (enlive-get-elements-by-class-name
                                details-node
                                "cb-text-complete")))
         (live-node (first (enlive-get-elements-by-class-name
                            details-node
                            "cb-text-live"))))
    (if live-node
        (cons "LIVE" (cons
                      (enlive-text live-node)
                      (cricbuzz-parse-scores details-node)))
      (if complete-node
          (cons "FINISHED" (cons
                            (enlive-text complete-node)
                            (cricbuzz-parse-scores details-node)))
        (list nil)))))

(defun cricbuzz-insert-match (match-node)
  "Format match node for preview"
  (let ((title (cricbuzz-get-title match-node))
        (time (cricbuzz-get-time match-node))
        (props (cricbuzz-get-properties match-node))
        (url (cricbuzz-get-url match-node))
        (status (cricbuzz-get-status match-node)))
    (insert (concat "* " title "\n"))
    (insert (concat "SCHEDULED: " time "\n"))
    ;; If status is available
    (if (first status)
        (progn
          (org-todo (first status))
          (insert (concat "+ Status :: " (second status) "\n"))
          (insert (concat "+ Scores :: \n"))
          (insert (concat "  + " (third status) "\n"))
          (insert (concat "  + " (fourth status) "\n"))))
    (insert "\n")
    (org-set-property "VENUE" (second props))
    (org-set-property "DESCRIPTION" (first props))
    (org-set-property "URL" (concat "[[" url "][cricbuzz-url]]"))))

;;;###autoload
(defun cricbuzz-get-live-scores ()
  "Display live scores in a buffer"
  (interactive)
  (let ((main-node (first (enlive-get-elements-by-class-name
                           (enlive-fetch cricbuzz-live-url)
                           "cb-schdl")))
        (buffer (get-buffer-create "live cricket scores")))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "#+TITLE: Live Cricket Scores\n")
    (insert "#+TODO: LIVE | FINISHED\n\n")
    (insert (format-time-string "Last updated [%Y-%m-%d %a %H:%M] \n"))
    (insert (concat "~scores via [[" cricbuzz-base-url "][cricbuzz]]~\n\n\n"))
    (cricbuzz-index-mode)
    (mapc 'cricbuzz-insert-match
          (enlive-get-elements-by-class-name main-node "cb-mtch-lst"))
    (setq buffer-read-only t)
    (switch-to-buffer buffer)
    (goto-char (point-min))
    ;; Save schedule and add to agenda
    (write-region (point-min) (point-max) cricbuzz-schedule-file)
    (add-to-list 'org-agenda-files cricbuzz-schedule-file)
    (flyspell-mode-off)))

;; Parse scorecard

(defun cricbuzz-get-last-url ()
  (search-backward "cricbuzz-url")
  ;; Take a margin of 5 chars to get url
  ;; TODO: Get org property directly
  (goto-char (- (match-beginning 0) 5))
  (thing-at-point 'url))

(defun cricbuzz-get-scorecard ()
  "Display scorecard in a buffer"
  (let ((main-node (enlive-fetch (cricbuzz-get-last-url))))
    (message "TODO: Not yet implemented")))

(defvar cricbuzz-index-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "r") '(lambda () (interactive) (cricbuzz-get-live-scores)))
    (define-key map (kbd "RET") '(lambda () (interactive) (cricbuzz-get-scorecard)))
    map)
  "Keymap for cricbuzz-index major mode")

(defvar cricbuzz-score-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "r") '(lambda () (interactive) (cricbuzz-get-scorecard)))
    map)
  "Keymap for cricbuzz-index major mode")

;;;###autoload
(define-derived-mode cricbuzz-index-mode org-mode
  "Cricbuzz-Index"
  "Major mode for cricbuzz live scores")

;;;###autoload
(define-derived-mode cricbuzz-score-mode org-mode
  "Cricbuzz-Score"
  "Major mode for viewing cricbuzz scorecards")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cb\\'" . cricbuzz-score-mode))

(provide 'cricbuzz)

;;; cricbuzz.el ends here
