;;; ox-pelican.el --- Pelican Markdown Back-End for Org Export Engine
;;  Copyright (C) 2015 Christopher Kotfila

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


;;; Commentary:
;; This library implements a Pelican back-end for Org exporter, 
;; based on `md' back-end.  See Org manual for more information.
;; 

;;; Code:
(require 'cl)
(require 'ox-md)

(defvar org-pelican-export-tags '("post" "page"))

(defvar org-pelican-properties '("Title" "Date" "Modified" "Tags"
		   "Category" "Slug" "Authors" "Summary"))

(defvar org-pelican-post-process-cmd "pelican -s pelicanconf.py")

(org-export-define-derived-backend 'pelican 'md
    :menu-entry
  '(?p "Export to Pelican Markdown"
       ((?P "To temporary buffer"
	    (lambda (a s v b) (org-pelican-export-to-buffer a s v b)))
	(?m "To file" (lambda (a s v b) (org-pelican-export-to-file a s v)))))
;	(?o "To file and open"
;	    (lambda (a s v b)
;	      (if a (org-md-export-to-markdown t s v)
;		(org-open-file (org-md-export-to-markdown nil s v)))))))
  :translate-alist
  '((inner-template . org-pelican-inner-template)))

(defun org/pelican-get-preamble ()
  "Get the pelican properties from the current heading's property drawer
and return them as a formatted string for insertion into the markdown"
  (mapconcat
   'identity
   (delq nil
	 (mapcar
	  (lambda (p_name)
	    (let ((p_value (org-entry-get
			    (save-excursion
			      (ignore-errors (org-back-to-heading)
					     (point))) p_name t)))
	      (unless (eq p_value nil)
		(format "%s: %s" p_name p_value))))
	  org-pelican-properties))
   "\n"))
				

(defun org-pelican-inner-template (contents info)
  (concat (org/pelican-get-preamble) "\n\n" contents))


(defun org-pelican-export-to-buffer
    (&optional async subtreep visible-only
	       body-only ext-plist post-process)
  "Export the current file to pelican style markdown and insert 
into a buffer either export the current subtree,  or map over 
elements selecting headlines with tags in 
`org-pelican-export-tags' and export each individual headline
seperately."
  (interactive)
  (let ((buffer-name "*Org Pelican MD Export*"))
    (if subtreep
	;; if it is a single subtree just export normally
	(org-export-to-buffer 'pelican buffer-name
	  async subtreep visible-only nil nil (lambda () (text-mode)))
      ;; other wise map over elements exporting each individually using
      ;; org-export-as and concatenate their content together.  Then place
      ;; everything into the export buffer
      (let ((output (mapconcat 'identity
			       (org-element-map (org-element-parse-buffer) 'headline
				 (lambda (hl)
				   (if (cl-intersection (org-element-property :tags hl)
							org-pelican-export-tags :test 'equal)
				       (save-excursion
					 (goto-char (org-element-property :begin hl))
					 (org-export-as 'pelican t visible-only))
				     ))) "\n\n"))
	    (encoding buffer-file-coding-system)
	  (buffer (get-buffer-create "*Org Pelican MD Export*")))
	(with-current-buffer buffer
	  (erase-buffer)
	  (setq buffer-file-coding-system encoding)
	  (insert output)
	  (goto-char (point-min))
	  (and (functionp post-process) (funcall post-process)))
	(when org-export-show-temporary-export-buffer
	  (switch-to-buffer-other-window buffer))
	buffer))))


(defun org-pelican-export-to-file
  (&optional async subtreep visible-only
	     body-only ext-plist post-process)
  "Export the current file to pelican style markdown and write
to file.  Either write the current subtree,  or map over elements 
selecting headlines with tags in `org-pelican-export-tags' and 
export each individual headline seperately."
  
  (interactive)
  
  (if subtreep
      (let ((outfile (org-export-output-file-name ".md" subtreep)))
	(org-export-to-file 'pelican outfile async subtreep visible-only))
    (let ((encoding (or org-export-coding-system buffer-file-coding-system)))
	;;; Begin Map
	(org-element-map (org-element-parse-buffer) 'headline
	  (lambda (hl)
	    (if (cl-intersection (org-element-property :tags hl)
				 org-pelican-export-tags :test 'equal)
		(save-excursion
		  (goto-char (org-element-property :begin hl))		  
		  (let* ((outfile (org-export-output-file-name ".md" t)) ;force subtreep t
			 (ext-plist (org-combine-plists `(:output-file ,outfile) ext-plist))
			 (output (org-export-as 'pelican t visible-only
						body-only ext-plist)))
		    (if (not (file-writable-p outfile))
			(error "Output file %s not writable" outfile)
		      (with-temp-buffer
			(insert output)
			(let ((coding-system-for-write encoding))
			  (write-file outfile)))
		    ;; (or (and (functionp post-process) (funcall post-process outfile))
		    ;; outfile)))
		    ))))))
	;;; End Map	 
	  ))
  ;; This should not be synchronous
  (message "%s" (shell-command-to-string org-pelican-post-process-cmd)))


(provide 'ox-pelican)
