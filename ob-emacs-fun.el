;;; ob-emacs-fun.el --- org-babel functions for evaluation of emacs-fun, 
;;; that is special emacs-lisp code for functions with diagrams.
;;; Author: Zenon@github
;;; copied from ob-emacs-lisp.el

;;
;; There might be cargo cult below. Constructive criticism is appreciated.
;;

(require 'ob)
(require 'ob-emacs-lisp)
(eval-when-compile (require 'ob-comint)) ; why?

;; http://stackoverflow.com/a/15627439 Hat tip to slitvinov
(defadvice org-babel-parse-src-block-match  
  (after org-babel-add-switches activate)
  "Add extra-sw to 3th element of the return value"
  (let ((extra-sw "+n -r -l \";(ref:%s)\""))
    (setq v (vconcat ad-return-value))
    (aset v 3 (concat (elt v 3) " " extra-sw))
    (setq ad-return-value (append v nil))))


(defun string-search-and-replace (search replace string)
  "Replace all instances of SEARCH with REPLACE in STRING."
  (replace-regexp-in-string (regexp-quote search) replace string t t))

(defun normalised-buffer-name ()
  (let* ((b0 (buffer-name))
	 (b1 (string-search-and-replace "\*" "x" b0))
	 (b2 (string-search-and-replace " " "_" b1))
	 (b3 (string-search-and-replace "\(" "_" b2))
	 (b4 (string-search-and-replace "\)" "_" b3)))
    b4))
	 

(setq block-counter 0)
(setq processed-buffer-name "unknown")

(defadvice org-export
  (before ob-emacs-fun--reset-block-counter activate)
  "Resets block-counter before running the export."
  (setq block-counter 0)
  (setq processed-buffer-name (normalised-buffer-name)))


(defvar org-babel-default-header-args:emacs-fun
  '((:hlines . "no")       ; horizontal lines are cut from tables
    (:colnames . "no")     ; header is not cut from tables
    (:results . "html")    ; HTML not escaped. (value taken, output ignored)
    (:exports . "both"));  ; code and value
  "Default arguments for evaluating an emacs-fun source block.")

(declare-function orgtbl-to-generic "org-table" (table params))

(defun emacs-fun-defaults (settings)
  "Merges these defaults into the current state. These settings win."
  (setq defaults (merge-alists settings defaults))
  "")

(defun org-babel-expand-body:emacs-fun (body params) 
  "Ignoring all the params."
  (format "(%s\n)" body))

(setq org-babel-debug:emacs-fun t)

(setq svg-template
"<object data='%s' type='image/svg+xml' 
    title='%s'
    width='%s' height='%s'>
</object>")

(setq defaults '((xmin . 0) (xmax . 500) 
		 (ymin . 0) (ymax . 500)
		 (width . 600) (height . 400)
		 (xlabel . "x") (ylabel . "y")
		 (title . "Picture")))


(defun merge-alists (list defaults)
  "Merges second list as defaults into first. I.e. first list wins."
   (let ((result (copy-alist list)))
      (dolist (x defaults)
         (when (null (assoc (car x) result))
        (add-to-list 'result x)))
      result))


;TODO stubs not used, still
(defun create-axis (xname yname xstubs ystubs)
  (let ((xstubs-str (if xstubs
	     (concat "  grid: color=green\n  selflocatingstubs: text\n"
		     (apply 'concat 
			    (mapcar (lambda (x) 
				      (format "\t%s\t%s\n" (car x) (cdr x)))
				    xstubs)))
			"  stubs: inc"))
	(ystubs-str 
	 (if ystubs
	     (concat "  selflocatingstubs: text\n"
		     (apply 'concat 
			    (mapcar (lambda (x) 
				      (format "\t%s\t%s\n" (car x) (cdr x)))
				    ystubs)))
			"  stubs: inc")))
    (format
"

#proc xaxis
  label: %s
%s

#proc yaxis
  label: %s
%s

" xname xstubs-str yname ystubs-str)))

(setq colors '(black red blue green orange black black black black black))

(defun create-data-line (fun-list x ymin ymax)
  (concat (number-to-string x) "\t"
	  (apply 'concat
		 (mapcar 
		  (lambda (n) 
		    (let ((myfun (cons 'lambda (cdr (nth n fun-list)))))
		      (concat (condition-case nil
				  (let ((y (funcall myfun x)))
				    (if (and (>= y ymin)
					     (<= y ymax))
					(number-to-string y)
				      "-"))
				 ('error "x"))
			      "\t")))
		  (number-sequence 0 (- (length fun-list) 1))))
	  "\n"))

(defun create-ploticus-data (fun-list xs ymin ymax)
  (apply 
   'concat 
   (mapcar (lambda (n) 
	     (create-data-line fun-list (nth n xs) ymin ymax))
	   (number-sequence 0 (- (length xs) 1)))))

(defun create-header (title xmin xmax ymin ymax)
  (format
"

#call $setdatefmt( \"dd.mm.yyyy\" )
#set today = $todaysdate()
#call $settimefmt( \"HH:MM:SS\" )
#set now = $time()

#proc areadef
  title: %s
  title2: (drawn @today, @now)
  title2details: align=R size=6
  rectangle: 1 1 5 3
  xrange: %s %s
  yrange: %s %s

" title xmin xmax ymin ymax))

(defun create-line-plot (n name y color)
  (format
"

#proc lineplot
  xfield: 1
  yfield: %s
  linedetails: color=%s style=%s
  legendlabel: %s

" y color n name))

(defun create-ploticus-code (fun-list params)
  (message 
   (let* ((title (cdr (assoc 'title params)))
	  (xmin (cdr (assoc 'xmin params)))
	  (xmax (cdr (assoc 'xmax params)))
	  (ymin (cdr (assoc 'ymin params)))
	  (ymax (cdr (assoc 'ymax params)))
	  (xlabel (cdr (assoc 'xlabel params)))
	  (ylabel (cdr (assoc 'ylabel params)))
	  (xstubs (cdr (assoc 'xstubs params)))
	  (ystubs (cdr (assoc 'ystubs params)))
	  (xs (number-sequence xmin xmax (/ (- xmax xmin) 50.0))))
     (concat
"
#proc getdata
  data:
"
       (create-ploticus-data fun-list xs ymin ymax)
       (create-header title xmin xmax ymin ymax)
       (create-axis xlabel ylabel xstubs ystubs)
       (apply 'concat (mapcar (lambda (x) 
				(create-line-plot
				 x
				 (symbol-name (car (nth x fun-list)))
				 (number-to-string (+ x 2)) 
				 (nth x colors)))
			      (number-sequence 0 (- (length fun-list) 1))))
"

#proc legend
  location: min+1 min-0.35
  format: down
"))))



(defun create-pic (fun-list config file-name)
  ""
  (let ((params nil)
	ploticus-code
	(in-file (org-babel-temp-file "ploticus-")))
    (message (format "FUU: %s" fun-list))
    (message (format "My configuration (w/o defaults) is %s" config))
    (if fun-list
	(progn
	  (with-temp-file in-file
	    (insert (create-ploticus-code fun-list config)))
	  (org-babel-eval
	   (concat "pl"
	     " -f " (org-babel-process-file-name in-file)
	     " -svg "
	     " -o " (org-babel-process-file-name file-name)) "")
	  ))
    fun-list))


(defun org-babel-execute:emacs-fun (body params)
  "Execute a block of emacs-fun code with Babel."
  (setq block-counter (+ block-counter 1))
  (save-window-excursion
    ((lambda (result)
       (if (or (member "scalar" (cdr (assoc :result-params params)))
	       (member "verbatim" (cdr (assoc :result-params params))))
	   (let ((print-level nil)
		 (print-length nil))
	     (format "%S" result))
	 (org-babel-reassemble-table
	  result
	  (org-babel-pick-name (cdr (assoc :colname-names params))
			       (cdr (assoc :colnames params)))
	  (org-babel-pick-name (cdr (assoc :rowname-names params))
			       (cdr (assoc :rownames params))))))
     (let* ((file-name 
	     (if (cdr (assoc :file params))
		 (cdr (assoc :file params))
	       (concat processed-buffer-name
		       "__figure_"
		       (number-to-string block-counter)
		       ".svg")))
	    (todo (org-babel-expand-body:emacs-fun body params))
	    ;; Is this a sensible way to log here?
	    (dummy1 (if org-babel-debug:emacs-fun 
			(message (concat "2B evaluated:\n" todo))
		      nil))
	    (fun-list (condition-case exc
			  (read todo)
			('error (message 
				 (format "Code could not be evaluated. %s" 
					 exc)))))
	    ;; first element might contain params
	    config 
	    (dummy2 (if (and fun-list (car fun-list))
			(if (listp (caar fun-list))
			    (progn (setq config (car fun-list))
				   (setq fun-list (cdr fun-list))))))
	    (config-including-defaults (merge-alists config defaults))
	    (result (create-pic 
		     fun-list
		     config-including-defaults
		     file-name)))
       (format svg-template file-name 
	       (cdr (assoc 'title config-including-defaults))
	       (cdr (assoc 'width config-including-defaults))
	       (cdr (assoc 'height config-including-defaults)))))))

(provide 'ob-emacs-fun)
