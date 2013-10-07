(in-package :pw-t-as.tree)

(defparameter *trees*
  (make-hash-table)
  "all the trees managed by the package")

(defun make-tree (name class &rest args)
  (setf (gethash name *trees*)
	(apply #'make-instance class args)))

(defun list-trees (&optional (class t))
  (iter:iter
    (iter:for (name tree) :in-hashtable *trees*)
    (when (typep tree class)
      (iter:collect name))))

(defun find-tree (name)
  (gethash name *trees*))

(defun remove-tree (name)
  (remhash name *trees*))

(defun remove-all-trees ()
  (clrhash *trees*))

(defun pprint-list-representation (list-representation &optional stream &rest args &key (tab-colinc 3) (dummy "*") &allow-other-keys)
  "list representation has the element as first, and the children as rest"
  (pprint-logical-block (stream nil)
    (when list-representation
      (let ((element (first list-representation)))
	(write element :stream stream)
	(pprint-tab :section-relative 1 tab-colinc stream)
	(pprint-logical-block (stream nil)
	  (let* ((children (rest list-representation))
		 (first-child (first children))
		 (rest-children (rest children)))
	    (if first-child
		(apply #'pprint-list-representation first-child stream args)
		(princ dummy stream))	    
	    (iter:iter
	      (iter:for child :in rest-children)
	      (if child
		  (progn
		    (pprint-newline :mandatory stream)
		    (apply #'pprint-list-representation child stream args))
		  (princ dummy stream)))))))))
