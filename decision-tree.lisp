;; License : FreeBSD =================================================
;; Author@Michael E. Karpeles
;; Desc  @This is a Decision Tree program.
;;
;;   * Instance - an example / entry of the data set
;;   * Feature  - a category / field of the data set
;;   * Class    - a possible value for an example for a given featuer")
;;
;; THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND 
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE FREEBSD 
;; PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
;; POSSIBILITY OF SUCH DAMAGE.
;; Site: http://www.freebsd.org/copyright/freebsd-license.html

;(defpackage :bk-tree-test (:use :cl :bk-tree))
;(in-package :bk-tree-test)

;(defvar *words* nil)    
;(defvar *tree* (make-instance 'bk-tree))

;; TESTS --------------------------------------------------------------------|
(defun test-information-gain (attribute-n file-root)
  "TEST: (test-information-gain 3 \"balloons\")"
  (let* ((dataset   (load-project file-root))
	 (attribute (aref (data-set-features dataset) attribute-n))
	 (examples  (data-set-examples dataset))) 
    (information-gain attribute examples)))

(defun test-remainder (attribute-n file-root)
  "TEST: (test-remainder 3 \"balloons\")"
  (let* ((dataset   (load-project file-root))
	 (attribute (aref (data-set-features dataset) attribute-n))
	 (examples  (data-set-examples dataset)))
    (remainder attribute examples)))

(defun test-entropy (file-root)
  "TEST: (test-entropy (data-set-examples (load-project \"balloons\")"
  (entropy (data-set-examples (load-project file-root))))

(defun test-branches-of (branch attribute-n file-root)
  "TEST: (test-partitions-of 2 (load-project \"balloons\")"
  (let* ((dataset   (load-project file-root))
	 (attribute (aref (data-set-features dataset) attribute-n))
	 (examples  (data-set-examples dataset))) 
    (nth branch (partitions-of attribute examples))))

(defun test-unique-class-probs-for (partition attribute-n file-root)
  (unique-class-probs-for (nth partition (test-partitions-of attribute-n file-root))))

(defun test-partitions-of (attribute-n file-root)
  "TEST: (test-partitions-of 2 (load-project \"balloons\")"
  (let* ((dataset   (load-project file-root))
	 (attribute (aref (data-set-features dataset) attribute-n))
	 (examples  (data-set-examples dataset))) 
    (partitions-of attribute examples)))

;; STRUCTURES ----------------------------------------------------------------|
(defstruct decision-tree
  "Structure for decision tree.
   Beta: Perhaps include breadth and depth information?"
  ;; Should use (error "~S must be provided" 'root-node)
  (root nil :type (or null node leaf)))

(defstruct node
  "A tree node represents a feature (attribute), a subset of example instances,
   and a branch for each class represented in the example list for said attribute"
  (via      nil)
  (feature  nil)) ; selected feature (attribute)

(defstruct leaf
  (via      nil)
  (outcome  nil))

(defstruct data-set
  (examples nil)
  (features nil)
  (goals    nil :type (or null goal)))

(defstruct feature
  (id      nil)
  (name    nil)  ; feature label / name
  (classes nil)) ; domain / range of values feature can assume

(defstruct goal
  (name    nil)  
  (classes nil)
  (counts  nil)) ; Goal classifications / categories
  
(defstruct example
  (id     nil)  ; A unique example # (set by me)
  (label  nil)  ; A unique example label (set by data)
  (values nil)  ; An attribute struct
  (class  nil)) ; Classification / result


;; Extra Credit -------------------------------------------------------------|

;(defun test-classify (unknown-instance names-filepath &optional(data-filepath names-filepath))
;  (classify unknown-instance (load-project names-filepath data-filepath)))

;(defun classify (unknown-instance training-data)
;  (let ((evolved-decision-tree (evolve-decision-tree (training-data))))
		 

;; DRIVERS ------------------------------------------------------------------|
(defun run (names-filepath &optional(data-filepath names-filepath))
  ;;(load-project names-filepath data-filepath))
  (evolve-decision-tree (load-project names-filepath data-filepath)))

;; DECISION TREE COMPONENTS -------------------------------------------------|
(defun evolve-decision-tree (data-set)
  "STAT: IP"
  (let ((default nil))
    (labels ((learn (features examples tree)
	       (if (null examples) 
		   (progn (format t "==========DECISION TREE==========~%")
			  (format t "* Statistics to go here...       ~%")
			  (format t "=================================~%")
			  tree)
		   ;; If all examples in data-set have same classification
		   (if (= (length (unique-class-probs-for examples)) 1)
		       ;; Return a leaf node representing class / outcome
		       (cons (example-class  (first examples)) tree)
		       (let* ((feature       (feature-selector features examples))
			      (partitions    (partitions-of    feature  examples))
			      (rest-features (remove (feature-id feature) features))
			      (updated-tree  (cons (make-node :feature (feature-name feature)
							      :via (nth (feature-id feature) (example-values (first (first partitions))))) tree)))
			 (loop for partition in partitions
			    if (= (length (unique-class-probs-for partition)) 1)
			    collect (make-leaf :outcome (example-class (first partition))
					       :via (nth (feature-id feature) (example-values (first partition)))) into leaves
			    else
			    collect (learn rest-features partition default) into nodes
			    finally (return (append updated-tree (cons leaves nodes)))))))))
      (print-tree-h (learn (data-set-features data-set) (data-set-examples data-set) default)))))


;; ATTRIBUTE SELECT: Entropy & Information Gain =============================|

(defun feature-selector (features examples)
  "STAT: IP
   TEST: (feature-selector (load-project \"zoo\"))
   DESC: Selects / splits on the attribute that maximizes information gain.
         Disregards the first attribute (unique ID) and 
         the last attribute (concept/outcome classes)"
  (let* ((features     (cdr (butlast (coerce features 'list))))
	 (best-feature (first features))
	 (best-ig      (information-gain best-feature examples)))
    (if (null features)
	nil
	(progn (loop for feature in (cdr features) do
		    (when (> (information-gain feature examples) best-ig)
		      (setf best-feature feature)
		      (setf best-ig (information-gain feature examples))))
	       best-feature))))

;; Information Gain ---------------------------------------------------------|
(defun information-gain (feature examples)
  "STAT: Done
   DESC: An Information-Theoretic Metric rule which uses a metric
         of theoretical information gain (in bits) maximization to 
         select a feature on which a split should be performed."
  (let ((parent-entropy (entropy examples)))
    (- parent-entropy (remainder feature examples))))

;; Remainder ----------------------------------------------------------------|
(defun remainder (feature examples)
  "STAT: Done
   DESC: Computes the remainder(feature) given:
         data-set-examples and a feature."
  (let ((feature-partitions (partitions-of feature examples)))
    (loop for partition in feature-partitions
       sum (* (/ (length partition) (length examples))
	      (entropy partition)))))

;; Information Content / Entropy --------------------------------------------|
(defun entropy (examples)
  "STAT: Done
   TEST: (entropy (data-set-examples (load-project \"balloons\")))
   DESC: Finds the IC / entropy given examples and goal"
  (let* ((probs (unique-class-probs-for examples)))
    (- (loop for prob in probs
	  when (> prob 0)
	  sum (* prob
		 (log prob 2))))))


;; FEATURE PARTITIONING =====================================================|

(defun partitions-of (feature examples)
  "STAT: DONE
   DESC: Partitions examples into as many blocks as there are unique class values
         for examples under the specified feature. Returns vector."
  (let ((blocks (make-array (length (feature-classes feature)) 
			    :initial-element '())))
    (loop for example in examples do
	 (let ((index (position (nth (feature-id feature)
				     (example-values example))
				(feature-classes feature))))
	   (setf (aref blocks index) (cons example (aref blocks index)))))
    (coerce blocks 'list)))

;; List of Class Probabilities within Example Partition ----------------------|
(defun unique-class-probs-for (partition)
  "STAT: DONE
   TEST: (unique-classes-for (car (test-partitions-of 3 \"balloons\")))
   DESC: Returns an a-list whose magnitude (length) is equal to the
         number of unique goal 'classes' which (range) appear over the examples 
         (partition). The keys of the a-list are the unique class value and the 
         values are the number (count) of examples sharing this class / total
         examples in partition"
    (let ((classes (make-hash-table))
	  (total   (length partition)))
      (loop for example in partition do
	   (if (not  (gethash (example-class example) classes))
	       (setf (gethash (example-class example) classes) (/ 1 total))
	       (setf (gethash (example-class example) classes) 
		     (/ (+ (* (gethash (example-class example) classes) 
			      total) 1) total))))
      (map2list classes)))

(defun branches-for (partition)
  ;; A hash-table of branches, sub-partitions of partition for which every example
  ;; in a branch shares the same outcome (goal) class
  (let ((branches (make-hash-table)))
    (loop for example in partition do
       ;; If there does not yet exist a key in the branches hash-table
       ;; s.t. a branch does not yet exist for the class specified by this example...
	 (if (not (gethash (example-class example) branches))
	     ;; Then add a new branch for this class type and add the example
	     (setf (gethash (example-class example) branches) example)
	     ;; Otherwise, push the example onto the branch designated for specified class
	     (setf (gethash (example-class example) branches)
		   (cons example (gethash (example-class example) branches)))))
    (map2list branches)))

(defun map2list (hashmap)
  (loop for key being the hash-keys of hashmap
     using (hash-value value)
     collect value))

;; FILE LOADING AND IO ======================================================|

(defun load-project (names-filepath &optional(data-filepath names-filepath))
  "STAT: DONE
   TEST: (load-project \"zoo\")
   DESC: Loads all the resources for a project and builds the corresponding
         data-set"
  (let* ((examples (load-data  data-filepath))
	 (names    (load-names names-filepath))
	 (features (update-feature-classes examples (car names)))
	 (goals    (update-goal-classes examples (final names))))
    (make-data-set :examples examples
		   :features features
		   :goals    goals)))

(defun load-data (file-path)
  "STAT: DONE
   TEST: (load-names \"zoo\")
   DESC: Loader for *.data decision tree files. Returns a list of 
         data examples / instances."
  (let ((data (load-file (concatenate 'string file-path ".data"))))
    (labels ((loader (entry-num entries examples)
	       (let ((entry (car entries)))
		 (if (null entries)
		     (reverse examples)
		     (loader (+ entry-num 1) 
			     (cdr entries) 
			     (cons (make-example :id      entry-num
						 :label  (first     entry)
						 :values (butlast   entry)
						 :class  (final     entry))
				   examples))))))
      (loader 0 data '()))))


(defun load-names (file-path)
  "STAT: DONE
   TEST: (load-names \"zoo\")
   DESC: Loader for *.name decision tree files. Returns a list containing
         a list of feature (attribute) structs and a goals struct"
  (let* ((lines          (load-file (concatenate 'string file-path ".names")))
	 (feature-names  (cdr (butlast lines)))
	 (goals          (make-goal :name    (final lines) 
				    :classes (first lines)))
	 (features       (loop for name in feature-names
			    counting name into id
			    collect (make-feature :id      (- id 1)
						  :name    name 
						  :classes nil)))
	 (features-array (make-array (length features) :initial-contents features)))
    (list features-array goals)))

(defun update-goal-classes (examples goals)
  "STAT: DONE
   DESC: Updates a count for the #examples having each class"
  (let* ((classes (goal-classes goals))
	 (counts (make-array (length classes) :initial-element 0)))
    (loop named dataset for example in examples do
	 (let ((index (position (example-class example) classes)))
	   (incf (aref counts index))))
    (and (setf (goal-counts goals) counts)
	 goals)))


(defun update-feature-classes (examples features)
  "STAT: UNTESTED
   DESC: Updates the classes for each feature (attribute) according to the
         instances provided in the data-set."
  (loop named dataset for example in examples do
       (loop for index below (array-total-size features) do
	    (when (not (member (nth index (example-values example))
			       (feature-classes (aref features index))))
	      (setf (feature-classes (aref features index))
		    (cons (nth index (example-values example))
			  (feature-classes (aref features index)))))))
  features)	 

;; File IO ------------------------------------------------------------------|
(defun load-file (file-path &optional (read-fun #'file2list))
    "Consumes a file and returns a list of lines as strings"
    (with-open-file (file-stream file-path)
      (funcall read-fun file-stream)))

(defun file2list (file-stream)
  (loop for line = (read file-stream nil 'EOF)
		until (eq line 'EOF)
		collect line))

(defun file2list-of-lines (file-stream)
  "Declared DEPRECATED by Mek"
  (remove "" (loop for line = (read-line file-stream nil 'EOF)
		until (eq line 'EOF)
		collect (string-right-trim '(#\Space #\Tab #\Newline) line))
	  :test #'equal))


;; PRINTING AND OUTPUT ------------------------------------------------------|
(defun print-tree-h (tree)
  "STAT: DONE
   TEST: (print-tree-h '(1 (2 3 (4 5 (6)) 7 8 (9) (10 11) 12 13 (14 (15 (16))))))
   DESC: Builds a human readable tree from a list, depth first"
  (labels ((build-level (depth rest)
	     (if (endp rest) 
		 t
		 (let ((head (first rest)))
		   (if (listp head)
		       (progn
			 (build-level (+ depth 1)      head )
			 (build-level    depth    (cdr rest)))
		       (progn
			 (when (node-p head) 
			   (format t "~vTo <~A?>~&"   (* 3 depth) (node-feature head))
			   (format t "~vT \\ via ~A~&"    (* 3 depth) (node-via     head)))
			 (when (leaf-p head)
			   (format t "~vT+--(~A)--> " (* 3 depth) (leaf-via head))
			   (format t "~A~&"           (leaf-outcome head))
			   (when (not (endp rest))
			     (format t "~vT|~&" (* 3 depth))))
			 (build-level depth (cdr rest))))))))
    (build-level 0 tree)))


;; Utility Functions --------------------------------------------------------|
(defun final (lst)
  "Returns the last element of a list"
  (first (last lst)))