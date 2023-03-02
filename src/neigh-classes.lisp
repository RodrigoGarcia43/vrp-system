(in-package :vrp)

(def-vrp-class counting-basic-solution ()
  ((num-clients) (routes))

  :documentation "A basic solution for counting."
  :constructor (new-counting-basic-solution)
  :print-object-string ("<counting-basic-solution ~a, ~a>" num-clients routes)
  :slots-for-obj= (num-clients routes))

(def-vrp-class counting-op-init ()
	   ()		

	   :documentation "A class to initialize the 'root' of the neighborhood tree."
	   :constructor (new-init-operation)
	   :print-object-string ("<counting-op-init>")
	   :slots-for-obj= ())

(def-vrp-class counting-op-select-route ()
  ((operation-id) (route-symbol))
  :documentation "A class to represent the select-route operation."
  :constructor (new-r-operation)
  :print-object-string ("<r-op id: ~a, route: ~a>" operation-id route-symbol)
  :slots-for-obj= (operation-id route-symbol))

(def-vrp-class counting-op-select-client ()
  ((operation-id) (route-symbol) (client-symbol))
  :documentation "A class to represent the select-client operation."
  :constructor (new-a-operation)
  :print-object-string ("<a-op id: ~a, route: ~a, client: ~a>" operation-id route-symbol client-symbol)
  :slots-for-obj= (operation-id route-symbol client-symbol))

(def-vrp-class counting-op-insert-client ()
  ((operation-id) (route-symbol) (client-symbol))
  :documentation "A class to represent the insert-client operation."
  :constructor (new-b-operation)
  :print-object-string ("<b-op id: ~a, route: ~a>, client: ~a" operation-id route-symbol client-symbol)
  :slots-for-obj= (operation-id route-symbol client-symbol))

(def-vrp-class counting-op-swap-clients ()
 ((operation-id) (client-1-symbol) (client-2-symbol))
 :documentation "A class to represent the swap-clients operation."
 :constructor (new-c-operation)
 :print-object-string ("<c-op id: ~a, client 1: ~a, client 2: ~a>" operation-id client-1-symbol client-2-symbol)
 :slots-for-obj= (operation-id client-1-symbol client-2-symbol))

(def-vrp-class counting-op-select-subroute ()
 ((operation-id) (route-symbol) (subroute-symbol))
 :documentation "A class to represent the select-subroute operation."
 :constructor (new-e-operation)
 :print-object-string ("<e-op id: ~a, route: ~a, subroute: ~a>" operation-id route-symbol subroute-symbol)
 :slots-for-obj= (operation-id route-symbol subroute-symbol))

(def-vrp-class counting-op-insert-subroute ()
 ((operation-id) (route-symbol) (subroute-symbol))
 :documentation "A class to represent the insert-subroute operation."
 :constructor (new-f-operation)
 :print-object-string ("<f-op id: ~a, route: ~a, subroute: ~a>" operation-id route-symbol subroute-symbol)
 :slots-for-obj= (operation-id route-symbol subroute-symbol))

(def-vrp-class counting-op-swap-subroutes ()
 ((operation-id) (subroute-1-symbol) (subroute-2-symbol))
 :documentation "A class to represent the swap-subroutes operation."
 :constructor (new-g-operation)
 :print-object-string ("<r-op id: ~a, subroute 1: ~a, subroute 2: ~a>" operation-id subroute-1-symbol subroute-2-symbol)
 :slots-for-obj= (operation-id route-symbol subroute-2-symbol))

(def-vrp-class counting-op-reverse-subroute ()
 ((operation-id) (subroute-symbol))
 :documentation "A class to represent the reverse-subroute operation."
 :constructor (new-h-operation)
 :print-object-string ("<h-op id: ~a, subroute: ~a>" operation-id subroute-symbol)
 :slots-for-obj= (operation-id subroute-symbol))

(def-vrp-class root-node ()
  ((node-id) (total) (child)) 
  :documentation "A class to represent a root-node in the neighberhood-tree."
  :constructor (new-root-node)
  :print-object-string ("<root-node id: ~a, count: ~a, child: ~a>" node-id total child)
  :slots-for-obj= (node-id total child))

(def-vrp-class r-node ()
  ((node-id     :initarg :node-id
		:initform 0)
   (total	 :initform 0)
   (parent	 :initarg :parent
		 :initform nil)
   (at-parent     :initarg :at-parent
		  :initform 1)
   ;; a list of pairs (child . route-number)
   (children   :initform nil)) 
  :documentation "A class to represent a select-root operation in the neighberhood-tree."
  :constructor (new-r-node)
  :print-object-string ("<r-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent children)
  :slots-for-clone (node-id total parent at-parent children))

(def-vrp-class a-node ()
  ((node-id  :initarg :node-id
	     :initform 0)
   (total :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (at-parent     :initarg :at-parent
		  :initform 1)
   (route-number  :initarg :route-number)
   (possibilities   :initform 0)
   ;; a list of pairs (child . route-number)
   (child   :initform nil)) 
  :documentation "A class to represent a select-client operation in the neighberhood-tree."
  :constructor (new-a-node)
  :print-object-string ("<a-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent route-number possibilities child)
  :slots-for-clone (node-id total parent at-parent route-number possibilities child))

(def-vrp-class b-node ()
  ((node-id  :initarg :node-id
	     :initform 0)
   (total :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (at-parent     :initarg :at-parent
		  :initform 1)
   (route-number  :initarg :route-number)
   (possibilities   :initform 0)
   (select-op-id :initarg :select-op-id)
   ;; a list of pairs (child . route-number)
   (child   :initform nil)) 
  :documentation "A class to represent an insert-client operation in the neighberhood-tree."
  :constructor (new-b-node)
  :print-object-string ("<b-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent route-number possibilities select-op-id child)
  :slots-for-clone (node-id total parent at-parent route-number possibilities select-op-id child))

(def-vrp-class c-node ()
  ((node-id  :initarg :node-id
	     :initform 0)
   (total :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (at-parent :initarg :at-parent
	      :initform 1)
   (select-op1-id :initarg :select-op1-id)
   (select-op2-id :initarg :select-op2-id)
   (child   :initform nil)) 
  :documentation "A class to represent a swap-clients operation in the neighberhood-tree."
  :constructor (new-c-node)
  :print-object-string ("<c-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent select-op1-id select-op2-id child)
  :slots-for-clone (node-id total parent at-parent select-op1-id select-op2-id child))

(def-vrp-class e-node ()
  ((node-id  :initarg :node-id
	     :initform 0)
   (total :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (at-parent :initarg :at-parent
	      :initform 1)
   (route-number :initarg :route-number)
   ;; a list of pairs (child . (subroute-len . positions))
   (children :initform nil)) 
  :documentation "A class to represent a select-subroute operation in the neighberhood-tree."
  :constructor (new-e-node)
  :print-object-string ("<e-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent route-number children)
  :slots-for-clone (node-id total parent at-parent route-number children))

(def-vrp-class f-node ()
  ((node-id  :initarg :node-id
	     :initform 0)
   (total :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (at-parent :initarg :at-parent
	      :initform 1)
   (route-number :initarg :route-number)
   ;; a list of pairs (child . (subroute-len . positions))
   (possibilities :initform 0)
   (select-op-id :initarg :select-op-id)
   (child :initform nil)) 
  :documentation "A class to represent an insert-subroute operation in the neighberhood-tree."
  :constructor (new-f-node)
  :print-object-string ("<e-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent route-number possibilities select-op-id child)
  :slots-for-clone (node-id total parent at-parent route-number possibilities select-op-id child))

(def-vrp-class g-node ()
  ((node-id  :initarg :node-id
	     :initform 0)
   (total :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (at-parent :initarg :at-parent
	      :initform 1)
   (select-op1-id :initarg :select-op1-id)
   (select-op2-id :initarg :select-op2-id)
   (child   :initform nil))  
  :documentation "A class to represent a swap-subroutes operation in the neighberhood-tree."
  :constructor (new-g-node)
  :print-object-string ("<e-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent select-op1-id select-op2-id child)
  :slots-for-clone (node-id total parent at-parent select-op1-id select-op2-id child))

(def-vrp-class h-node ()
  ((node-id  :initarg :node-id
	     :initform 0)
   (total :initform 0)
   (parent :initarg :parent
	   :initform nil)
   (at-parent :initarg :at-parent
	      :initform 1)
   (select-op-id :initarg :select-op-id)
   (child :initform nil))   
  :documentation "A class to represent a reverse-subroute operation in the neighberhood-tree."
  :constructor (new-h-node)
  :print-object-string ("<e-node id: ~a, count: ~a, parent: ~a, at-parent: ~a>" node-id total parent at-parent)
  :slots-for-obj= (node-id total parent at-parent select-op-id child)
  :slots-for-clone (node-id total parent at-parent select-op-id child))

(def-vrp-class nil-node ()
  ((total :initform 0
	  :initarg :total-count)
   (parent :initarg :parent
	   :initform nil)
   (at-parent :initarg :at-parent
	      :initform 1))   
  :documentation "A class to represent a nil-node in the neighberhood-tree."
  :constructor (new-nil-node)
  :print-object-string ("<nil-node count: ~a, parent: ~a, at-parent: ~a>" total parent at-parent)
  :slots-for-obj= (total parent at-parent)
  :slots-for-clone (total parent at-parent))

(def-vrp-class search-state ()
  ((number-of-analysed-solutions :initform 0)
   (solution-mapper :initarg :solution-mapper
		    :initform (make-hash-table)))
  :documentation "A class that stores the state of a search over a set of solutions."
  :constructor (new-search-state)
  :print-object-string ("<search-state, analysed-solutions: ~a>" number-of-analysed-solutions)
  :slots-for-obj= (number-of-analysed-solutions solution-mapper)
  :slots-for-clone (number-of-analysed-solutions solution-mapper))

(def-vrp-class neighborhood-basic-region ()
  ((first-index :initarg :first-index
		  :initform nil)
     (info :initarg :info)
     (subtree-sols :initarg :subtree-sols
		   :initform 0)
     (cardinality :initarg :cardinality
		  :initform 0))
  :documentation "A class that represents a basic region in the neighborhood."
  :constructor (new-neighborhood-region)
  :print-object-string ("<neighborhood-basic-region, first-index: ~a, info: ~a, cardinality: ~a>" first-index info cardinality)
  :slots-for-obj= (first-index info subtree-sols cardinality)
  :slots-for-clone (first-index info subtree-sols cardinality))

(def-vrp-class neighborhood-region ()
      ((number-id :initarg :number-id)
       (index-range :initarg :index-range)
       (cardinality :initarg :cardinality
		    :initform 0)
       (basic-region-list :initarg :basic-region-list
			  :initform nil)
       (basic-indexes :initarg :basic-indexes
		      :initform nil)
       (basic-keys :initarg :basic-keys
		   :initform nil)
       (search-state :initform (make-instance 'search-state))
       (neighborhood :initarg :neighborhood
		     :initform nil))
  :documentation "A class that represents a region in the neighborhood."
  :constructor (new-neighborhood-region)
  :print-object-string ("<neighborhood-region, number-id: ~a, index-range: ~a, cardinality: ~a, neighborhood: ~a>" number-id index-range cardinality neighborhood)
  :slots-for-obj= (number-id index-range cardinality basic-region-list basic-indexes basic-keys search-state neighborhood)
  :slots-for-clone (number-id index-range cardinality basic-region-list basic-indexes basic-keys search-state neighborhood))

(def-vrp-class neighborhood-tree ()
      ((problem :initarg :problem
		:initform nil)
       (solution :initarg :solution)
       (criterion :initarg :criterion)
       (counting-solution :initarg :counting-solution)
       (tree :initarg :tree)
       (cardinality :initarg :cardinality 
		    :initform 0)
       (number-of-regions :initform nil)
       (region-list :initarg :region-list
		    :initform nil)
       (region-indexes :initarg :region-indexes
		       :initform nil)
       (region-keys :initarg :region-keys
		    :initform nil)
       (search-state :initform (make-instance 'search-state)))
  :documentation "A class that works as an API for the neighborhood-tree."
  :constructor (new-neighborhood-tree)
  :print-object-string ("<neighborhood-tree, problem: ~a, solution: ~a, criterion: ~a, counting-solution: ~a, cardinality: ~a>" problem solution criterion counting-solution cardinality)
  :slots-for-obj= (problem solution criterion counting-solution tree cardinality number-of-regions region-list region-indexes region-keys search-state)
  :slots-for-clone (problem solution criterion counting-solution tree cardinality number-of-regions region-list region-indexes region-keys search-state))
