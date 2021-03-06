;(defmacro defgeneric-support-type (name lambda-list &body body)
;  (let ((hash-name (gensym))
;        (gene-name (gensym)))
;    `(progn
;       (defgeneric ,(read-from-string (format nil "~A-generic" name)) ,lambda-list)
;       (let ((,hash-name (make-hash-table :test 'equal)))
;         (defun ,name ,lambda-list
;           (block nil
;             (maphash #'(lambda (x y)
;                          (if (do ((type-list x (cdr type))
;                                   (args-list ,(cons list
;                                                     lambda-list)
;                                              (cdr args-list)))
;                                  ((or (null type-list)
;                                       (null args-list))
;                                   (return-from ,name
;                                     (funcall y ,@lambda-list)))
;                                (if (not (typep (car args-list)
;                                                (car type-list)))
;                                    (return))))))
;             (funcall ,(read-from-string (format nil "~A-generic" name))
;                      ,@lambda-list))))))))

(defvar *generic-type-table* (make-hash-table :test 'eq))
(defvar *save-type-list-table* (make-hash-table :test 'eq))

(defmacro defgeneric-support-type (name lambda-list)
  (let (;(hash-name (gensym))
        (gfun-name (gensym "gFun-Name"))
        (type-list (gensym "type-list"))
        (args-list (gensym "args-list"))
        (lambda-arg-1 (gensym "lambda-arg-1"))
        (lambda-arg-2 (gensym "lambda-arg-2"))
        (hash-table (make-hash-table :test 'equal)))
    `(progn (setf (gethash ',name *generic-type-table*) ',gfun-name
                  (gethash ',name *save-type-list-table*) ,hash-table)
            (defgeneric ,gfun-name ,lambda-list)
            (defun ,name ,lambda-list
              ;(block nil
                (maphash #'(lambda (,lambda-arg-1 ,lambda-arg-2)
                             (do ((,type-list ,lambda-arg-1 (cdr ,type-list))
                                  (,args-list ,(cons 'list lambda-list)
                                              (cdr ,args-list)))
                                 ((or (null ,type-list)
                                      (null ,args-list))
                                  (return-from ,name
                                    (funcall ,lambda-arg-2 ,@lambda-list)))
                               (cond ((null (car ,type-list)))
                                     ((not (typep (car ,args-list)
                                                  (car ,type-list)))
                                      (return)))))
                         (gethash ',name *save-type-list-table*))
              (funcall (gethash ',name *generic-type-table*)
                       ,@lambda-list)))))

;;like this (defmethod-sup-type test (x (y (type my-number))) body)

(defmacro defmethod-support-type (name lambda-list &body body)
  (let* ((f #'(lambda (x) x))
         (resl (mapcar #'(lambda (x)
                          (cond ((atom x) nil)
                                ((atom (cadr x)) nil)
                                ((eq (caadr x) 'type) t)))
                       lambda-list)))
    (if (member-if f resl)
        `(setf (gethash ',(mapcar #'(lambda (x y)
                                      (if x (cadadr y)))
                                  resl
                                  lambda-list)
                        (gethash ',name *save-type-list-table*))
               #'(lambda ,(mapcar (labels ((iter (lst)
                                             (cond ((atom lst) lst)
                                                   ((atom (car lst))
                                                    (car lst))
                                                   (t (iter (cdr lst))))))
                                    #'iter)
                                  lambda-list)
                   ,@body))
        `(defmethod ,(gethash name *generic-type-table*) ,lambda-list ,@body))))
