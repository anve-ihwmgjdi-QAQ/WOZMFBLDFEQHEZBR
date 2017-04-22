;;2.1  标量 向量 矩阵

;;标量symbol or number 向量 array 2, 矩阵 make-array 2

(load "datamethod.lisp")

(defun make-向量 (行)
  (make-array (list 行 1)))
(defun make-矩阵 (行 列)
  (make-array (list 行 列)))

(defun 矩阵行个数 (矩阵) (array-dimension 矩阵 0))
(defun 矩阵列个数 (矩阵) (array-dimension 矩阵 1))

;(defmacro do矩阵 (矩阵 &body body)
;  (let ((行    (gensym))
;        (列    (gensym)))
;    `(let* ((,行 (array-dimension ,矩阵 0))
;            (,列 (array-dimension ,矩阵 1))
;            (array (make-array (list ,行 ,列))))
;       (do ((迭代-行 0 (1+ 迭代-行)))
;           ((= 迭代-行 ,行) array)
;         (do ((迭代-列 0 (1+ 迭代-列)))
;             ((= 迭代-列 ,列))
;           ,@body)))))

(defmacro do矩阵 (length-行 length-列 return-value &body body)
  `(do ((迭代-行 0 (1+ 迭代-行)))
       ((= 迭代-行 ,length-行) ,return-value)
     (do ((迭代-列 0 (1+ 迭代-列)))
         ((= 迭代-列 ,length-列))
       ,@body)))

(defun set-向量or矩阵 (向量 &rest args)
  (let ((list (copy-list args)))
    (do矩阵 (矩阵行个数 向量) (矩阵列个数 向量) 向量
      (unless list (return 向量))
      (setf (aref 向量 迭代-行 迭代-列)
            (car list))
      (pop list))))

(defun 转置2 (矩阵)
  (let* ((行    (矩阵行个数 矩阵))
         (列    (矩阵列个数 矩阵))
         (array (make-array (list 列 行))))
    (do ((i 0 (1+ i)))
        ((= i 行) array)
      (do ((就 0 (1+ 就)))
          ((= 就 列))
        (setf (aref array 就 i)
              (aref 矩阵  i 就))))))

(defun 转置 (矩阵)
  (let* ((行 (矩阵行个数 矩阵))
         (列 (矩阵列个数 矩阵))
         (array (make-array (list 列 行))))
    (do矩阵 行 列 array
      (setf (aref array 迭代-列 迭代-行)
            (aref 矩阵  迭代-行 迭代-列)))))

(defun 矩阵? (x)
  (and (arrayp x)
       (= (array-rank x) 2)))
(defun 向量? (x)
  (and (矩阵? x)
       (or (= 1 (矩阵行个数 x))
           (= 1 (矩阵列个数 x)))))
(deftype 矩阵 () '(satisfies 矩阵?))
(deftype 向量 () '(satisfies 向量?))
(deftype and-矩阵-not-向量! () '(and 矩阵 (not 向量)))
;(deftype and-向量-not-矩阵! () '(and 向量 (not 矩阵)))

(defgeneric-support-type 矩阵相加 (标量/向量/矩阵-1 标量/向量/矩阵-2))

(defmethod-support-type 矩阵相加 (x y)
  (error "在函数[矩阵相加], 输入必须为 标量 或 向量 或矩阵"))
;(defmethod-support-type 矩阵相加 ((x (type and-矩阵-not-向量!))
;                                  (y (type and-矩阵-not-向量!)))
;  (let ((行 (array-dimension x 0))
;        (列 (array-dimension x 1)))
;    (if (and (= 行 (array-dimension y 0))
;             (= 列 (array-dimension y 1)))
;        (do矩阵 行 列 x
;          (setf (aref x 迭代-行 迭代-列)
;                (+ (aref x 迭代-行 迭代-列)
;                   (aref y 迭代-行 迭代-列))))
;        (error "在广义函数[矩阵相加], 两个矩阵的形状不同"))))

(defmethod-support-type 矩阵相加 ((矩阵-1 (type and-矩阵-not-向量!))
                                  (矩阵-2 (type and-矩阵-not-向量!)))
  (let ((行 (矩阵行个数 矩阵-1))
        (列 (矩阵列个数 矩阵-1)))
    (if (and (= 行 (矩阵行个数 矩阵-2))
             (= 列 (矩阵列个数 矩阵-2)))
        (do矩阵 行 列 矩阵-1
          (setf (aref 矩阵-1 迭代-行 迭代-列)
                (+ (aref 矩阵-1 迭代-行 迭代-列)
                   (aref 矩阵-2 迭代-行 迭代-列))))
        (error "在广义函数[矩阵相加], 两个矩阵的形状不同"))))

(flet ((标量+矩阵 (标量 矩阵)
         (do矩阵 (矩阵行个数 矩阵) (矩阵列个数 矩阵) 矩阵
           (setf (aref 矩阵 迭代-行 迭代-列) (+ 标量 (aref 矩阵 迭代-行 迭代-列))))))
  (defmethod-support-type 矩阵相加 ((标量 (type number)) (矩阵 (type 矩阵)))
    (标量+矩阵 标量 矩阵))
  (defmethod-support-type 矩阵相加 ((矩阵 (type 矩阵)) (标量 (type number)))
    (标量+矩阵 标量 矩阵)))

(flet ((向量+矩阵 (向量 矩阵)
         ;;暂时不知道向量的转置该怎么加, 所以把行向量当做标准列向量处理
         (if (= 1 (矩阵行个数 向量)) ;矩阵的行只有一个元素
             (do矩阵 (矩阵行个数 矩阵) (矩阵列个数 矩阵) 矩阵
               (setf (aref 矩阵 迭代-行 迭代-列)
                     (+ (aref 矩阵 迭代-行 迭代-列)
                        (aref 向量 0 迭代-列))))
             (do矩阵 (矩阵行个数 矩阵) (矩阵列个数 矩阵) 矩阵
               (setf (aref 矩阵 迭代-行 迭代-列)
                     (+ (aref 矩阵 迭代-行 迭代-列)
                        (aref 向量 迭代-行 0)))))))
  (defmethod-support-type 矩阵相加 ((向量 (type 向量)) (矩阵 (type 矩阵)))
    (向量+矩阵 向量 矩阵))
  (defmethod-support-type 矩阵相加 ((矩阵 (type 矩阵)) (向量 (type 向量)))
    (向量+矩阵 向量 矩阵)))

;;2.2 矩阵和向量相乘
(defun 矩阵相乘-元素对应乘积 (矩阵1 矩阵2)
  (let ((行 (矩阵行个数 矩阵-1))
        (列 (矩阵列个数 矩阵-2)))
    (if (and (= 行 (矩阵行个数 矩阵-2))
             (= 列 (矩阵列个数 矩阵-2)))
        (do矩阵 行 列 矩阵-1
          (setf (aref 矩阵-1 迭代-行 迭代-列)
                (* (aref 矩阵-1 迭代-行 迭代-列)
                   (aref 矩阵-2 迭代-行 迭代-列))))
        (error "在函数[矩阵相乘-元素对应乘积], 两个矩阵的形状不同"))))

;(defgeneric-support-type 矩阵相乘 (标量/向量/矩阵-1 标量/向量/矩阵-2))

;(defmethod-support-type 矩阵相乘 ((矩阵-1 (type and-矩阵-not-向量!))
;                                  (矩阵-2 (type and-矩阵-not-向量!)))
;  (let* ((new-行 (矩阵行个数 矩阵-1))
;         (new-列 (矩阵列个数 矩阵-2))
;         (new-矩阵 (make-array (list new-行 new-列)))
;         (k (矩阵行个数 矩阵-1)))
;    (if (= k (矩阵列个数 矩阵-2))
;        (do矩阵 new-行 new-列 new-矩阵
;          (setf (aref new-矩阵 迭代-行 迭代-列)
;                (do ((i 0 (1+ i))
;                     (j 0))
;                    ((> i k) j)
;                  (setq j (+ j (* (if (>= i (矩阵列个数 矩阵-1))
;                                      0
;                                      (aref 矩阵-1 迭代-行 i))
;                                  (if (>= i (矩阵行个数 矩阵-2))
;                                      0
;                                      (aref 矩阵-2 i 迭代-列)))))
;                  )))
;        (error "矩阵1的行不等于矩阵2的列"))))

(defun 矩阵相乘2 (矩阵-1 矩阵-2)
  (let* ((new-行 (矩阵行个数 矩阵-1))
         (new-列 (矩阵列个数 矩阵-2))
         (new-矩阵 (make-array (list new-行 new-列)))
         (k (矩阵行个数 矩阵-1)))
    (if (= k (矩阵列个数 矩阵-2))
        (do矩阵 new-行 new-列 new-矩阵
          (setf (aref new-矩阵 迭代-行 迭代-列)
                (do ((i 0 (1+ i))
                     (j 0))
                    ((> i (1+ k)) j) ;;莫名其妙, 以后再看
                  (setq j (+ j (* (if (>= i (矩阵列个数 矩阵-1))
                                      0
                                      (aref 矩阵-1 迭代-行 i))
                                  (if (>= i (矩阵行个数 矩阵-2))
                                      0
                                      (aref 矩阵-2 i 迭代-列)))))
                  )))
        (error "矩阵1的行不等于矩阵2的列"))))

;;;2.3 单位矩阵和矩阵逆
