(deftemplate Node
(slot LeftTop(type NUMBER)) ;; 9 слотов для положения фишек
(slot MiddleTop(type NUMBER))
(slot RightTop(type NUMBER))
(slot LeftMiddle(type NUMBER))
(slot MiddleMiddle(type NUMBER))
(slot RightMiddle(type NUMBER))
(slot LeftBottom (type NUMBER))
(slot MiddleBottom(type NUMBER))
(slot RightBottom(type NUMBER))
(slot Depth(type NUMBER)) ;; глубина вершины в дереве
(slot Id(type NUMBER) (default 0)) ;; уникальный идентификатор вершины
(slot Status(type NUMBER) (default 0)) ;; статус вершины:

(slot From (type NUMBER)) ;; ссылка на родителя
(slot Exp (type NUMBER)) ;; значение целевой функции для данной вершины
);

(defglobal
?*Id* = 0 ;; объявляем и инициализируем глобальную переменную
?*H* = 2 ;;1 - h1, 2 - h2
?*STEP* = 0 ;;1 для пошагового режима
?*DEL* = 0 ;; количество удаленных повторных вершин
);

(deffunction Get_Id() ;; функция получения следующего Id
(bind ?*Id* (+ ?*Id* 1)) ;; инкрементируем Id
(return ?*Id*);
);

(deffunction W(
?LeftTop ?MiddleTop ?RightTop
?LeftMiddle ?MiddleMiddle ?RightMiddle 
?LeftBottom ?MiddleBottom ?RightBottom)
(bind ?a 0)
(if (not (= ?LeftTop 0)) then (bind ?a (+ 1 ?a))) ;; подсчет f(n)
(if (not (= ?MiddleTop 1)) then (bind ?a (+ 1 ?a))) ;; в части h(n)
(if (not (= ?RightTop 2)) then (bind ?a (+ 1 ?a))) ;; по всем (8-ми!)
(if (not (= ?LeftMiddle 3)) then (bind ?a (+ 1 ?a)))
(if (not (= ?MiddleMiddle 4)) then (bind ?a (+ 1 ?a)))
(if (not (= ?RightMiddle 5)) then (bind ?a (+ 1 ?a))) ;; ячейкам
(if (not (= ?LeftBottom 6)) then (bind ?a (+ 1 ?a)))
(if (not (= ?MiddleBottom 7)) then (bind ?a (+ 1 ?a)))
(if (not (= ?RightBottom 8)) then (bind ?a (+ 1 ?a)))

(return ?a); ;; возвращаемое значение
);

(deffunction M(
?LeftTop ?MiddleTop ?RightTop
?LeftMiddle ?MiddleMiddle ?RightMiddle 
?LeftBottom ?MiddleBottom ?RightBottom)
(bind ?a 0)

;;MiddleTop
(if (or (= ?LeftTop 1)(= ?MiddleMiddle 1)(= ?RightTop 1)) then (bind ?a (+ 1 ?a))
else (if (or(= ?LeftMiddle 1)(= ?MiddleBottom 1)(= ?RightMiddle 1)) then (bind ?a (+ 2 ?a)))
else (if (or(= ?RightBottom 1)(= ?LeftBottom 1)) then (bind ?a (+ 3 ?a))))
;;RightTop
(if (or (= ?MiddleTop 2)(= ?RightMiddle 2)) then (bind ?a (+ 1 ?a))
else (if (or(= ?MiddleMiddle 2)(= ?RightBottom 2)(= ?LeftTop 2)) then (bind ?a (+ 2 ?a)))
else (if (or(= ?LeftMiddle 2)(= ?MiddleBottom 2)) then (bind ?a (+ 3 ?a)))
else (if (= ?LeftBottom 2) then (bind ?a (+ 4 ?a))))
;;LeftMiddle
(if (or (= ?LeftTop 3)(= ?MiddleMiddle 3)(= ?LeftBottom 3)) then (bind ?a (+ 1 ?a))
else (if (or(= ?MiddleTop 3)(= ?RightMiddle 3)(= ?MiddleBottom 3)) then (bind ?a (+ 2 ?a)))
else (if (or(= ?RightTop 3)(= ?RightBottom 3)) then (bind ?a (+ 3 ?a))))
;;MiddleMiddle
(if (or (= ?LeftMiddle 4)(= ?MiddleTop 4)(= ?RightMiddle 4)(= ?MiddleBottom 4)) then (bind ?a (+ 1 ?a))
else (if (not(= ?MiddleMiddle 4)) then (bind ?a (+ 2 ?a))))
;;RightMiddle
(if (or (= ?RightTop 5)(= ?MiddleMiddle 5)(= ?RightBottom 5)) then (bind ?a (+ 1 ?a))
else (if (or(= ?MiddleTop 5)(= ?LeftMiddle 5)(= ?MiddleBottom 5)) then (bind ?a (+ 2 ?a)))
else (if (or(= ?LeftTop 5)(= ?LeftBottom 5)) then (bind ?a (+ 3 ?a))))
;;LeftBottom
(if (or (= ?MiddleBottom 6)(= ?LeftMiddle 6)) then (bind ?a (+ 1 ?a))
else (if (or(= ?MiddleMiddle 6)(= ?LeftTop 6)(= ?RightBottom 6)) then (bind ?a (+ 2 ?a)))
else (if (or(= ?RightMiddle 6)(= ?MiddleTop 6)) then (bind ?a (+ 3 ?a)))
else (if (= ?RightTop 6) then (bind ?a (+ 4 ?a))))
;;MiddleBottom
(if (or (= ?LeftBottom 7)(= ?MiddleMiddle 7)(= ?RightBottom 7)) then (bind ?a (+ 1 ?a))
else (if (or(= ?LeftMiddle 7)(= ?MiddleTop 7)(= ?RightMiddle 7)) then (bind ?a (+ 2 ?a)))
else (if (or(= ?RightTop 7)(= ?LeftTop 7)) then (bind ?a (+ 3 ?a))))
;;RightBottom
(if (or (= ?MiddleBottom 8)(= ?RightMiddle 8)) then (bind ?a (+ 1 ?a))
else (if (or(= ?MiddleMiddle 8)(= ?RightTop 8)(= ?LeftBottom 8)) then (bind ?a (+ 2 ?a)))
else (if (or(= ?LeftMiddle 8)(= ?MiddleTop 8)) then (bind ?a (+ 3 ?a)))
else (if (= ?LeftTop 8) then (bind ?a (+ 4 ?a))))
(return ?a);
);

(deffunction F(?Depth
?LeftTop ?MiddleTop ?RightTop
?LeftMiddle ?MiddleMiddle ?RightMiddle 
?LeftBottom ?MiddleBottom ?RightBottom)
(bind ?a ?Depth)
(if (= ?*H* 1) then
    (bind ?a (+ ?a (W ?LeftTop ?MiddleTop ?RightTop ?LeftMiddle ?MiddleMiddle ?RightMiddle ?LeftBottom ?MiddleBottom ?RightBottom)))
)
(if (= ?*H* 2) then
    (bind ?a (+ ?a (M ?LeftTop ?MiddleTop ?RightTop ?LeftMiddle ?MiddleMiddle ?RightMiddle ?LeftBottom ?MiddleBottom ?RightBottom)))
)
(return ?a);
);

(deffunction print(?Depth ?Exp
?LeftTop ?MiddleTop ?RightTop
?LeftMiddle ?MiddleMiddle ?RightMiddle 
?LeftBottom ?MiddleBottom ?RightBottom)

(printout t "F=" ?Exp ", H=" (- ?Exp ?Depth) crlf
    ?LeftTop " " ?MiddleTop " " ?RightTop crlf
	?LeftMiddle " " ?MiddleMiddle " " ?RightMiddle crlf
	?LeftBottom " " ?MiddleBottom " " ?RightBottom crlf
  )
)

(deffacts start ;; факты соответствующие исходному состоянию
(Node(LeftTop 3) (MiddleTop 6) (RightTop 4)
(LeftMiddle 2) (MiddleMiddle 5)(RightMiddle 8)
(LeftBottom 7) (MiddleBottom 1) (RightBottom 0)
(Depth 0)
(From 0)
(Exp (F 0 3 6 4 2 5 8 7 1 0))
(Id (Get_Id))
)
(min (F 0 3 6 4 2 5 8 7 1 0)) ;; фиксируется текущее значение min f(n)
(if (= ?*STEP* 1) then (printout t crlf "Start node: " crlf)(print 0 (F 0 3 6 4 2 5 8 7 1 0) 3 6 4 2 5 8 7 1 0))
);

(defrule move_circle
(declare (salience 1000)) ;; максимальный приоритет!!!
?f1<-(Node (LeftTop ?LT)(MiddleTop ?MT)(RightTop ?RT)
		   (LeftMiddle ?LM)(MiddleMiddle ?MM)(RightMiddle ?RM)
		   (LeftBottom ?LB)(MiddleBottom ?MB)(RightBottom ?RB)(Exp ?X)(Depth ?D1)) ;; первый факт
?f2<-(Node (LeftTop ?LT)(MiddleTop ?MT)(RightTop ?RT)
		   (LeftMiddle ?LM)(MiddleMiddle ?MM)(RightMiddle ?RM)
		   (LeftBottom ?LB)(MiddleBottom ?MB)(RightBottom ?RB)(Exp ?Y&~?X)(Status 0)(Depth ?D2));; второй факт
;;с неравным значением ЦФ
(test(< ?X ?Y)) ;;большим, чем у первого состояния
=>
(retract ?f2) ;; удаление вершины с большей ЦФ
(bind ?*DEL* (+ 1 ?*DEL*))
(if (= ?*STEP* 1) then (printout t crlf "Delete repeating node: " crlf)(print ?D2 ?Y ?LT ?MT ?RT ?LM ?MM ?RM ?LB ?MB ?RB))
)

(defrule goal_test ;;проверка целевого состояния
(declare (salience 500))
?f<-(Node (LeftTop 0) (MiddleTop 1) (RightTop 2);; состояние
(LeftMiddle 3) (MiddleMiddle 4) (RightMiddle 5) ;; целевое
(LeftBottom 6) (MiddleBottom 7) (RightBottom 8)
(Status ~2) ;; текущий статус вершины «не целевое»
(From ?Id)(Exp ?E))
=>
(modify ?f(Status 2));;текущая вершина помечается как «целевая»
(assert (Id (Get_Id)))
(printout t crlf "Solution: " crlf)
)

(defrule select_answer ;; изменение статуса промежуточных вершин
;; - бэктрекинг
(declare (salience 500))
(;; вершина со статусом 2, ссылается на
Node(Status 2) (From ?Id)
(LeftTop ?LT)(MiddleTop ?MT)(RightTop ?RT)
(LeftMiddle ?LM)(MiddleMiddle ?MM)(RightMiddle ?RM)
(LeftBottom ?LB)(MiddleBottom ?MB)(RightBottom ?RB)(Exp ?E)(Depth ?D)) ;; родителя
?f<-(Node(Id ?Id) (Status ~2))
=>
(modify ?f(Status 2))
(print ?D ?E ?LT ?MT ?RT ?LM ?MM ?RM ?LB ?MB ?RB)
)

(defrule delete_not_answer
(declare (salience 400)) ;; более низкий приоритет !!!
(Node(Status 2)) ;; если появилась вершина со статусом 2
?f<-(Node(Status ~2)) ;; все вершина со статусом НЕ 2, надо удалить
=>
(retract ?f)
)

(defrule Stop_1 ;; выполняется, если решение не существует
(declare (salience 200)) ;; приоритет низкий
(not (Node(Status 0|2))) ;; в базе фактов нет вершин со статусом 0 или 2,
=> ;; т.е. нераскрытых и с целевым состоянием
(halt)
(printout t "No solutions" crlf)
)

(defrule Stop_2 ;; выполняется, если решение найдено
(declare (salience 200))
(Node(Status 2)) ;; есть вершина с целевым состоянием
=>
(halt)
(printout t "Found solution" crlf)
)

(defrule find_min ;; определение текущего минимума ЦФ
(declare (salience 150)) ;; приоритет низкий!
?fmin<-(min ?min) ;; адрес факта, хранящего значение текущего
;; min целевой функции
(Node(Exp ?E&:(< ?E ?min))(Status 0)) ;;существование
;; вершины, у которой значение целевой функции
;; меньше текущего min
=>
(retract ?fmin) ;; удалить факт с текущим min
(assert (min ?E)) ;; создать факт с новым текущим min
(if (= ?*STEP* 1) then (printout t crlf "New min: " ?min crlf))
)

(defrule min_not_found ;; повышение минимума, если нет вершин с такой ЦФ
(declare (salience 120)) ;; приоритет низкий!
?fmin<-(min ?min) ;; адрес факта, хранящего значение текущего
;; min целевой функции
(not(exists(Node(Exp ?E&:(= ?E ?min))(Status 0)))) ;;существование
;; вершины, у которой значение целевой функции
;; равно текущему min
=>
(retract ?fmin) ;; удалить факт с текущим min
(assert (min (+ ?min 1))) ;; создать факт с новым min
(if (= ?*STEP* 1) then (printout t crlf "New min: " (+ ?min 1) crlf))
)

(defrule make_new_path_LeftTop 
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop 0) (MiddleTop ?MT) (RightTop ?RT) 
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E 0 ?MT ?RT ?LM ?MM ?RM ?LB ?MB ?RB))
(bind ?a (F (+ 1 ?L) ?MT 0 ?RT ?LM ?MM ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?MT) (MiddleTop 0) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a ?MT 0 ?RT ?LM ?MM ?RM ?LB ?MB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LM ?MT ?RT 0 ?MM ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LM) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle 0) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L)  ?a1 ?LM ?MT ?RT 0 ?MM ?RM ?LB ?MB ?RB))

(retract ?fmin)
(assert (min (min ?a ?a1)))
)

(defrule make_new_path_MiddleTop 
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop 0) (RightTop ?RT) 
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT 0 ?RT ?LM ?MM ?RM ?LB ?MB ?RB))

(bind ?a (F (+ 1 ?L) 0 ?LT ?RT ?LM ?MM ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop 0) (MiddleTop ?LT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a 0 ?LT ?RT ?LM ?MM ?RM ?LB ?MB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LT ?RT 0 ?LM ?MM ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?RT) (RightTop 0)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?RT 0 ?LM ?MM ?RM ?LB ?MB ?RB))

(bind ?a2 (F (+ 1 ?L) ?LT ?MM ?RT ?LM 0 ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MM) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle 0) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a2) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a2 ?LT ?MM ?RT ?LM 0 ?RM ?LB ?MB ?RB))

(retract ?fmin)
(assert (min (min ?a ?a1 ?a2)))
)

(defrule make_new_path_RightTop 
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop ?MT) (RightTop 0) 
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT ?MT 0 ?LM ?MM ?RM ?LB ?MB ?RB))

(bind ?a (F (+ 1 ?L) ?LT 0 ?MT ?LM ?MM ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop 0) (RightTop ?MT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a ?LT 0 ?MT ?LM ?MM ?RM ?LB ?MB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LT ?MT ?RM ?LM ?MM 0 ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RM)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle 0)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?MT ?RM ?LM ?MM 0 ?LB ?MB ?RB))

(retract ?fmin)
(assert (min (min ?a ?a1)))
)

(defrule make_new_path_LeftMiddle
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT) 
(LeftMiddle 0) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT ?MT ?RT 0 ?MM ?RM ?LB ?MB ?RB))

(bind ?a (F (+ 1 ?L) 0 ?MT ?RT ?LT ?MM ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop 0) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LT) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a 0 ?MT ?RT ?LT ?MM ?RM ?LB ?MB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LT ?MT ?RT ?MM 0 ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?MM) (MiddleMiddle 0) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?MT ?RT ?MM 0 ?RM ?LB ?MB ?RB))

(bind ?a2 (F (+ 1 ?L) ?LT ?MT ?RT ?LB ?MM ?RM 0 ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LB) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom 0) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a2) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a2 ?LT ?MT ?RT ?LB ?MM ?RM 0 ?MB ?RB))

(retract ?fmin)
(assert (min (min ?a ?a1 ?a2)))
)

(defrule make_new_path_MiddleMiddle
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT) 
(LeftMiddle ?LM) (MiddleMiddle 0) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT ?MT ?RT ?LM 0 ?RM ?LB ?MB ?RB))

(bind ?a (F (+ 1 ?L) ?LT 0 ?RT ?LM ?MT ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop 0) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MT) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a ?LT 0 ?RT ?LM ?MT ?RM ?LB ?MB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LT ?MT ?RT 0 ?LM ?RM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle 0) (MiddleMiddle ?LM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?MT ?RT 0 ?LM ?RM ?LB ?MB ?RB))

(bind ?a2 (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?RM 0 ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?RM) (RightMiddle 0)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a2) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a2 ?LT ?MT ?RT ?LM ?RM 0 ?LB ?MB ?RB))

(bind ?a3 (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?MB ?RM ?LB 0 ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MB) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom 0)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a3) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a3 ?LT ?MT ?RT ?LM ?MB ?RM ?LB 0 ?RB))

(retract ?fmin)
(assert (min (min ?a ?a1 ?a2 ?a3)))
)

(defrule make_new_path_RightMiddle
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT) 
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle 0)
(LeftBottom ?LB) (MiddleBottom ?MB) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT ?MT ?RT ?LM ?MM 0 ?LB ?MB ?RB))

(bind ?a (F (+ 1 ?L) ?LT ?MT 0 ?LM ?MM ?RT ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop 0)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RT)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a ?LT ?MT 0 ?LM ?MM ?RT ?LB ?MB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LT ?MT ?RT ?LM 0 ?MM ?LB ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle 0) (RightMiddle ?MM)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?MT ?RT ?LM 0 ?MM ?LB ?MB ?RB))

(bind ?a2 (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?MM ?RB ?LB ?MB 0))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RB)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom 0)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a2) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a2 ?LT ?MT ?RT ?LM ?MM ?RB ?LB ?MB 0))

(retract ?fmin)
(assert (min (min ?a ?a1 ?a2)))
)

(defrule make_new_path_LeftBottom
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT) 
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom 0) (MiddleBottom ?MB) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT ?MT ?RT ?LM ?MM ?RM 0 ?MB ?RB))

(bind ?a (F (+ 1 ?L) ?LT ?MT ?RT 0 ?MM ?RM ?LM ?MB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle 0) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LM) (MiddleBottom ?MB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a ?LT ?MT ?RT 0 ?MM ?RM ?LM ?MB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?MM ?RM ?MB 0 ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?MB) (MiddleBottom 0)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?MT ?RT ?LM ?MM ?RM ?MB 0 ?RB))

(retract ?fmin)
(assert (min (min ?a ?a1)))
)

(defrule make_new_path_MiddleBottom
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT) 
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom 0) (RightBottom ?RB)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT ?MT ?RT ?LM ?MM ?RM ?LB 0 ?RB))

(bind ?a (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?MM ?RM 0 ?LB ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom 0) (MiddleBottom ?LB)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a ?LT ?MT ?RT ?LM ?MM ?RM 0 ?LB ?RB))

(bind ?a1 (F (+ 1 ?L) ?LT ?MT ?RT ?LM 0 ?RM ?LB ?MM ?RB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle 0) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MM)(RightBottom ?RB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?MT ?RT ?LM 0 ?RM ?LB ?MM ?RB))

(bind ?a2 (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?MM ?RM ?LB ?RB 0))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?RB)(RightBottom 0)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a2) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a2 ?LT ?MT ?RT ?LM ?MM ?RM ?LB ?RB 0))

(retract ?fmin)
(assert (min (min ?a ?a1 ?a2)))
)

(defrule make_new_path_RightBottom
(declare (salience 100)) ;; приоритет самый низкий!!
?fmin<-(min ?min) ;; получение ссылки на факт с текущим минимумом
?f<-(Node(Status 0) (Depth ?L) (Id ?Id) ;; определение состояния с пустым полем
(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT) 
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom ?MB) (RightBottom 0)
(Exp ?E& :(= ?E ?min))) ;; проверка ЦФ вершины на min
=>
;;(printout t ?min " " (fact-slot-value ?f Exp) crlf)
(modify ?f(Status 1)) ;; изменение статуса вершины на «раскрыта»
(if (= ?*STEP* 1) then (printout t crlf "Current node: " crlf)(print ?L ?E ?LT ?MT ?RT ?LM ?MM ?RM ?LB ?MB 0))

(bind ?a (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?MM 0 ?LB ?MB ?RM))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle 0)
(LeftBottom ?LB) (MiddleBottom ?MB)(RightBottom ?RM)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a ?LT ?MT ?RT ?LM ?MM 0 ?LB ?MB ?RM))

(bind ?a1 (F (+ 1 ?L) ?LT ?MT ?RT ?LM ?MM ?RM ?LB 0 ?MB))

(assert (Node(LeftTop ?LT) (MiddleTop ?MT) (RightTop ?RT)
(LeftMiddle ?LM) (MiddleMiddle ?MM) (RightMiddle ?RM)
(LeftBottom ?LB) (MiddleBottom 0)(RightBottom ?MB)
(Depth(+ ?L 1)) (From ?Id) (Exp ?a1) (Id (Get_Id))))
(if (= ?*STEP* 1) then (printout t crlf "New node: " crlf)(print (+ 1 ?L) ?a1 ?LT ?MT ?RT ?LM ?MM ?RM ?LB 0 ?MB))

(retract ?fmin)
(assert (min (min ?a ?a1)))
)