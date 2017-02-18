#lang racket

(require racket/include)
(include "binary-tree-test.rkt")
;; TASK pregatitor
(define empty-tree
  '())


;aici pentru initierea unui nou nod care are doar o valoare
(define init-node 
  (λ (value) (list value)) 
  )
; structura nodului ales de mine este astfel (valoare fiu-stang fiu-drept)
(define make-node
  (λ (left right value) (list value left right))
  )

(define is-leaf?
  (λ (node) (if (null? node)
                #f
                (if (number? node)
                    #t
                (if (null? (cdr node))
                    #t
                    #f)))))
                

(define get-value
  (λ (node) (if (null? node)
                null
                (if (number? node)
                    node
                (car node))))
  )

(define get-left
  (λ (node) (if (null? node)
                null
                (if (is-leaf? node)
                    null
                    (if (= 2 (length node))
                        (if (null? (car (cdr node)))
                            null
                            (if (> (get-value node) (car (car (cdr node))))
                                (car (cdr node))
                                null))
                        (car (cdr node))))))
  )

(define get-right
  (λ (node) (if (null? node)
                null
                (if (is-leaf? node)
                    null
                    (if (= 2 (length node))
                        (if (null? (car (cdr node)))
                            null
                        (if (< (get-value node) (get-value (car (cdr node))))
                            (car (cdr node))
                            null))
                        (car (reverse node))))))
  )

(define is-node?
  (λ (node) (if (null? node)
                #f
                (if (is-leaf? node)
                    #t
                    (if (= 2 (length node))
                        #t
                (if (= 3 (length node))
                    #t
                    #f)))))
  )

(define is-empty?
  (λ (tree) (if (null? tree)
                #t
                #f))
  )

(define has-left?
  (λ (tree) (if (null? (get-left tree))
                #f
                #t))
  )

(define has-right?
  (λ (tree) (if (null? (get-left tree))
                #f
                #t))
  )
; aceasta functie ma ajuta la gasirea minimului , de asemenea aceasta functie ma ajuta si la
;gasirea maximului deoarece i-am pus un argument care e lasat liber la alegere (get-left sau get-right)
(define (minmax c tree)
  (if (is-empty? tree)
      tree
      (if (is-leaf? tree)
          (car tree)
          (minmax c (c tree))))) 

(define minimum
  (λ (tree) (minmax get-left tree))
  )

(define maximum
  (λ (tree) (minmax get-right tree))
  )

(define (inaltime tree)
  (if (null? tree)
      0
  (if (is-leaf? tree)
      1
      (if (= 2 (length tree))
          (+ 1 (inaltime (car (cdr tree))))
          (+ 1 (max (inaltime (get-left tree)) (inaltime (get-right tree))))))))


(define height
  (λ (tree) (inaltime tree))
  )
; aici am definit inordine si l-am apelat in functia inorder , am facut cu ajutorul functiei
; append fiind recursiv 
(define (inordine tree)
  (if (null? tree)
      null
      (append (inordine (get-left tree)) (list (get-value tree)) (inordine (get-right tree)))))

(define (preordine tree)
  (if (null? tree)
      null
      (append (list (get-value tree)) (inordine (get-left tree)) (inordine (get-right tree)))))

(define inorder
  (λ (tree) (inordine tree)))


(define preorder
  (λ (tree) #f)
  )

(define postorder
  (λ (tree) #f)
  )

(define (minimul tree f)
  (if (null? (f tree))
      (get-value tree)
      (minimul (f tree) f)))
; pentru successor m-am gandit astfel , am luat primu caz dreapta maxim stanga , iar apoi
; celalalt caz era atunci cand nu aveam fiu drept ,astfel trebuia sa parcurg de la radacina
; arborelui pana la aceea valoare si am luat si un acumulator pentru a retine valoarea
; succesorului , iar aceasta se schimba pe parcurs daca gaseam o valoare mai apropiata de valoarea
; cautata.
(define (succ tree val acc)
  (if (= (get-value tree) val)
      acc
      (if (< (- acc val) 0)
          (succ (get-right tree) val (get-value (get-right tree)))
      (if (< (- (get-value tree) val) 0)
          (if (> (get-value tree) val)
              (succ (get-left tree) val acc)
              (succ (get-right tree) val acc))
          (if (< (- (get-value tree) val) (- acc val))
              (if (> (get-value tree) val)
                  (succ (get-left tree) val (get-value tree))
                  (succ (get-right tree) val (get-value tree)))
              (if (> (get-value tree) val)
                  (succ (get-left tree) val acc)
                  (succ (get-right tree) val acc)))))))

(define (succesorul tree val radacina)
 (if (null? tree)
     null
     (if (> (get-value tree) val)
         (succesorul (get-left tree) val radacina)
         (if (< (get-value tree) val)
             (succesorul (get-right tree) val radacina)
             (if (null? (get-right tree))
                 (succ radacina val (get-value radacina))
                 (minimul (get-right tree) get-left))))))


(define successor
  (λ (tree value) (succesorul tree value tree))
  )
; pentru predecesor cazul al 2-lea este la fel ca la succesor iar , primul caz este
; stanga maxim dreapta.
(define (prec tree val acc)
  (if (= (get-value tree) val)
      acc
      (if (< (- (get-value tree) val) (- acc val))
          (if (> (get-value tree) val)
              (prec (get-left tree) val (get-value tree))
              (prec (get-right tree) val (get-value tree)))
          (if (> (get-value tree) val)
              (prec (get-left tree) val acc)
              (prec (get-right tree) val acc)))))

(define (predecesorul tree val radacina)
 (if (null? radacina)
     null
     (if (null? tree)
         null
         (if (= (car (inorder radacina)) val)
             null
     
     (if (> (get-value tree) val)
         (predecesorul (get-left tree) val radacina)
         (if (< (get-value tree) val)
             (predecesorul (get-right tree) val radacina)
             (if (null? (get-left tree))
                 (prec radacina val (get-value radacina))
                 (minimul (get-left tree) get-right))))))))

(define predecessor
  (λ (tree value) (predecesorul tree value tree))
  )

(define binary-search-tree '(9 (3 (2 (1)) (6 (5) (8 (7)))) (12 (11) (15 (13) (21)))) )

;;Task 1

; Pentru inserare cautam pozitia unde trebuia sa fie introdus noul nod, iar pana atunci in parcurgerea
; arborelui nu schimbam nimic, urmand sa schimb doar sub-arborele unde trebuia sa fie introdus noul nod
; deoarece trebuia balansat acel arbore.
(define (insereaza tree val)
  (if (null? tree)
      (init-node val)
      (if (is-leaf? tree)
          (if (= (get-value tree) val)
              tree
          (balance (list (get-value tree) (list val))))
          (if (< val (get-value tree))
              (if (null? (get-left tree))
                  (balance (make-node (init-node val) (get-right tree) (get-value tree)))
                  (if (null? (get-right tree))
                      (balance (list (get-value tree) (insereaza (get-left tree) val)))
                      (balance (make-node (insereaza (get-left tree) val) (get-right tree) (get-value tree)))))
              (if (> val (get-value tree))
                  (if (null? (get-right tree))
                      (balance (make-node (get-left tree) (init-node val) (get-value tree)))
                      (if (null? (get-left tree))
                          (balance (list (get-value tree) (insereaza (get-right tree) val)))
                          (balance (make-node (get-left tree) (insereaza (get-right tree) val) (get-value tree)))))
                  (balance tree))))))
              
          

(define insert
  (λ (tree value) (insereaza tree value))
  )
; Aici urmeaza cele 4 cazuri pentru balansarea arborelui , aceste functii sunt apelate in balance
; in functie de lungimile celor 2 copii ale nodului radacina
(define (leftleft tree)
  (if (null? (get-right tree))
      (if (null? (get-right (get-left tree)))
      (make-node (get-left (get-left tree)) (init-node (get-value tree)) (get-value (get-left tree)))
      (make-node (list (get-value (get-left tree)) (get-left (get-left tree))) (init-node (get-value tree)) (get-value (get-right (get-left tree)))))
  (if (null? (get-right (get-left tree)))
      (make-node (get-left (get-left tree)) (list (get-value tree) (get-right tree)) (get-value (get-left tree)))
  (make-node (get-left (get-left tree)) (make-node (get-right (get-left tree)) (get-right tree) (get-value tree)) (get-value (get-left tree))))))

(define (leftright tree)
  (if (null? (get-left (get-left tree)))
      (if (null? (get-right tree))
          (make-node  (init-node (get-value (get-left tree))) (init-node  (get-value tree))  (get-value (get-right (get-left tree))))
      (if (null? (get-left (get-right (get-left tree))))
          (make-node (list (get-value (get-left tree))) (make-node (get-right (get-right (get-left tree))) (get-right tree) (get-value tree))
                     (get-value (get-right (get-left tree))))
          (make-node (list (get-value (get-left tree)) (get-left (get-right (get-left tree))))
                     (list (get-value tree) (get-right tree)) (get-value (get-right (get-left tree))))))
      
      (if (null? (get-left (get-right (get-left tree))))
          (make-node (list (get-value (get-left tree)) (get-left (get-left tree))) (make-node (get-right (get-right (get-left tree))) (get-right tree) (get-value tree))
                     (get-value (get-right (get-left tree))))
          (make-node (make-node (get-left (get-left tree)) (get-left (get-right (get-left tree))) (get-value (get-left tree)))
                     (list (get-value tree) (get-right tree)) (get-value (get-right (get-left tree)))))))

(define (rightright tree)
  (if (null? (get-left tree))
      (if (null? (get-left (get-right tree)))
      (make-node (init-node (get-value tree)) (get-right (get-right tree)) (get-value (get-right tree)))
      (make-node (init-node (get-value tree)) (list (get-value (get-right tree)) (get-right (get-right tree))) (get-value (get-left (get-right tree)))))
  (if (null? (get-left (get-right tree)))
      (make-node (list (get-value tree) (get-left tree)) (get-right (get-right tree)) (get-value (get-right tree)))
      (make-node (make-node (get-left tree) (get-left (get-right tree)) (get-value tree))
                 (get-right (get-right tree)) (get-value (get-right tree))))))
  
(define (rightleft tree)
  (if (null? (get-right (get-right tree)))
      (if (null? (get-left tree))
          (make-node (init-node (get-value tree)) (init-node (get-value (get-right tree))) (get-value (get-left (get-right tree))))
      (if (null? (get-left (get-left (get-right tree))))
          (make-node (list (get-value tree) (get-left tree)) (list (get-value (get-right tree)) (get-right (get-left (get-right tree))))
                     (get-value (get-left (get-right tree))))
          (make-node (make-node (get-left tree) (get-left (get-left (get-right tree))) (get-value tree))
                     (init-node (get-value (get-right tree))) (get-value (get-left (get-right tree))))))
      (if (null? (get-left (get-left (get-right tree))))
          (make-node (list (get-value tree) (get-left tree)) (make-node (get-right (get-left (get-right tree))) (get-right (get-right tree)) (get-value (get-right tree)))  
                     (get-value (get-left (get-right tree))))
          (make-node (make-node (get-left tree) (get-left (get-left (get-right tree))) (get-value tree))
                     (make-node  (get-right (get-left (get-right tree))) (get-right (get-right tree)) (get-value (get-right tree)) ) (get-value (get-left (get-right tree)))))))


; aici sunt apelate cele 4 cazuri ale balansului in functie de lungimile celor 2 copii.
(define balance
  (λ (tree)
    (if (null? tree)
        tree
    (if (> (- (height (get-left tree)) (height (get-right tree))) 1)
        (if (>= (height (get-left (get-left tree))) (height (get-right (get-left tree))))
            (leftleft tree)
            (leftright tree))
        (if (< (- (height (get-left tree)) (height (get-right tree))) -1)
            (if (>= (height (get-right (get-right tree))) (height (get-left (get-right tree))))
                (rightright tree)
                (rightleft tree))
            tree))))
  )


; Functia uniune ce va fi apelata in union , aceasta functie introduce in primul arbore
; toate elementele arborelui 2, rezultatul (tree1) fiind uniunea celor 2 arbori.

(define (uniune tree1 tree2)
  (if (null? tree2)
      tree1
      (uniune (insert tree1 (get-value tree2)) (remove tree2 (get-value tree2)))))

(define union
  (λ (tree1 tree2)
    (uniune tree2 tree1)
            )
  )


(define (intersectie tree1 tree2 acc)
  (if (null? tree1)
      acc
      (if (contains tree2 (get-value tree1))
          (intersectie (remove tree1 (get-value tree1)) tree2 (insert acc (get-value tree1)))
          (intersectie (remove tree1 (get-value tree1)) tree2  acc))))

(define intersection
  (λ (tree1 tree2)
    (intersectie tree1 tree2 '()))
  )

; Aceasta functie am creat-o pentru a putea apela in complements , acumulator fiind
; arborele vid apoi parcurgand tree2 introduc doar acele valori care nu se regasesc
; in celalalt arbore.
(define (scadere tree1 tree2 acc)
  (if (null? tree2)
      acc
      (if (contains tree1 (get-value tree2))
          (scadere tree1 (remove tree2 (get-value tree2)) acc)
          (scadere tree1 (remove tree2 (get-value tree2)) (insert acc (get-value tree2))))))

(define complements
  (λ (tree1 tree2) (scadere tree2 tree1 '()))
  )

(define contains
  (λ (tree value)
    (if (null? tree)
        #f
        (if (is-leaf? tree)
            (if (= (car tree) value)
                #t
                #f)
            (if (= 2 (length tree))
                (if (= (car tree) value)
                    #t
                    (contains (car (cdr tree)) value))
                (if (= (get-value tree) value)
                    #t
                    (if (> value (get-value tree))
                        (contains (get-right tree) value)
                        (contains (get-left tree) value)))))))
  )
; Aceasta functie e pentru a ma ajuta sa returnez cel mai stanga copil din dreapta unei radacini
; anume pentru a putea face interschimbarea din remove.
(define (urmatorul tree)
  (if (null? (get-left tree))
      tree
      (urmatorul (get-left tree))))


; Aceasta functie respecta stergerea dintr-un arbore AVL , aici am folosit si functia balance
; pentru a putea rebalansa arborele dupa ce se face o stergere.
(define remove
  (lambda (tree value)
    (if (null? tree)
        null
   (if (is-leaf? tree)
       (if (= (car tree) value)
           null
           tree)
       (if (= 2 (length tree))
           (if (= (car tree) value)
               (balance (car (cdr tree)))
               (if (= (get-value (car (cdr tree))) value)
                   (if (is-leaf? (car (cdr tree)))
                       (init-node (get-value tree))
                       (list (get-value tree) (car (cdr (car (cdr tree))))))
               (balance (list (car tree) (remove (car (cdr tree)) value)))))
           (if (= (car tree) value)
               (if (is-leaf? (get-right tree))
                   (balance (list (car (get-right tree)) (get-left tree)))
                   (balance (make-node (get-left tree) (remove (get-right tree) (car (urmatorul (get-right tree))))
                                       (car (urmatorul (get-right tree))))))
               (if (= value (car (get-left tree)))
                   (if (is-leaf? (get-left tree))
                       (balance (list (car tree) (get-right tree)))
                      (balance (make-node (remove (get-left tree) value) (get-right tree) (get-value tree))))
                   (if (= value (car (get-right tree)))
                       (if (is-leaf? (get-right tree))
                           (balance (list (car tree) (get-left tree)))
                           (balance (make-node (get-left tree) (remove (get-right tree) value) (get-value tree))))
               (if (> value (get-value tree))
                   (if (null? (get-right tree))
                       (balance (list (car tree) (get-left tree)))
                   (balance (make-node (get-left tree) (remove (get-right tree) value) (get-value tree))))
                   
                       (if (null? (get-left tree))
                           (balance (list (car tree) (get-right tree)))
                           (balance (make-node (remove (get-left tree) value) (get-right tree) (get-value tree))))))))))))
           
       
                            
            
  )

;;Task 2
; Aici in aceasta functie generez toate submultimile din Lista
(define (submultimi Lista)
  (if (null? Lista)
      (list '())
      (append (map (λ (el) (cons (car Lista) el))
                   (submultimi (cdr Lista)))
              (submultimi (cdr Lista)))))
; In aceata functie pastrez acele submultimi de k lungime .              
(define (kSubmultimi Lista k)
  (if (< k 15)
  (filter (λ (el) (= (length el) k)) (submultimi Lista))
  '()))

(define k-subsets
  (λ (set k) (kSubmultimi (inorder set) k))
  )


(define (permutari lista acc)
  (if (= (length acc) (length lista))
      (list acc)
      (apply append (map (λ (x)
                         (if (member x acc) 
                             '()
                             (permutari lista (cons x acc)) 
                             )) lista))))
(define (permZigZag lista)
  (permutari lista '()))


(define (caz1 lista)
  (if (= 1 (length lista))
       #t
      (if (= 2 (length lista))
          (if (> (car (cdr lista)) (car lista))
              #f
               #t )
          (if (= 3 (length lista))
              (if (> (car lista) (car (cdr lista)))
                  (if (< (car (cdr lista)) (car (reverse lista)))
                       #t
                      #f)
                  #f)
              (if (> (car lista) (car (cdr lista)))
                  (if (< (car (cdr lista)) (car (cdr (cdr lista))))
                      (caz1 (cdr (cdr lista)))
                      #f)
                  #f)))))


(define (caz2 lista)
  (if (= 1 (length lista))
      #t
      (if (= 2 (length lista))
          (if (> (car lista) (car (cdr lista)))
              #f
              #t)
          (if (= 3 (length lista))
              (if (< (car lista) (car (cdr lista)))
                  (if (> (car (cdr lista)) (car (reverse lista)))
                       #t
                      #f)
                  #f)
              (if (< (car lista) (car (cdr lista)))
                  (if (> (car (cdr lista)) (car (cdr (cdr lista))))
                      (caz2 (cdr (cdr lista)))
                      #f)
                  #f)))))
              

(define (filtruZig lista)
  (if (> (car lista) (car (cdr lista)))
      (caz1 lista)
      (caz2 lista)))


(define zig-zag-subsets
  (λ (set) (filter filtruZig (permZigZag (inorder set))))
  )

;;BONUS
; Aici construiesc arborele sintactic dupa expresia matematica data
; arborele are structura ca cea din poza enuntului temei adica
; radacina : simbol(*/+-) si copii fiind ori simbol ori operatori(numere)
(define (parser1 ex)
  (if (list? (car ex))
      (if (list? (car (reverse ex)))
          (if (equal? '+ (car (cdr ex)))
              (make-node (parser1 (car ex)) (parser1 (car (reverse ex))) '+)
              (if (equal? '- (car (cdr ex)))
                  (make-node (parser1 (car ex)) (parser1 (car (reverse ex))) '-)
                   (if (equal? '* (car (cdr ex)))
                       (make-node (parser1 (car ex)) (parser1 (car (reverse ex))) '*)
                       (if (equal? '/ (car (cdr ex)))
                           (make-node (parser1 (car ex)) (parser1 (car (reverse ex))) '/)
                           null))))
          (if (equal? '+ (car (cdr ex)))
              (make-node (parser1 (car ex)) ( car (reverse ex)) '+)
              (if (equal? '- (car (cdr ex)))
                  (make-node (parser1 (car ex)) (car (reverse ex)) '-)
                   (if (equal? '* (car (cdr ex)))
                       (make-node (parser1 (car ex)) (car (reverse ex)) '*)
                       (if (equal? '/ (car (cdr ex)))
                           (make-node (parser1 (car ex)) (car (reverse ex)) '/)
                           null)))))
      (if (list? (car (reverse ex)))
          (if (equal? '+ (car (cdr ex)))
              (make-node  (car ex) (parser1 (car (reverse ex))) '+)
              (if (equal? '- (car (cdr ex)))
                  (make-node  (car ex) (parser1 (car (reverse ex))) '-)
                   (if (equal? '* (car (cdr ex)))
                       (make-node  (car ex) (parser1 (car (reverse ex))) '*)
                       (if (equal? '/ (car (cdr ex)))
                           (make-node  (car ex) (parser1 (car (reverse ex))) '/)
                           null))))
          (if (equal? '+ (car (cdr ex)))
              (make-node  (car ex)  (car (reverse ex)) '+)
              (if (equal? '- (car (cdr ex)))
                  (make-node (car ex)  (car (reverse ex)) '-)
                   (if (equal? '* (car (cdr ex)))
                       (make-node  (car ex)  (car (reverse ex)) '*)
                       (if (equal? '/ (car (cdr ex)))
                           (make-node (car ex)  (car (reverse ex)) '/)
                           null)))))))
  
(define parser
  (λ (expression) (parser1 expression)
    )
  )
; Aici in aceasta functie evaluez arborele sintactic urmand a
; intoarce rezultatul potrivit.
(define (eval ex )
  (if (number? (get-left ex))
      (if (number? (get-right ex))
          (if (equal? '+ (get-value ex))
              (+ (get-left ex) (get-right ex))
              (if (equal? '- (get-value ex))
                  (- (get-left ex) (get-right ex))
                   (if (equal? '* (get-value ex))
                       (* (get-left ex) (get-right ex))
                       (if (equal? '/ (get-value ex))
                           (/ (get-left ex) (get-right ex))
                           0))))
          (if (equal? '+ (get-value ex))
              (+ (get-left ex) (eval (get-right ex)))
              (if (equal? '- (get-value ex))
                  (- (get-left ex) (eval (get-right ex)))
                   (if (equal? '* (get-value ex))
                       (* (get-left ex) (eval (get-right ex)))
                       (if (equal? '/ (get-value ex))
                           (/ (get-left ex) (eval (get-right ex)))
                           0)))))
      (if (number? (get-right ex))
      (if (equal? '+ (get-value ex))
              (+ (eval (get-left ex)) (get-right ex))
              (if (equal? '- (get-value ex))
                  (- (eval (get-left ex)) (get-right ex))
                   (if (equal? '* (get-value ex))
                       (* (eval (get-left ex))  (get-right ex))
                       (if (equal? '/ (get-value ex))
                           (/ (eval (get-left ex)) (get-right ex))
                           0))))
      (if (equal? '+ (get-value ex))
              (+ (eval (get-left ex)) (eval (get-right ex)))
              (if (equal? '- (get-value ex))
                  (- (eval (get-left ex)) (eval (get-right ex)))
                   (if (equal? '* (get-value ex))
                       (* (eval (get-left ex))  (eval (get-right ex)))
                       (if (equal? '/ (get-value ex))
                           (/ (eval (get-left ex)) (eval (get-right ex)))
                           0)))))))
      
  

(define evaluate
  (λ (expr-tree) (eval expr-tree)
  ))

;; SECȚIUNE DE TESTARE - NU modificați această linie!
;; ATENȚIE! Pentru a primi punctaj pe temă, NU modificați această secțiune!
;;
;; CHECK - TASK 0 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 0 : 30 puncte) ;;check-exp
(define functions (list is-node? is-leaf? is-empty? get-value make-node get-right get-left inorder height insert empty-tree)) ;;check-exp
(define tree0 binary-search-tree) ;;check-exp
(check-exp-part 'is-node .037 (is-node? tree0) #t)
(check-exp-part 'is-leaf?1 .037 (is-leaf? tree0) #f)
(check-exp-part 'is-leaf?2 .037 (is-leaf? (init-node 8)) #t)
(check-exp-part 'is-empty?1 .037 (is-empty? tree0) #f)
(check-exp-part 'is-empty?2 .037 (is-empty? empty-tree) #t)
(check-exp-part 'get-value1 .037 (get-value tree0) 9)
(check-exp-part 'get-value2 .037 (get-value (get-left tree0)) 3)
(check-exp-part 'get-value3 .037 (get-value (get-right tree0)) 12)
(check-exp-part 'make-node .037 (make-node (get-left tree0) (get-right tree0) (get-value tree0)) binary-search-tree)
(check-exp-part 'minimum .0833 (minimum tree0) 1)
(check-exp-part 'maximum .0833 (maximum tree0) 21)
(check-exp-part 'height1 .0833 (height tree0) 5)
(check-exp-part 'height2 .0833 (height (get-left (get-left tree0))) 2)
(check-exp-part 'successor1 .055 (successor tree0 9) 11)
(check-exp-part 'successor2 .055 (successor tree0 5) 6)
(check-exp-part 'successor3 .055 (successor tree0 8) 9)
(check-exp-part 'predecessor1 .056 (predecessor tree0 9) 8)
(check-exp-part 'predecessor2 .056 (predecessor tree0 5) 3)
(check-exp-part 'predecessor3 .057 (predecessor tree0 12) 11)
;; SFÂRȘIT CHECK - TASK 0 - NU modificați această linie!
;;
;; CHECK - Task1 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 1 : 50 puncte) ;;check-exp
(define A (create-tree '(8 9 10 15 8 5 0 1 4 5 9 7 1 0 151 651 61 45 416 2542 -8 3541 644 2 4 8542 51 142 215) functions)) ;;check-exp
(define B (create-tree '(942 4 54 64 94 25 0 -815 485 251 64 8 10 5 4 644 2 216 2541 5 8 7 5254 2542 214 4511) functions)) ;;check-exp
(define C (create-tree '(8 5 4 1 846 54 0 -5552 4 5 810 42 545 842 54 5488 8755 14 679 25 78 25 955 7891 789 8891 97 54 15 2465 155) functions)) ;;check-exp
(define D (create-tree '(8 9 1 5 9 7 5 9 78 1 5 6 9 89 24 52 95 22 94 6 485 18 6 97 8 100 4 9 655 478 92) functions)) ;;check-exp
(check-exp-part 'check-set1 .04 (test-task1 (create-tree '(8 4 2 1 -5 6 1 8 9 5 3 11 17 10 -6 4 8) functions) functions) result-check-set1)
(check-exp-part 'check-set2 .04 (test-task1 (create-tree '(-9 8 2 1 4 0 9 3 4 2 5 9 11 481 51 35 15 0 4 15 251 6551 12 3 4 7 9) functions) functions) result-check-set2)
(check-exp-part 'check-set3 .04 (test-task1 A functions) result-check-set3)
(check-exp-part 'check-set4 .04 (test-task1 B functions) result-check-set4)
(check-exp-part 'check-set5 .04 (test-task1 C functions) result-check-set5)
(check-exp-part 'union1 .005 (test-task1 (union A B) functions) result-union1)
(check-exp-part 'union2 .005 (test-task1 (union C D) functions) result-union2)
(check-exp-part 'union3 .005 (test-task1 (union A D) functions) result-union3)
(check-exp-part 'union4 .005 (test-task1 (union (union A B) (union C D)) functions) result-union4)
(check-exp-part 'intersection1 .01 (test-task1 (intersection A B) functions) result-intersection1)
(check-exp-part 'intersection2 .01 (test-task1 (intersection B C) functions) result-intersection2)
(check-exp-part 'intersection3 .01 (test-task1 (intersection C D) functions) result-intersection3)
(check-exp-part 'intersection4 .01 (test-task1 (intersection (intersection A B) (intersection  C D)) functions) result-intersection4)
(check-exp-part 'complements1 .01 (test-task1 (complements A B) functions) result-complements1)
(check-exp-part 'complements2 .01 (test-task1 (complements C D) functions) result-complements2)
(check-exp-part 'complements3 .01 (test-task1 (complements C D) functions) result-complements3)
(check-exp-part 'complements4 .01 (test-task1 (complements (complements A B) (complements C D)) functions) result-complements4)
(check-exp-part 'insert1 .005 (test-task1 (insert B -7) functions) result-insert1)
(check-exp-part 'insert2 .005 (test-task1 (insert A 59525) functions) result-insert2)
(check-exp-part 'insert3 .005 (test-task1 (insert C 988522) functions) result-insert3)
(check-exp-part 'insert4 .005 (test-task1 (insert D -812612) functions) result-insert4)
(check-exp-part 'remove1 .02 (test-task1 (remove binary-search-tree (minimum binary-search-tree)) functions) result-remove1)
(check-exp-part 'remove2 .02 (test-task1 (remove binary-search-tree 9) functions) result-remove2)
(check-exp-part 'remove3 .02 (test-task1 (remove binary-search-tree 3) functions) result-remove3)
(check-exp-part 'remove4 .02 (test-task1 (remove (remove (remove A (successor A 10)) (predecessor A 0)) 416) functions) result-remove4)
(check-exp-part 'complex1 .02 (test-task1 (union A (intersection B C)) functions) result-complex1)
(check-exp-part 'complex2 .02 (test-task1 (insert (intersection (complements B C) (remove (union A B) (predecessor A 51))) 7851) functions) result-complex2)
(check-exp-part 'complex3 .02 (test-task1 (insert (remove (remove (union (intersection (complements B C) (complements B A)) binary-search-tree) 214) 1) 1) functions) result-complex3)
(check-exp-part 'complex4 .02 (test-task1 (union (intersection (complements B A) (union C D)) (complements A D)) functions) result-complex4)
(check-exp-part 'complex5 .02 (test-task1 (intersection (union (complements A B) (complements C D)) (complements (intersection A B) (intersection C D))) functions) result-complex5)
(check-exp-part 'complex6 .02 (test-task1 (remove (insert (union (union (complements A B) (intersection C D)) (intersection (complements C D) (intersection A B))) 22) -8) functions) result-complex6)
(check-exp-part 'complex7 .02 (test-task1 (union (union (intersection A C) (complements A D)) (intersection (complements B C) (intersection B D))) functions) result-complex7)
(check-exp-part 'complex8 .02 (test-task1 (union (union (union A B) (union C D)) (intersection (intersection A B) (intersection C D))) functions) result-complex8)
(check-exp-part 'complex9 .02 (test-task1 (intersection (union (complements A B) (complements B A)) (intersection (union A B) (union C D))) functions) result-complex9)
(check-exp-part 'complex10 .02 (test-task1 (insert (remove (intersection (union (complements B A) (union (complements C D) (intersection A B))) (intersection (complements B (union A C)) (union C D))) 485) 100) functions) result-complex10)
(check-exp-part 'height-balanced1 .04 (check-self-balancing-tree B functions) #t)
(check-exp-part 'height-balanced2 .04 (check-self-balancing-tree A functions) #t)
(check-exp-part 'height-balanced3 .04 (check-self-balancing-tree C functions) #t)
(check-exp-part 'height-balanced4 .04 (check-self-balancing-tree D functions) #t)
(check-exp-part 'height-balanced5 .04 (let [(tree (create-tree '(1 2 3 4 5 6 7 8 9 10) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced6 .04 (let [(tree (create-tree '(20 19 18 17 16 15 10 9 8 7 6 5 4 3 2 1) functions))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced7 .04 (let [(tree (union A (intersection B C)))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced8 .04 (let [(tree (remove (insert (union (complements A D) (intersection B C)) 24) 416))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced9 .04 (let [(tree (union (remove binary-search-tree 9) A))] (check-self-balancing-tree tree functions)) #t)
(check-exp-part 'height-balanced10 .04 (let [(tree (intersection (union (remove A (get-value A)) (remove B (get-value B))) (remove C (get-value C))))] (check-self-balancing-tree tree functions)) #t)
;; SFÂRȘIT CHECK - TASK 1 - NU modificați această linie!
;;
;; CHECK - TASK 2 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 2 : 20 puncte) ;;check-exp
(check-exp-part 'k-subsets1 0.1 (test-subsets (k-subsets (intersection A B) 8) result-k-subsets1) #t)
(check-exp-part 'k-subsets2 0.1 (let [(subsets (k-subsets binary-search-tree 11))] (and (= (length subsets) 78) (not (equal? (member '(2 3 5 6 8 9 11 12 13 15 21) subsets) #f)))) #t)
(check-exp-part 'k-subsets3 0.1 (test-subsets (k-subsets (create-tree '(1 2 3 4 5) functions) 3) result-k-subsets3) #t)
(check-exp-part 'k-subsets4 0.1 (test-subsets (k-subsets (create-tree '(8 7 6 5) functions) 2) result-k-subsets4) #t)
(check-exp-part 'k-subsets5 0.1 (test-subsets (k-subsets D 20) result-k-subsets5) #t)
(check-exp-part 'zig-zag-subsets1 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 2 3 4 5 6) functions)) result-zig-zag1) #t)
(check-exp-part 'zig-zag-subsets2 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 2 3 4) functions)) result-zig-zag2) #t)
(check-exp-part 'zig-zag-subsets3 0.1 (test-subsets (zig-zag-subsets (create-tree '(1 7 9 10 5) functions)) result-zig-zag3) #t)
(check-exp-part 'zig-zag-subsets4 0.1 (test-subsets (zig-zag-subsets (create-tree '(98 5 1 -85 -457) functions)) result-zig-zag4) #t)
(check-exp-part 'zig-zag-subsets5 0.1 (length (zig-zag-subsets (create-tree '(982 616 542 125 98 85) functions))) 122)
;; SFÂRȘIT CHECK - TASK 2 - NU modificați această linie!
;;
;; CHECK - BONUS - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Bonus 3 : 20 puncte BONUS) ;;check-exp
(check-exp-part 'bonus1 0.1 (test-bonus (parser '(1 + (((2 * 3) - 4) * 5))) functions) 11)
(check-exp-part 'bonus2 0.1 (test-bonus (parser '((((5 + 8) * (9 - (8 / 2))) + (8 * 9)) * 10)) functions) 1370)
(check-exp-part 'bonus3 0.1 (test-bonus (parser '((5 * 8) - (7 * (3 + (5 * (10 / 2)))))) functions) -156)
(check-exp-part 'bonus4 0.1 (test-bonus (parser '(((((80 - 78) + 15) * 4 ) / 2) + (7 + (((5 * 3) - 2) * 4)))) functions) 93)
(check-exp-part 'bonus5 0.2 (test-bonus (parser '(((((((((5 + 8) + (9 + 8)) * 3) + (8 - 7)) * 2) + 10) / 2) * 10) - (5 + (7 + (8 * (1 + 2)))))) functions) 924)
(check-exp-part 'bonus6 0.2 (test-bonus (parser '((((((5 + 6) * 7) + 9) * 10) / 2) + (7 * (2 * (4 * (10 - (7 + (1 * (2 - 1))))))))) functions) 542)
(check-exp-part 'bonus7 0.2 (test-bonus (parser '(((5 + (7 - (2 * (3 + (9 - (7 + (8 + (5 * 2)))))))) + (5 * (((2 + 2) * (3 + 7)) + (7 * (9 - (4 + 7)))))) / 2)) functions) 84)
;; SFÂRȘIT CHECK - BONUS - NU modificați această linie!
;; SFÂRȘIT SECȚIUNE DE TESTARE

(sumar)
