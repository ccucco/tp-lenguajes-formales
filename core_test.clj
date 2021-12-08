(ns proyecto-scheme-99593.core-test
  (:require [clojure.test :refer :all]
            [proyecto-scheme-99593.core :refer :all]))

(deftest leer-entrada-test
  (testing "Leer entrada"
     (is (= "(hola mundo)"  (with-in-str "(hola mundo)" (leer-entrada))))
     (is (= "123"           (with-in-str "123" (leer-entrada))))
  )
)

(deftest verificar-parentesis-test
  (testing "Verificar parentesis"
    (is (= 1  (verificar-parentesis "(hola 'mundo")))
    (is (= -1 (verificar-parentesis "(hola '(mundo)))")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
    (is (= 0  (verificar-parentesis "(hola '(mundo) )")))
  )
)

(deftest actualizar-ambiente-test
  (testing "Actualizar ambiente"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3)     (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
    (is (= '(a 1 b 2 c 3)     (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))))
    (is (= '(b 7)             (actualizar-amb () 'b 7)))
  )
)

(deftest buscar-test
  (testing "Buscar"
    (is (= '3                                            (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
    (is (= (generar-mensaje-error :unbound-variable 'f)  (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
  )
)

(deftest error?-test
  (testing "Error?"
    (is (= true   (error? (list (symbol ";ERROR:") 'mal 'hecho)) ))
    (is (= false  (error? (list 'mal 'hecho)) ))
    (is (= true   (error? (list (symbol ";WARNING:") 'mal 'hecho)) ))
  )
)

(deftest proteger-bool-en-str-test
  (testing "Proteger Bool En Str"
    (is (= "(or %F %f %t %T)"           (proteger-bool-en-str "(or #F #f #t #T)") ))
    (is (= "(and (or %F %f %t %T) %T)"  (proteger-bool-en-str "(and (or #F #f #t #T) #T)") ))
    (is (= ""                           (proteger-bool-en-str "") ))
  )
)

(deftest restaurar-bool-test
  (testing "Restaurar Bool"
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)"))  (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))))
  )
)

(deftest igual?-test
  (testing "Igual?"
    (is (= true   (igual? 'if 'IF) ))
    (is (= true   (igual? 'if 'if) ))
    (is (= true   (igual? 'IF 'IF) ))
    (is (= false  (igual? 'IF "IF") ))
    (is (= false  (igual? 6 "6") ))
  )
)

(deftest fnc-append-test
  (testing "Fnc Append"
    (is (= '(1 2 3 4 5 6 7)                                     (fnc-append '( (1 2) (3) (4 5) (6 7)))))
    (is (= (generar-mensaje-error :wrong-type-arg 'append '3)   (fnc-append '( (1 2) 3 (4 5) (6 7)))))
    (is (= (generar-mensaje-error :wrong-type-arg 'append 'A)   (fnc-append '( (1 2) A (4 5) (6 7)))))
    (is (= '(1 2 (3 4 (5)) 6 7)                                 (fnc-append '( (1 2) ((3 4 (5))) (6 7)))))
  )
)

(deftest fnc-equal?-test
  (testing "Fnc Equal?"
    (is (= (symbol "#t")  (fnc-equal? ()) ))
    (is (= (symbol "#t")  (fnc-equal? '(A)) ))
    (is (= (symbol "#t")  (fnc-equal? '(A a)) ))
    (is (= (symbol "#t")  (fnc-equal? '(A a A)) ))
    (is (= (symbol "#t")  (fnc-equal? '(A a A a)) ))
    (is (= (symbol "#f")  (fnc-equal? '(A a A B)) ))
    (is (= (symbol "#t")  (fnc-equal? '(1 1 1 1)) ))
    (is (= (symbol "#f")  (fnc-equal? '(1 1 2 1)) ))
    (is (= (symbol "#f")  (fnc-equal? '(("asd" 5)("ASD" 5))) ))
    (is (= (symbol "#t")  (fnc-equal? '(("asd" 5)("asd" 5))) ))
  )
)

(deftest fnc-read-test
  (testing "Fnc read"
     (is (= '(hola mundo)                                               (with-in-str "(hola mundo)" (fnc-read ()))))
     (is (= (generar-mensaje-error :io-ports-not-implemented 'read)     (fnc-read '(1))))
     (is (= (generar-mensaje-error :wrong-number-args-prim-proc 'read)  (fnc-read '(1 2))))
     (is (= (generar-mensaje-error :wrong-number-args-prim-proc 'read)  (fnc-read '(1 2 3))))
  )
)

(deftest fnc-sumar-test
  (testing "Fnc sumar"
    (is (= 0                                              (fnc-sumar ()) ))
    (is (= 3                                              (fnc-sumar '(3)) ))
    (is (= 7                                              (fnc-sumar '(3 4)) ))
    (is (= 12                                             (fnc-sumar '(3 4 5)) ))
    (is (= 18                                             (fnc-sumar '(3 4 5 6)) ))
    (is (= (generar-mensaje-error :wrong-type-arg1 '+ 'A) (fnc-sumar '(A 4 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '+ 'A) (fnc-sumar '(3 A 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '+ 'A) (fnc-sumar '(3 4 A 6))))
  )
)

(deftest fnc-restar-test
  (testing "Fnc restar"
    (is (= (generar-mensaje-error :wrong-number-args-oper '-)   (fnc-restar ()) ))
    (is (= -3                                                   (fnc-restar '(3)) ))
    (is (= -1                                                   (fnc-restar '(3 4)) ))
    (is (= -6                                                   (fnc-restar '(3 4 5)) ))
    (is (= -12                                                  (fnc-restar '(3 4 5 6)) ))
    (is (= (generar-mensaje-error :wrong-type-arg1 '- 'A)       (fnc-restar '(A 4 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '- 'A)       (fnc-restar '(3 A 5 6))))
    (is (= (generar-mensaje-error :wrong-type-arg2 '- 'A)       (fnc-restar '(3 4 A 6))))
  )
)

(deftest fnc-menor-test
  (testing "Fnc Menor"
    (is (= (symbol "#t")                                   (fnc-menor ()) ))
    (is (= (symbol "#t")                                   (fnc-menor '(1)) ))
    (is (= (symbol "#t")                                   (fnc-menor '(1 2)) ))
    (is (= (symbol "#t")                                   (fnc-menor '(1 2 3)) ))
    (is (= (symbol "#t")                                   (fnc-menor '(1 2 3 4)) ))
    (is (= (symbol "#f")                                   (fnc-menor '(1 2 2 4)) ))
    (is (= (symbol "#f")                                   (fnc-menor '(1 2 1 4)) ))
    (is (= (generar-mensaje-error :wrong-type-arg1 '< 'A)  (fnc-menor '(A 1 2 4)) ))
    (is (= (generar-mensaje-error :wrong-type-arg2 '< 'A)  (fnc-menor '(1 A 1 4)) ))
    (is (= (generar-mensaje-error :wrong-type-arg2 '< 'A)  (fnc-menor '(1 2 A 4)) ))
  )
)

(deftest fnc-mayor-test
  (testing "Fnc Mayor"
    (is (= (symbol "#t")                                   (fnc-mayor ()) ))
    (is (= (symbol "#t")                                   (fnc-mayor '(1)) ))
    (is (= (symbol "#t")                                   (fnc-mayor '(2 1)) ))
    (is (= (symbol "#t")                                   (fnc-mayor '(3 2 1)) ))
    (is (= (symbol "#t")                                   (fnc-mayor '(4 3 2 1)) ))
    (is (= (symbol "#f")                                   (fnc-mayor '(4 2 2 1)) ))
    (is (= (symbol "#f")                                   (fnc-mayor '(4 2 1 4)) ))
    (is (= (generar-mensaje-error :wrong-type-arg1 '> 'A)  (fnc-mayor '(A 3 2 1)) ))
    (is (= (generar-mensaje-error :wrong-type-arg2 '> 'A)  (fnc-mayor '(3 A 2 1)) ))
    (is (= (generar-mensaje-error :wrong-type-arg2 '> 'A)  (fnc-mayor '(3 2 A 1)) ))
  )
)

(deftest fnc-mayor-o-igual-test
  (testing "Fnc Mayor o igual"
    (is (= (symbol "#t")                                    (fnc-mayor-o-igual ()) ))
    (is (= (symbol "#t")                                    (fnc-mayor-o-igual '(1)) ))
    (is (= (symbol "#t")                                    (fnc-mayor-o-igual '(2 1)) ))
    (is (= (symbol "#t")                                    (fnc-mayor-o-igual '(3 2 1)) ))
    (is (= (symbol "#t")                                    (fnc-mayor-o-igual '(4 3 2 1)) ))
    (is (= (symbol "#t")                                    (fnc-mayor-o-igual '(4 2 2 1)) ))
    (is (= (symbol "#f")                                    (fnc-mayor-o-igual '(4 2 1 4)) ))
    (is (= (generar-mensaje-error :wrong-type-arg1 '>= 'A)  (fnc-mayor-o-igual '(A 3 2 1)) ))
    (is (= (generar-mensaje-error :wrong-type-arg2 '>= 'A)  (fnc-mayor-o-igual '(3 A 2 1)) ))
    (is (= (generar-mensaje-error :wrong-type-arg2 '>= 'A)  (fnc-mayor-o-igual '(3 2 A 1)) ))
  )
)

(deftest evaluar-escalar-test
  (testing "Evaluar escalar"
    (is (= '(32 (x 6 y 11 z "hola"))                      (evaluar-escalar 32 '(x 6 y 11 z "hola")) ))
    (is (= '("chau" (x 6 y 11 z "hola"))                  (evaluar-escalar "chau" '(x 6 y 11 z "hola")) ))
    (is (= '(11 (x 6 y 11 z "hola"))                      (evaluar-escalar 'y '(x 6 y 11 z "hola")) ))
    (is (= '("hola" (x 6 y 11 z "hola"))                  (evaluar-escalar 'z '(x 6 y 11 z "hola")) ))
    (is (= (generar-mensaje-error :unbound-variable 'n)   (first (evaluar-escalar 'n '(x 6 y 11 z "hola")) )))
    (is (= '(x 6 y 11 z "hola")                           (second (evaluar-escalar 'n '(x 6 y 11 z "hola")) )))
  )
)

(deftest evaluar-define-test
  (testing "Evaluar define"
    (is (= (symbol "#<unspecified>")                                          (first (evaluar-define '(define x 2) '(x 1))) ))
    (is (= '(x 2)                                                             (second (evaluar-define '(define x 2) '(x 1))) ))
    (is (= (symbol "#<unspecified>")                                          (first (evaluar-define '(define (f x) (+ x 1)) '(x 1))) ))
    (is (= '(x 1 f (lambda (x) (+ x 1)))                                      (second (evaluar-define '(define (f x) (+ x 1)) '(x 1))) ))
    (is (= (symbol "#<unspecified>")                                          (first (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))) ))
    (is (= '(x 1 f (lambda (x) (display x) (newline) (+ x 1)))                (second (evaluar-define '(define (f x) (display x) (newline) (+ x 1)) '(x 1))) ))
    (is (= (generar-mensaje-error :missing-or-extra 'define '(define))        (first (evaluar-define '(define) '(x 1))) ))
    (is (= '(x 1)                                                             (second (evaluar-define '(define) '(x 1))) ))
    (is (= (generar-mensaje-error :missing-or-extra 'define '(define x))      (first (evaluar-define '(define x) '(x 1))) ))
    (is (= '(x 1)                                                             (second (evaluar-define '(define x) '(x 1))) ))
    (is (= (generar-mensaje-error :missing-or-extra 'define '(define ()))     (first (evaluar-define '(define ()) '(x 1))) ))
    (is (= '(x 1)                                                             (second (evaluar-define '(define ()) '(x 1))) ))
    (is (= (generar-mensaje-error :bad-variable 'define '(define () 2))       (first (evaluar-define '(define () 2) '(x 1))) ))
    (is (= '(x 1)                                                             (second (evaluar-define '(define () 2) '(x 1))) ))
    (is (= (generar-mensaje-error :bad-variable 'define '(define 2 x))        (first (evaluar-define '(define 2 x) '(x 1))) ))
    (is (= '(x 1)                                                             (second (evaluar-define '(define 2 x) '(x 1))) ))
  )
)

(deftest evaluar-if-test
  (testing "Evaluar if"
    (is (= '(2 (n 7))                                             (evaluar-if '(if 1 2) '(n 7)) ))
    (is (= '(7 (n 7))                                             (evaluar-if '(if 1 n) '(n 7)) ))
    (is (= '(7 (n 7))                                             (evaluar-if '(if 1 n 8) '(n 7)) ))
    (is (= (symbol "#<unspecified>")                              (first (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))) ))
    (is (= "(n 7 %f %f)"                                          (proteger-bool-en-str (second (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f"))))) ))
    (is (= "(8 (n 7 %f %f))"                                      (proteger-bool-en-str (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))) ))
    (is (= "(%<unspecified> (n 9 %f %f))"                         (proteger-bool-en-str (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))) ))
    (is (= (generar-mensaje-error :missing-or-extra 'if '(if))    (first (evaluar-if '(if) '(n 7))) ))
    (is (= '(n 7)                                                 (second (evaluar-if '(if) '(n 7))) ))
    (is (= (generar-mensaje-error :missing-or-extra 'if '(if 1))  (first (evaluar-if '(if 1) '(n 7))) ))
    (is (= '(n 7)                                                 (second (evaluar-if '(if 1) '(n 7))) ))
  )
)

(deftest evaluar-or-test
  (testing "Evaluar or"
    (is (= "(%f (%f %f %t %t))"   (proteger-bool-en-str (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))) ))  
    (is (= "(%t (%f %f %t %t))"   (proteger-bool-en-str (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))) ))
    (is (= "(7 (%f %f %t %t))"   (proteger-bool-en-str (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))) ))
    (is (= "(5 (%f %f %t %t))"   (proteger-bool-en-str (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))) ))
    (is (= "(%f (%f %f %t %t))"   (proteger-bool-en-str (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))) ))
  )
)


(deftest evaluar-set!-test
  (testing "Evaluar Set!"
    (is (= (symbol "#<unspecified>")                                        (first (evaluar-set! '(set! x 1) '(x 0))) ))
    (is (= '(x 1)                                                           (second (evaluar-set! '(set! x 1) '(x 0))) ))
    (is (= (generar-mensaje-error :unbound-variable 'x )                    (first (evaluar-set! '(set! x 1) '())) ))
    (is (= '()                                                              (second (evaluar-set! '(set! x 1) '())) ))
    (is (= (generar-mensaje-error :missing-or-extra 'set! '(set! x) )       (first (evaluar-set! '(set! x) '(x 0))) ))
    (is (= '(x 0)                                                           (second (evaluar-set! '(set! x) '(x 0))) ))
    (is (= (generar-mensaje-error :missing-or-extra 'set! '(set! x 1 2) )   (first (evaluar-set! '(set! x 1 2) '(x 0))) ))
    (is (= '(x 0)                                                           (second (evaluar-set! '(set! x) '(x 0))) ))
    (is (= (generar-mensaje-error :bad-variable 'set! '1 )                  (first (evaluar-set! '(set! 1 2) '(x 0))) ))
    (is (= '(x 0)                                                           (second (evaluar-set! '(set! 1 2) '(x 0))) ))
  )
)




