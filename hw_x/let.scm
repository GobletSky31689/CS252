(let ([x 3]) (println x)) ;; Differs from usual let syntax by missing extra parens
(let ([x 4]) (println x))

(let ([x 4]) (let ([y 2]) (println (+ x y))))
(let ([x 4][y 2]) (println (+ x y)))

