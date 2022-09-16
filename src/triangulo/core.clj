(ns triangulo.core
  (:require [clojure.math :as math]))

(defn calc-perimetro
  "Dado os lados [a b c] do triangulo, retorna o seu perimetro"
  [a b c]
  (+ a b c))

(defn calc-radianos
  "Dado os lados [a b c] do triangulo, retorna o ângulo ∠A em radianos"
  [a b c]
  (let [sqr-a (* a a)
        sqr-b (* b b)
        sqr-c (* c c)
        divider (* 2 (* b c))]
    (-> (+ sqr-b sqr-c)
        (- sqr-a)
        (/ divider)
        math/acos)))

(defn calc-angulo
  "Dado os lados [a b c] do triangulo, retorna o ângulo ∠A em graus"
  [a b c]
  (-> (calc-radianos a b c)
      math/to-degrees))

(defn calc-area
  "Dado os lados [a b c] do triangulo, retorna sua area"
  [a b c]
  (let [half-the-perimeter (/ (calc-perimetro a b c) 2)
        vector-of-sides (vector a b c)
        subtract-half-perimeter (partial - half-the-perimeter)]
    (->> (map subtract-half-perimeter vector-of-sides)
        (reduce * )
        (* half-the-perimeter)
        math/sqrt)))

(defn calc-altura
  "Dado um lado [a] do triangulo e sua area, retorna a altura do lado [a] até o vertice oposto"
  [a area]
  (-> (* 2 area)
      (/ a)))

(defn equilateral?
  "Dado os lados [a b c] do triangulo, retorna se ele é equilatero
  Um triangulo equilatero possui todos os seus lados com mesma medida"
  [a b c]
  (== a b c))

(defn isosceles?
  "Dado os lados [a b c] do triangulo, retorna se ele é isosceles
  Um triangulo isosceles possui pelo menos dois lados com mesma medida"
  [a b c]
  (or
    (= a b)
    (= b c)
    (= a c)))

(defn escaleno?
  "Dado os lados [a b c] do triangulo, retorna se ele é escaleno
  Um Triangulo escaleno possui todos os lados com medidas diferentes"
  [a b c]
  (not (isosceles? a b c))
  )

(defn calcula-todos-angulos
  "Dado os lados [a b c] do triangulo, retorna uma lista com todos os angulos em graus.
  Obs.: O resultado não é exato já que o angulo é arredondado"
  [a b c]
  (let [a-angle (math/round (calc-angulo a b c))
        b-angle (math/round (calc-angulo b a c))
        c-angle (math/round (calc-angulo c a b))]
    (list a-angle b-angle c-angle)
    ))

(defn retangulo?
  "Dado os lados [a b c] do triangulo, retorna se o triangulo é retangulo
  Um triangulo retangulo é um triangulo em que possui pelo menos um angulo igual a 90"
  [a b c]
  (let [all-angles (calcula-todos-angulos a b c)
        right-angle? (partial = 90)]
    (boolean (some right-angle? all-angles))
    ))

(defn obtuso?
  "Dado os lados [a b c] do triangulo, verifica se o triangulo é obtusângulo
  Um triangulo é obstusângulo caso exista algum angulo maior do que 90"
  [a b c]
  (let [all-angles (calcula-todos-angulos a b c)
        obtuse-angle? #(> % 90)]
    (boolean (some obtuse-angle? all-angles))))

(defn agudo?
  "Dado os lados [a b c] do triangulo, retorna se o triangulo é acutângulo
  O triangulo é acutângulo caso exista algum angulo menor do que 90"
  [a b c]
  (let [all-angles (calcula-todos-angulos a b c)
        acute-angle? #(< % 90)]
    (every? acute-angle? all-angles)))

(defn gerar-dados-completos
  [a b c]
  (let [area (calc-area a b c)]
        {:lados [a b c]
         :retagulo (retangulo? a b c)
         :obtuso (obtuso? a b c)
         :agudo (agudo? a b c)
         :escaleno (escaleno? a b c)
         :isosceles (isosceles? a b c)
         :equilateral (equilateral? a b c)
         :area area
         :altura [(calc-altura a area)
                  (calc-altura b area)
                  (calc-altura c area)]
         :angulos [(calc-angulo a b c)
                   (calc-angulo b c a)
                   (calc-angulo c a b)]}))

(comment
  (require 'clojure.pprint)
  (escaleno? 60 51.96152 30)
  (retangulo? 60 51.96152 30)
  (clojure.pprint/pprint (gerar-dados-completos 30 20 44))
  (clojure.pprint/pprint (gerar-dados-completos 60 51.96152 30))
  (clojure.pprint/pprint (gerar-dados-completos 15.14741 28.08887 30))
  )
