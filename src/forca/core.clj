(ns forca.core
  (:gen-class))

(def total-de-vidas 6)
(def palavra-secreta "AMOI")

(defn perdeu [] (print "Você perdeu!!"))
(defn ganhou [] (print "Você ganhou!!"))

(defn letras-faltantes [palavra acertos]
    (remove (fn [letra] (contains? acertos (str letra))) palavra))

(defn acertou-a-palavra-toda? [palavra acertos] 
    (empty? (letras-faltantes palavra acertos)))

(defn le-letra! [] 
  (do (print "Chute uma letra: ") (flush) (read-line)))

(defn acertou? [chute palavra] (.contains palavra chute))

(defn imprime-forca [vidas palavra acertos]
  (println "Vidas " vidas)
  (doseq [letra (seq palavra)]
    (if (contains? acertos (str letra))
      (print letra " ") (print "_" " ")))
  (println))

(defn jogo [vidas palavra acertos]
  (imprime-forca vidas palavra acertos)
  (cond 
    (= vidas 0) (perdeu)
    (acertou-a-palavra-toda? palavra acertos) (ganhou)
    :else
    (let [chute (le-letra!)]
      (if (acertou? chute palavra)
        (do 
          (println "Acertou a letra!")
          (recur vidas palavra (conj acertos chute)))
        (do
          (println "Errou!! Perdeu 1 vida!")
          (recur (dec vidas) palavra acertos))))))

(defn comeca-o-jogo [] ( jogo total-de-vidas palavra-secreta #{} ))

(defn -main [& args]
  (comeca-o-jogo))

;; Funções para testar conhecimentos

(defn fib[x]
    (if (= x 0) 0 
    (if (= x 1) 1 
    (+ (fib (- x 1)) (fib (- x 2))))))

(defn fib[x]
    (loop [a 1 b 1 numero 2]
      (if (= numero x) b
        (recur b (+ a b) (inc numero)))))        

(defn soma[n] 
    (loop [contador 1 soma 0]
        (if (> contador n) soma
        (recur (inc contador) (+ soma contador)))))

(defn listaDeSequencia [lista] 
  (->> lista 
    (map (fn [x] (* x 2))) 
    (reduce (fn [acc n] (+ acc n)))))