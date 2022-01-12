; --- common code ---

(defn parserFactory [toConst toVar toFunc] (fn parse [exp]
                                               (cond
                                                 (number? exp) (toConst exp)
                                                 (symbol? exp) (toVar (str exp))
                                                 :else (apply (toFunc (first exp)) (map parse (rest exp))))))

; --- HW-09 ---

(defn constant [x] (fn [m] x))
(defn variable [name] (fn [m] (m name)))

(defn divDouble [a b] (/ (double a) b))

(defn expressionFactory [operator]
      (fn [& args] (fn [vars] (apply operator (map #(% vars) args)))))

(def add (expressionFactory +))
(def subtract (expressionFactory -))
(def multiply (expressionFactory *))
(def divide (expressionFactory divDouble))
(def negate (expressionFactory -))
(def sinh (expressionFactory #(Math/sinh %)))
(def cosh (expressionFactory #(Math/cosh %)))

(def operationsFunctions {'+      add
                          '-      subtract
                          '*      multiply
                          '/      divide
                          'negate negate
                          'sinh   sinh
                          'cosh   cosh
                          })

(defn parseFunction [s] ((parserFactory constant variable operationsFunctions)
                         (read-string s)))

; --- HW-10 ---

(load-file "proto.clj")

(def _value (field :value))
(def _valueName (field :valueName))

(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))

(def _op (field :op))
(def _opName (field :opName))
(def _opArgs (field :opArgs))
(def _diffFunction (field :diffFunction))

(def toStringSuffix (method :toStringSuffix))

(defn createExpressionProto [evaluate toString diff toStringSuffix]
      {:evaluate       evaluate
       :toString       toString
       :diff           diff
       :toStringSuffix toStringSuffix
       })

(declare ZERO)
(def Constant (constructor
                (fn [this value] (assoc this :value value))
                (createExpressionProto
                  (fn [this vars] (_value this))
                  (fn [this] (format "%.1f" (_value this)))
                  (fn [this valueDiff] ZERO)
                  toString)))

(def ZERO (Constant 0.0))
(def ONE (Constant 1.0))

(def Variable (constructor
                (fn [this valueName] (assoc this :valueName valueName))
                (createExpressionProto
                  (fn [this vars] (get vars (clojure.string/lower-case (subs (_valueName this) 0 1))))
                  (fn [this] (_valueName this))
                  (fn [this valueDiff] (if (= (_valueName this) valueDiff) ONE ZERO))
                  toString)))

(def Operation (constructor
                 (fn [this op opName diffFunction] (assoc this :op op :opName opName :diffFunction diffFunction))
                 (createExpressionProto
                   (fn [this vars] (apply (_op this) (mapv #(evaluate % vars) (_opArgs this))))
                   (fn [this] (str "(" (_opName this) " " (clojure.string/join " " (mapv toString (_opArgs this))) ")"))
                   (fn [this diffVariable] ((_diffFunction this) (_opArgs this) (mapv #(diff % diffVariable) (_opArgs this))))
                   (fn [this] (str "(" (clojure.string/join " " (mapv toStringSuffix (_opArgs this))) " " (_opName this) ")")))))

(defn operationFactory [op opName diffFunction]
      (constructor
        (fn [this & opArgs] (assoc this :opArgs opArgs))
        (Operation op opName diffFunction)))

(defn diffFactory [func] (fn [args args'] (func args')))
(def Add (operationFactory + "+" (diffFactory #(apply Add %))))
(def Subtract (operationFactory - "-" (diffFactory #(apply Subtract %))))
(def Negate (operationFactory - "negate" (diffFactory #(apply Negate %))))

(def Multiply (operationFactory * "*"
                                (fn [args args'] (Add (Multiply (nth args' 0) (nth args 1))
                                                      (Multiply (nth args 0) (nth args' 1))))))
(def Divide (operationFactory divDouble "/"
                              (fn [args args'] (Divide (Subtract (Multiply (nth args' 0) (nth args 1))
                                                                 (Multiply (nth args 0) (nth args' 1)))
                                                       (Multiply (nth args 1) (nth args 1))))))

(declare Cosh)
(def Sinh (operationFactory #(Math/sinh %) "sinh" (fn [[args] [args']] (Multiply args' (Cosh args)))))
(def Cosh (operationFactory #(Math/cosh %) "cosh" (fn [[args] [args']] (Multiply args' (Sinh args)))))

(defn booleanFactory [f]
      #(if (f (> %1 0.0) (> %2 0.0)) 1.0 0.0))
(def And (operationFactory (booleanFactory #(and %1 %2)) "&&" nil))
(def Or (operationFactory (booleanFactory #(or %1 %2)) "||" nil))
(def Xor (operationFactory (booleanFactory #((comp not =) %1 %2)) "^^" nil))

(def operationsObjects {'+            Add
                        '-            Subtract
                        '*            Multiply
                        '/            Divide
                        'negate       Negate
                        'sinh         Sinh
                        'cosh         Cosh
                        (symbol "&&") And
                        (symbol "||") Or
                        (symbol "^^") Xor
                        })

(defn parseObject [s] ((parserFactory Constant Variable operationsObjects)
                       (read-string s)))

; --- HW-11 ---

(load-file "parser.clj")

(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(def *digit (+char "0123456789"))
(def *number (+map read-string (+str (+seq (+opt (+char "-")) (+str (+plus *digit)) (+char ".") *digit))))

(def *all-chars (mapv char (range 32 128)))
(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))

(defn *parseExpression [p] (+seqn 1 *ws (+char "(") (+seqf cons *ws p (+star (+seqn 0 *ws p))) *ws (+char ")")))

(def *identifieOpChar (+or *letter (+char "+-*/&|^")))
(def *identifieVarChar (+char "xyzXYZ"))
(def *identifierVar (+str (+seqf cons *ws *identifieVarChar (+star *identifieVarChar))))
(def *identifierOp (+str (+seqf cons *ws *identifieOpChar (+star *identifieOpChar))))

(def parseObjectSuffix
  (letfn [(*expression [] (delay (+or
                                   (+map Constant *number)
                                   (+map (comp Variable str) *identifierVar)
                                   (+map (comp #(operationsObjects %) symbol) *identifierOp)
                                   (+map #(apply (last %) (drop-last %)) (*parseExpression (*expression)))
                                   )))]
         (+parser (+seqn 0 *ws (*expression) *ws))))