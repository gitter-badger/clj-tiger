(ns clj-tiger.parse
  (:require [clojure.edn :as edn]
            [instaparse.core :as inst]))

(defn rewrite-arith
  ([x] x)
  ([x y] [x y])
  ([e1 op e2]  [op e1 e2])
  )

(defn rewrite-assign
  ([x] x)
  ([e1 e2] [:assign e1 e2]))

(defn xformer [t]
  (inst/transform {;:exp (fn [& e] (apply vec e))
                   :int (fn [i] [:int (edn/read-string i)])
                   :term rewrite-arith
                   :factor rewrite-arith
                   :comp rewrite-arith
                   :andterm rewrite-arith
                   :orterm rewrite-arith
                   :negatom rewrite-arith
                   :tyfield (fn [x y] [x y])
                   :id (fn [x] x)
                   :tyfields (fn [& rest] (vec rest))
                   :kwarrayof (fn [& id] [:array id])
                   :assign rewrite-assign
                   :recordfield (fn [n e] [:field n e])
                   
                   } t))

(def parser
  (inst/parser
   "S = {dec}

    dec = vardec 
        | tvardec
        | fundec
        | tfundec
        | tydec

    vardec =  <kwvar> id <assigned> exp
    tvardec = <kwvar> id <colon> id <assigned> <ws> exp
    fundec = <kwfunction> id <lp> tyfields <rp> <equal> exp
    tfundec = <kwfunction> id <lp> tyfields <rp> <colon> id <equal> exp
    tydec = <kwtype> id <equal> array
          | <kwtype> id <equal> <lb> tyfields <rb>
          | <kwtype> id <equal> id

    tyfields = Epsilon
             | tyfield { <comma> tyfield }

    tyfield = id <colon> id

    array = <'array'> <ws+> <'of'> <ws+> id

    <exp> = assign

    assign = varexp <assigned> orterm
           | orterm
    orterm = orterm orop andterm
            | andterm
    andterm = andterm andop comp
            | comp
    comp = term 
         | term compop term
    term = term addop factor | factor
    factor = factor mulop negatom | negatom
    negatom = '-' <ws*> atom
            | atom

    <atom> = int
        | <lp> exp <rp>
        | seqexp
        | string
        | record
        | arrayexp
        | ifexp
        | whileexp
        | forexp
        | break
        | letexp
        | (nil / varexp)

    letexp = <kwlet> {dec} <kwin> {exp} <kwend>
    break = <kwbreak>
    forexp = <kwfor> id <assigned> exp <kwto> exp <kwdo> exp
    ifexp = <kwif> exp <kwthen> exp <kwelse> exp
          | <kwif> exp <kwthen> exp
    whileexp = <kwwhile> exp <kwdo> exp
    varexp = id {field | arraysub}
    record = id <'('> <ws*> recordfield { <comma> recordfield } <')'> <ws*>
    recordfield = id <equal> exp
    arrayexp = id <ls> exp <rs> <kwof> exp
    int = #'[0-9]+' <ws*>
    field = <'.'> id
    arraysub = <ls> exp <rs>
    nil = <kwnil>
    seqexp = <lp> exp ( <semicolon> exp ) + <rp>
    string = <quote> #'([^\"]|\\\\|\\\\\")*' <quote>

    kwbreak = 'break' <ws*>
    kwfor = 'for' <ws*>
    kwend = 'end' <ws*>
    kwlet = 'let' <ws*>
    kwto = 'to' <ws*>
    kwnil = 'nil' <ws*>
    kwif = 'if' <ws+>
    kwin = 'in' <ws+>
    kwthen = 'then' <ws+>
    kwelse = 'else' <ws+>
    kwdo = 'do' <ws+>
    kwwhile = 'while' <ws+>
    kwvar = 'var' <ws+>
    kwfunction = 'function' <ws+>
    kwtype = 'type' <ws+>
    kwof = 'of' <ws+>
    
    orop = '|' <ws*>
    andop = '&' <ws*>
    compop = ('='|'<'|'<='|'>'|'>='|'<>') <ws*>
    mulop = ('*' | '/') <ws*>
    addop = ('+' | '-') <ws*>

    quote = '\"'
    lb = '{' <ws*>
    rb = '}' <ws*>
    lp = '(' <ws*>
    rp = ')' <ws*>
    ls = '[' <ws*>
    rs = ']' <ws*>
    assigned = ':=' <ws*>
    equal = '=' <ws*>
    colon = ':' <ws*>
    semicolon = ';' <ws*>
    comma = ',' <ws*>
    id = #'[a-z][a-z0-9]*' <ws+>

    ws = #'\\s*'  ")
  )

(defn parse-tiger
  [input]
  (-> input
      parser
      xformer))

