(ns clj-tiger.t_parse
  (:use [midje.sweet]
        [clj-tiger.parse]))

(facts "About parsing variable declarations."
       (fact "It can parse simple variable declarations with integers."
             (parse-tiger "var x := 412")
             => [:S [:dec [:vardec "x" [:int 412]]]]
             (parse-tiger "var x : int := 4123")
             => [:S [:dec [:tvardec "x" "int" [:int 4123]]]]
             (parse-tiger "var a := 10
                           var b := 20")
             => [:S [:dec [:vardec "a" [:int 10]]]
                 [:dec [:vardec "b" [:int 20]]]]))

(facts "About function declarations."
       (fact "Function declaration parse."
             (parse-tiger "function foo(i : int) = 100")
             => [:S [:dec [:fundec "foo" [["i" "int"]] [:int 100]]]]
             (parse-tiger "function foo(i : int, j:int) = 100")
             => [:S [:dec [:fundec "foo" [["i" "int"] ["j" "int"]] [:int 100]]]]

             (parse-tiger "function foo(i : int) : int = 100")
             => [:S [:dec [:tfundec "foo" [["i" "int"]] "int" [:int 100]]]]
             (parse-tiger "function foo(i : int, j:int) : int = 100")
             => [:S [:dec [:tfundec "foo" [["i" "int"] ["j" "int"]] "int" [:int 100]]]]))

;; ## Part 3 Type Declarations, arrays, records

(facts "About type declarations."
       (fact "We can declare simple types."
             (parse-tiger "type foo = int")
             => [:S [:dec [:tydec "foo" "int"]]])
       (fact "We can declare record types."
             (parse-tiger "type foo = { i :int }")
             => [:S [:dec [:tydec "foo" [["i" "int"]]]]]
             (parse-tiger "type bar = { i : foo, j : bar }")
             => [:S [:dec [:tydec "bar" [["i" "foo"] ["j" "bar"]]]]])
       (fact "We can declare arrays."
             (parse-tiger "type baz = array of foo")
             => [:S [:dec [:tydec "baz" [:array "foo"]]]]))

;; ## Part 4 --- Variable Expressions, including records and subscripts

(facts "About variable expressions"
       (fact "We can parse simple variables."
             (parse-tiger "var x := y")
             => [:S [:dec [:vardec "x" [:varexp "y"]]]])
       (fact "We can parse record field access."
             (parse-tiger "var x := y.x")
             => [:S [:dec [:vardec "x" [:varexp "y" [:field "x"]]]]]
             (parse-tiger "var x := y.x.z")
             => [:S [:dec [:vardec "x" [:varexp "y" [:field "x"] [:field "z"]]]]])
       (fact "We can parse array subscripts."
             (parse-tiger "var x := y[10]")
             => [:S [:dec [:vardec "x" [:varexp "y" [:arraysub [:int 10]]]]]]
             (parse-tiger "var x := y[z]")
             => [:S [:dec [:vardec "x" [:varexp "y" [:arraysub [:varexp "z"]]]]]]
             (parse-tiger "var x := y[10][30]")
             => [:S [:dec [:vardec "x" [:varexp "y" [:arraysub [:int 10]] [:arraysub [:int 30]]]]]])
       (fact "We can parse combinations of array subscripts and record fields."
             (parse-tiger "var x := z.a[q.b[10]].b[20]")
             => [:S [:dec [:vardec "x" [:varexp "z" [:field "a"] [:arraysub [:varexp "q" [:field "b"] [:arraysub [:int 10]]]]
                                        [:field "b"] [:arraysub [:int 20]]]]]]))

;; ## Part 5 --- l-value, nil, parens, sequencing, no-value, string literal

(facts "About nil"
       (fact "We can parse it."
             (parse-tiger "var x := nil") => [:S [:dec [:vardec "x" [:nil]]]]))

(facts "About parens"
       (fact "We can parse them too."
             (parse-tiger "var x := (a)")
             => [:S [:dec [:vardec "x" [:varexp "a"]]]]
             (parse-tiger "var x := ((a))")
             => [:S [:dec [:vardec "x" [:varexp "a"]]]]
             (parse-tiger "var x := (((a)))")
             => [:S [:dec [:vardec "x" [:varexp "a"]]]]
             (parse-tiger "var x := ((((a))))")
             => [:S [:dec [:vardec "x" [:varexp "a"]]]]))

(facts "About sequences"
       (fact "They are parens containing semicolon-separated expressions."
             (parse-tiger "var x := (a;b;c)")
             => [:S [:dec [:vardec "x" [:seqexp [:varexp "a"] [:varexp "b"] [:varexp "c"]]]]]
             (parse-tiger "var x := (a;(x;y;z);c)")
             => [:S [:dec [:vardec "x" [:seqexp [:varexp "a"] [:seqexp [:varexp "x"] [:varexp "y"] [:varexp "z"]] [:varexp "c"]]]]]
             ))

;; Don't worry about the octal and multiline stuff unless you want to use that.
(facts "About string literals"
       (fact "We can parse strings."
             (parse-tiger "var x := \"asdf\"")
             => [:S [:dec [:vardec "x" [:string "asdf"]]]]
             (parse-tiger "var x := \"as\tdf\"")
             => [:S [:dec [:vardec "x" [:string "as\tdf"]]]]))


;; ## Part 6 --- negation, function calls, arithmetic, comparison
;; Hint: use a stratified grammar to handle precedence.

(facts "About arithmetic"
       (fact "Precedence is good for add, multiply, subtract, and divide."
             (parse-tiger "var x := a + c")
             => [:S [:dec [:vardec "x" [[:addop "+"] [:varexp "a"] [:varexp "c"]]]]]
             (parse-tiger "var x := a * c")
             => [:S [:dec [:vardec "x" [[:mulop "*"] [:varexp "a"] [:varexp "c"]]]]]
             (parse-tiger "var x := a * b - c * d ")
             => [:S [:dec [:vardec "x" [[:addop "-"]
                                        [[:mulop "*"] [:varexp "a"] [:varexp "b"]]
                                        [[:mulop "*"] [:varexp "c"] [:varexp "d"]]]]]]

             (parse-tiger "var x := a * b * c * d ")
             => [:S [:dec [:vardec "x" [[:mulop "*"] 
                                        [[:mulop "*"] 
                                         [[:mulop "*"] [:varexp "a"] [:varexp "b"]]
                                         [:varexp "c"]]
                                        [:varexp "d"]]]]])

       (fact "add, multiply, subtract, and divide are left associative."
             (parse-tiger "var x := a + c + d")
             => [:S [:dec [:vardec "x" [[:addop "+"]
                                        [[:addop "+"]
                                         [:varexp "a"] [:varexp "c"]] [:varexp "d"]]]]]
             (parse-tiger "var x := a * c * d")
             => [:S [:dec [:vardec "x" [[:mulop "*"] [[:mulop "*"] [:varexp "a"] [:varexp "c"]] [:varexp "d"]]]]]
             (parse-tiger "var x := a * b * c * d ")
             => [:S [:dec [:vardec "x" [[:mulop "*"]
                                        [[:mulop "*"] 
                                         [[:mulop "*"] [:varexp "a"] [:varexp "b"]]
                                         [:varexp "c"]]
                                        [:varexp "d"]]]]]
             (parse-tiger "var x := a * b * c * d * e")
             => [:S [:dec [:vardec "x" [[:mulop "*"]
                                        [[:mulop "*"]
                                         [[:mulop "*"]
                                          [[:mulop "*"] [:varexp "a"] [:varexp "b"]]
                                          [:varexp "c"]]
                                         [:varexp "d"]]
                                        [:varexp "e"]]]]])

       (fact "Precedencs is good for comparisons."
             (parse-tiger "var x := a - c < b * d")
             => [:S [:dec [:vardec "x" [[:compop "<"]
                                        [[:addop "-"] [:varexp "a"] [:varexp "c"]]
                                        [[:mulop "*"] [:varexp "b"] [:varexp "d"]]]]]]
             (parse-tiger "var x := a - c > b * d")
             => [:S [:dec [:vardec "x" [[:compop ">"] [[:addop "-"] [:varexp "a"] [:varexp "c"]]
                                        [[:mulop "*"] [:varexp "b"] [:varexp "d"]]]]]]))

(facts "About negation"
       (fact "Negations are handled correctly."
             (parse-tiger "var x := a - c > - b * - d")
             => [:S [:dec [:vardec "x" [[:compop ">"] [[:addop "-"] [:varexp "a"] [:varexp "c"]]
                                        [[:mulop "*"] ["-" [:varexp "b"]] ["-" [:varexp "d"]]]]]]]
             ))

;; ## Part 7 --- Booleans

(facts "About Booleans"
       (fact "And and Or follow proper precedences as well."
             (parse-tiger "var x := a & b | c & d")
             => [:S [:dec [:vardec "x" [[:orop "|"]
                                        [[:andop "&"] [:varexp "a"] [:varexp "b"]]
                                        [[:andop "&"] [:varexp "c"] [:varexp "d"]]]]]]

             (fact "Precedencs is good for comparisons."
                   (parse-tiger "var x := a - c < b * d & a > 5")
                   => [:S [:dec [:vardec "x" [[:andop "&"]
                                              [[:compop "<"]
                                               [[:addop "-"] [:varexp "a"] [:varexp "c"]]
                                               [[:mulop "*"] [:varexp "b"] [:varexp "d"]]]
                                              [[:compop ">"] [:varexp "a"] [:int 5]]]]]]) ))

;; ## Part 8 --- Records, Array, Assignments

(facts "About assignment"
       (fact "Simple assignments work."
             (parse-tiger "var x := a := 10")
             => [:S [:dec [:vardec "x" [:assign [:varexp "a"] [:int 10]]]]])
       (fact "Record fields can be used as L-values and record can be created."
             (parse-tiger "var x := data.first := list(car=10,cdr=data.cdr)")
             => [:S [:dec [:vardec "x" [:assign [:varexp "data" [:field "first"]]
                                    [:record "list" [:field "car" [:int 10]]
                                     [:field "cdr" [:varexp "data" [:field "cdr"]]]]]]]])

       (fact "Arrays can be used as L-values and arrays can be created."
             (parse-tiger "var x := data[first] := list [ 20 ] of 0")
             => [:S [:dec [:vardec "x"
                           [:assign [:varexp "data" [:arraysub [:varexp "first"]]] 
                            [:arrayexp "list" [:int 20] [:int 0]]]]]]))

;; ## Part 9 --- if-then-else,if-then,while,for,break

(facts "About control structures"
       (fact "if-then-else works"
             (parse-tiger "var x := if x > 5 then a + b else a")
             => [:S [:dec [:vardec "x"
                           [:ifexp [[:compop ">"]
                                    [:varexp "x"]
                                    [:int 5]]
                            [[:addop "+"]
                             [:varexp "a"]
                             [:varexp "b"]]
                            [:varexp "a"]]]]])
       (fact "if-then works"
             (parse-tiger "var x := if x > 5 then a")
             => [:S [:dec [:vardec "x"
                           [:ifexp [[:compop ">"]
                                    [:varexp "x"]
                                    [:int 5]]
                            [:varexp "a"]]
                           ]]])
       (fact "nested if-then-else works"
             (parse-tiger "var x := if x > 5 then if x < 10 then a + e else a")
             => [:S [:dec [:vardec "x"
                           [:ifexp [[:compop ">"]
                                    [:varexp "x"]
                                    [:int 5]]
                            [:ifexp
                             [[:compop "<"]
                              [:varexp "x"]
                              [:int 10]]
                             [[:addop "+"]
                              [:varexp "a"]
                              [:varexp "e"]]
                             [:varexp "a"]]]]]]
             (parse-tiger "var x := if x > 5 then if x < 10 then a else c else f")
             => [:S [:dec [:vardec "x"
                           [:ifexp [[:compop ">"]
                                 [:varexp "x"]
                                 [:int 5]]
                            [:ifexp
                             [[:compop "<"]
                              [:varexp "x"]
                              [:int 10]]
                             [:varexp "a"]
                             [:varexp "c"]] 
                            [:varexp "f"]]]]])
       (fact "while works"
             (parse-tiger "var x := while a > 10 do a := 10")
             => [:S [:dec [:vardec "x" [:whileexp  [[:compop ">"] [:varexp "a"] [:int 10]]
                                        [:assign [:varexp "a"] [:int 10]]]]]])

       (fact "for and break works"
             (parse-tiger "var x := for a := 10 + 30 to b + c do (a + 1)")
             => [:S [:dec [:vardec "x"
                           [:forexp "a"
                            [[:addop "+"] [:int 10] [:int 30]]
                            [[:addop "+"] [:varexp "b"] [:varexp "c"]]
                            [[:addop "+"] [:varexp "a"] [:int 1]]]]]]
             (parse-tiger "var x := for a := 10 + 30 to b + c do break")
             => [:S [:dec [:vardec "x"
                           [:forexp "a"
                            [[:addop "+"] [:int 10] [:int 30]]
                            [[:addop "+"] [:varexp "b"] [:varexp "c"]]
                            [:break]]]]]))

(facts "about let"
       (fact "it handles single declaration"
             (parse-tiger "var x := let var v := 10 in v end")
             => [:S [:dec [:vardec "x"
                           [:letexp [:dec [:vardec "v" [:int 10]]]
                            [:varexp "v"]]]]])

       (fact "it handles multiple declarations"
             (parse-tiger "var x := let var v := 10 var x := 20 in v + x end")
             => [:S [:dec [:vardec "x"
                           [:letexp
                            [:dec [:vardec "v" [:int 10]]]
                            [:dec [:vardec "x" [:int 20]]]
                            [[:addop "+"]
                             [:varexp "v"]
                             [:varexp "x"]]]]]]))
