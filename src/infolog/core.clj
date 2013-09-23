(ns infolog.core
  (:import infolog.PrologLexer))

(def t1 "/**a*/ p (a).")
(def t2 "/** \nHallo Welt\n */ p (a).")
(def t3 "/** \n@author Mike\n    @date Today\n    @descr  \n    This module cotains several basic examples\n*/\n\n:-module(test, [foo/1]).\n\n%% element(Element,List) if E in List then true\n\nelement(E,[E|_T]).\nelement(E, [_H|T]):-\n    element( E, T ).\n%! is_not_visited(Element,List) if E not in List then true\n\nis_not_visited( _ , [] ).\nis_not_visited( H, [H|_T] ):-\n    false.\nis_not_visited( A, [H|T] ):-\n    A \\= H,\n    is_not_visited(A, T).\n\n/** @author me\n    @descr returns true\n*/\n\nistrue(true).\n\n\ncheck_boolean_expression(BExpr) :- %%covering all boolean expression with relational operators instead of '==' and '!='%\n  relational_binary_op(BExpr,Arg1,Arg2,EX,EY,Call),!,\n  evaluate_int_argument(Arg1,EX),\n  evaluate_int_argument(Arg2,EY),\n  Call.\n\n\nfoo(a).")


(defn transform-comments [token]
  (let [text (.getText token)
        start-line (-> token .getStartPos .getLine)
        start-column (-> token .getStartPos .getPos)
        end-line (-> token .getEndPos .getLine)
        end-column (-> token .getEndPos .getPos)]
    {:text text
     :start-line start-line
     :start-column start-column
     :end-line end-line
     :end-column end-column}))

(defn extract-comments [text]
  (let [comments (PrologLexer/lex text)]
    (map transform-comments comments)))
