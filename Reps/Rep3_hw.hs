module Rep3_hw where

Lemma sumAddEqSumAux: sum xs + a .=. sum_aux xs a
Proof by induction on List xs
Case []
      To show: sum [] + a .=. sum_aux [] a

      Proof
      sum [] + a
      (by def sum)     .=. sum_aux [] 0 + a
      (by def sum_aux) .=. 0 + a
      (by zeroAdd)     .=. a
      (by def sum_aux) .=. sum_aux [] a
  QED

Case (x:xs)
    To show: sum (x:xs) + a .=. sum_aux (x:xs) a
    IH: sum xs + a .=. sum_aux xs a

    Proof
            sum (x:xs) + a
    (by def sum) .=.  sum_aux (x:xs) 0 + a
    (by def sum_aux) .=. sum_aux xs (0+x) + a
    (by zeroAdd) .=. sum_aux xs x + a
    (by IH) .=. sum xs + x + a

            sum_aux (x:xs) a
    (by def sum_aux) .=. sum_aux xs (a+x)
    (by IH)          .=. sum xs + (a + x)
    (by addComm)     .=. sum xs + (x + a)
    (by addAssoc)    .=. sum xs + x + a
   QED
QED


Lemma: sum (xs ++ ys) .=. sum xs + sum ys
Proof by induction on List xs
    Case []
    To show: sum ([] ++ ys) .=. sum [] + sum ys

    Proof
            sum ([] ++ ys)
    (by def ++) .=. sum ys
            sum [] + sum ys
    (by def sum) .=. sum_aux [] 0 + sum ys
    (by def sum_aux) .=. 0 + sum ys
    (by zeroAdd) .=. sum ys
    QED

    Case (x:xs)
    To show: sum ((x:xs) ++ ys) .=. sum (x:xs) + sum ys
    IH: sum (xs ++ ys) .=. sum xs + sum ys

    Proof
            sum ((x:xs) ++ ys)
    (by def ++) .=. sum (x : (xs ++ ys))
    (by def sum) .=. sum_aux (x : (xs ++ ys)) 0
    (by def sum_aux) .=. sum_aux (xs ++ ys) (0+x)
    (by sumAddEqSumAux) .=. sum (xs ++ ys) + (0+x)
    (by zeroAdd) .=. sum (xs ++ ys) + x
    (by IH) .=. (sum xs + sum ys) + x

            sum (x:xs) + sum ys
    (by sumAddEqSumAux) .=. sum_aux (x:xs) (sum ys)
    (by def sum_aux) .=. sum_aux xs ((sum ys) + x)
    (by sumAddEqSumAux) .=. sum xs + ((sum ys) + x)
    (by addAssoc) .=. (sum xs + sum ys) + x
        QED
 QED

-------
data List a = [] | a : List a
data Tree a = Leaf | Node (Tree a) a (Tree a)

sumTree Leaf = 0
sumTree (Node l x r) = sumTree l + x + sumTree r

sum [] = 0
sum (x:xs) = x + sum xs

inorder Leaf = []
inorder (Node l x r) = (inorder l) ++ (x : (inorder r))

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

axiom zeroAdd: 0 + b .=. b
axiom addAssoc: a + (b + c) .=. (a + b) + c

goal sum (inorder t) .=. sumTree t

Lemma sum_append: sum (xs ++ ys) .=. sum xs + sum ys
Proof by induction on List xs

Case []
  To show: sum ([] ++ ys) .=. sum [] + sum ys

  Proof
                              sum ([] ++ ys)
      (by def ++)         .=. sum ys
      (by zeroAdd)        .=. 0 + sum ys
      (by def sum)        .=. sum [] + sum ys
  QED

Case x:xs
  To show: sum ((x:xs) ++ ys) .=. sum (x:xs) + sum ys
  IH:      sum (xs ++ ys)     .=. sum xs + sum ys

  Proof
                              sum ((x:xs) ++ ys)
      (by def ++)         .=. sum (x : (xs ++ ys))
      (by def sum)        .=. x + sum (xs ++ ys)
      (by IH)             .=. x + (sum xs + sum ys)
      (by addAssoc)       .=. (x + sum xs) + sum ys
      (by def sum)        .=. sum (x:xs) + sum ys
  QED
QED

Lemma: sum (inorder t) .=. sumTree t
Proof by induction on Tree t
Case Leaf
    To show: sum (inorder Leaf) .=. sumTree Leaf

    Proof
    sum (inorder Leaf)
    (by def inorder) .=. sum []
    (by def sum) .=. 0
    sumTree Leaf
    (by def sumTree) .=. 0
    QED

Case Node l x r
     To show: sum (inorder (Node l x r)) .=. sumTree (Node l x r)
       IH1:     sum (inorder l)            .=. sumTree l
       IH2:     sum (inorder r)            .=. sumTree r

     Proof
         sum (inorder (Node l x r))
         (by def inorder) .=. sum ((inorder l) ++ (x : (inorder r)))
         (by sum_append) .=. sum (inorder l) + sum (x : (inorder r))
         (by def sum) .=. sum (inorder l) + (x + sum(inorder r))
         (by IH1) .=. sumTree l + (x + sum(inorder r))
         (by IH2) .=. sumTree l + (x + sumTree r)
         (by addAssoc) .=. sumTree l + x + sumTree r

     sumTree (Node l x r)
         (by def sumTree) .=. sumTree l + x + sumTree r
     QED
 QED