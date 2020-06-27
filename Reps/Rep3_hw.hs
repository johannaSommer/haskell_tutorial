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

