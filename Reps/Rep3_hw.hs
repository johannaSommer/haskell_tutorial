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


---

data Tree a = L | N (Tree a) a (Tree a)

flat L = []
flat (N l x r) = flat l ++ (x : flat r)

app L xs = xs
app (N l x r) xs = app l (x : app r xs)

[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

axiom app_assoc: (xs ++ ys) ++ zs .=. xs ++ (ys ++ zs)
axiom app_nil: xs ++ [] .=. xs
axiom nil_app: [] ++ xs .=. xs

goal app t [] .=. flat t

Lemma gen: app t xs .=. flat t ++ xs
Proof by induction on Tree t
Case L
    To show: app L xs .=. flat L ++ xs

    Proof
    app L xs
    (by def app) .=. xs

    flat L ++ xs
    (by def flat) .=. [] ++ xs
    (byy def ++) .=. xs
    QED

Case (N l x r)
    To show: app (N l x r) xs .=. flat (N l x r) ++ xs
    IH1: app l xs .=. flat l ++ xs
    IH2: app r xs .=. flat r ++ xs

    Proof
    app (N l x r) xs
    (by def app) .=. app l (x : app r xs)
    (by IH1) .=. flat l ++ (x: app r xs)
    (by IH2) .=. flat l ++ (x:(flat r ++ xs))
    (by def ++) .=. flat l ++ ((x:flat r) ++ xs)
    (by app_assoc) .=. flat l ++ (x : flat r) ++ xs

    flat (N l x r) ++ xs
    (by def flat) .=. flat l ++ (x : flat r) ++ xs
    QED
 QED

Lemma: app t [] .=. flat t
Proof
                     app t []
    (by gen)     .=. flat t ++ []
    (by app_nil) .=. flat t
QED

---------------------

data List a = [] | a : List a
data Bool = True | False

filter f [] = []
filter f (x : xs) = if f x then x : filter f xs else filter f xs

(f . g) x = f (g x)

axiom if_True: (if True then x else y) .=. x
axiom if_False: (if False then x else y) .=. y

goal filter p . filter p .=. filter p

---

Lemma fpfp: filter p (filter p xs) .=. filter p xs
Proof by induction on List xs
Case []
    To show: filter p (filter p []) .=. filter p []

    Proof
    filter p (filter p [])
    (by def filter) .=. filter p []
    QED

Case (x:xs)
    To show: filter p (filter p (x:xs)) .=. filter p (x:xs)
    IH: filter p (filter p xs) .=. filter p xs
    Case True
        Assumption: f x == True

        Proof
        filter p (filter p (x:xs))
        (by def filter) .=. filter p (if p x then x : filter p xs else filter p xs)
        (by Assumption) .=. filter p (if True then x : filter p xs else filter p xs)
        (by if_True) .=. filter p (x : filter p xs)
        (by def filter) .=. if f x then x : filter f (filter p xs) else filter f (filter p xs)
        (by Assumption) .=. if True then x : filter f (filter p xs) else filter f (filter p xs)
        (by if_True) .=. x : filter p (filter p xs)
        (by IH) .=. x : filter p xs

        filter p (x:xs)
        (by def filter) .=. if f x then x : filter f xs else filter f xs
        (by Assumption) .=. if True then x : filter f xs else filter f xs
        (by if_True) .=. x : filter p xs
        QED
    Case False
        Assumption: f x == False

        Proof
        filter p (filter p (x:xs))
        (by def filter) .=. filter p (if p x then x : filter p xs else filter p xs)
        (by Assumption) .=. filter p (if False then x : filter p xs else filter p xs)
        (by if_False) .=. filter p (filter p xs)
        (by IH) .=. filter p xs

        filter p (x:xs)
        (by def filter) .=. if f x then x : filter f xs else filter f xs
        (by Assumption) .=. if False then x : filter f xs else filter f xs
        (by if_False) .=. filter p xs
        QED
    QED
QED


Lemma: filter p . filter p .=. filter p
Proof by extensionality with xs
  To show: (filter p . filter p) xs .=. filter p xs

  Proof
                     (filter p . filter p) xs
      (by def .) .=. filter p (filter p xs)
      (by fpfp)  .=. filter p xs
  QED
QED

