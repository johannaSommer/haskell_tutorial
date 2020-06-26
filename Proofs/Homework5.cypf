Lemma:  reverse (reverse xs) .=. xs
	Proof by induction on List xs
	Case []
		To show: reverse (reverse []) .=. []
		
		Proof
				reverse (reverse [])
		(by def reverse) .=. reverse []
		(by def reverse) .=. []
		QED
		
	Case x:xs
        To show: reverse (reverse (x:xs)) .=. (x:xs)
		IH:  reverse (reverse xs) .=. xs
		
		Proof
				reverse (reverse (x:xs))
                      (by def reverse)  .=. reverse (snoc (reverse xs) x)
		(by reverse_snoc) .=. x: reverse (reverse xs)
                      (by IH)           .=. x:xs
                      QED
        QED

Lemma: reverse (snoc xs y) .=. y : reverse xs
    Proof by induction on List xs
    Case []
        To show: reverse (snoc [] y) .=. y : reverse []
        
        Proof
		        reverse (snoc [] y) 
        (by def snoc)     .=. reverse (y:[])
        (by def reverse)  .=. snoc (reverse[]) y
        (by def reverse)  .=. snoc [] y
        (by def snoc)     .=. [y]
        (by def :)        .=. (y:[])
        (by def reverse)  .=. (y:reverse [])
        QED

    Case x:xs
        To show: reverse (snoc (x:xs) y) .=. y: reverse (x:xs)
        IH: reverse (snoc xs y) .=. y: reverse xs
        Proof
	        	 reverse (snoc (x:xs) y) 
        (by def snoc)     .=. reverse (x:snoc xs y)
        (by def reverse)  .=. snoc(reverse (snoc xs y)) x
        (by IH)           .=. snoc (y: reverse xs) x
        (by def snoc)     .=. y:snoc (reverse xs) x
        (by def reverse)  .=. y:reverse (x:xs)
        QED
    QED

Lemma:  sum (xs ++ ys) .=. sum xs + sum ys
	Proof by induction on List xs
	Case []
        To show: sum ([] ++ ys) .=. sum [] + sum ys
		
		Proof
            sum ([] ++ ys)
            (by def ++) .=. sum ys
            (by zeroAdd) .=. 0 + sum ys
            (by def sum) .=. sum [] + sum ys
		QED
		
	Case x:xs
        To show: sum ((x:xs) ++ ys) .=. sum (x:xs) + sum ys
        IH:  sum (xs ++ ys) .=. sum xs + sum ys
		
		Proof
            sum ((x:xs) ++ ys)
            (by def ++) .=. sum (x : (xs ++ ys)) 
            (by def sum) .=. x + sum (xs ++ ys)
            (by IH) .=. x + (sum xs + sum ys)
            (by addAssoc) .=. (x + sum xs) + sum ys
            (by def sum) .=. sum (x:xs) + sum ys
                      QED
        QED
      
Lemma:  sum (mapLength (mapAppend xs yss)) .=. sum (mapLength yss) + (length xs * length yss)
	Proof by induction on List yss
	Case []
        To show: sum (mapLength (mapAppend xs [])) .=. sum (mapLength []) + (length xs * length [])
        
		
		Proof
            sum (mapLength (mapAppend xs []))
            (by def mapAppend) .=. sum (mapLength [])
            (by addZero) .=. sum (mapLength []) + 0
            (by mulZero) .=. sum (mapLength []) + (length xs * 0)
            (by def length) .=. sum (mapLength []) + (length xs * length [])
            
		QED
		
	Case x:xs
        To show: sum (mapLength (mapAppend xs y:yss)) .=. sum (mapLength y:yss) + (length xs * length y:yss)
        IH:  sum (mapLength (mapAppend xs yss)) .=. sum (mapLength yss) + (length xs * length yss)
		
		Proof
            Proof
                                    sum (mapLength (mapAppend xs (y:yss)))
        (by def mapAppend)      .=. sum (mapLength ((y ++ xs) : mapAppend xs yss))
        (by def mapLength)      .=. sum (length (y ++ xs) : mapLength (mapAppend xs yss))
        (by def sum)            .=. length (y ++ xs) + sum (mapLength (mapAppend xs yss))
        (by IH)                 .=. length (y ++ xs) + (sum (mapLength yss) + length xs * length yss)
        (by length_append)      .=. length y + length xs + (sum (mapLength yss) + length xs * length yss)
        (by addReorder)         .=. length y + sum (mapLength yss) + (length xs + length xs * length yss)
        (by mulOne)             .=. length y + sum (mapLength yss) + (length xs * 1 + length xs * length yss)
        (by mulAdd)             .=. length y + sum (mapLength yss) + (length xs * (1 + length yss))
        (by def length)         .=. length y + sum (mapLength yss) + (length xs * length (y:yss))
        (by def sum)            .=. sum (length y : mapLength yss) + (length xs * length (y:yss))
        (by def mapLength)      .=. sum (mapLength (y:yss)) + (length xs * length (y:yss))
            
                      QED
        QED