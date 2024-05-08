--Alumno: Eduardo Biali Garcia Gomez
--Num. Cuenta: 320113987

{-Ejercicio 1:
Dado un arreglo numérico, devolver la suma de sus elementos.-}
sumaElems :: [Int] -> Int
sumaElems lst = sumaAux lst 0
  where
    sumaAux [] acc = acc
    sumaAux (x:xs) acc = sumaAux xs (acc + x)


{-
Ejercicio 2:
Dada una cadena de caracteres, devolver una cadena con las vocales
que están en ella.
-}

vocales :: String -> String
vocales str = vocalesAux str ""
  where
    vocalesAux [] acc = acc
    vocalesAux (x:xs) acc
      | x `elem` "aeiouAEIOU" = vocalesAux xs (acc ++ [x])
      | otherwise = vocalesAux xs acc

{-
Ejercicio 3:
Dado un arreglo circular ordenado pero rotado, devolver el número de
desplazamientos necesarios para que el arreglo inicie en la posición 0.
-}

giraArreglo :: [Int] -> Int
giraArreglo lst = giraAux lst 1
  where
    giraAux [x] _ = 0
    giraAux (x:y:xs) count
      | x > y = count
      | otherwise = giraAux (y:xs) (count + 1)




{-
Ejercicio 4: 
Dado un número decimal, devolver una representación binaria del mismo.
-}

decBin :: Int -> String
decBin 0 = "0"
decBin n = decBinAux n ""
  where
    decBinAux 0 acc
      | acc == "" = "0"
      | otherwise = acc
    decBinAux num acc = decBinAux (num `div` 2) (show (num `mod` 2) ++ acc)


{-
Ejercicio 5:
Marinela tiene una promoción en la que regala gansitos al comprar
gansitos. Supongamos que sabemos exactamente cuántos gansitos tiene
que comprar una persona para ganar otro gansito. El objetivo es saber
cuántos gansitos en total podemos comer dado el precio del gansito, el
dinero disponible y los gansitos a comprar para obtener uno gratis.
-}

gansitos :: Int -> Int -> Int -> Int
gansitos precio dinero promo = gansitosAux (dinero `div` precio) 0
  where
    gansitosAux 0 acc = acc
    gansitosAux comprados acc
      | comprados >= promo = gansitosAux (comprados - promo) (acc + promo + 1)
      | otherwise = acc + comprados








