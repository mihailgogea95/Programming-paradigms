module RL where

import Data.Array
import Data.List
import System.Random
import Text.Printf

{-
    O stare este reprezentată ca un număr întreg.
    O cale este o listă de stări, eventual infinită.

    O estimare reprezintă o mulțime de asocieri (stare, informație aferentă).
    Cu toate că mediul este bidimensional, utilizăm o reprezentare liniară
    a acestuia, bazată pe un `Array`, cu stările indexate ca în figura din 
    enunț.
-}
type State      = Int
type Path       = [State]
type Estimation = Array State StateInfo

{-
    Lățimea și înălțimea mediului, precum și numărul de stări.
-}
width, height, nStates :: Int
width   = 4
height  = 3
nStates = width * height

{-
    Perechile de stări vecine.
-}
neighbors :: [(State, State)]
neighbors = [ (1, 2), (1, 5)
            , (2, 1), (2, 3)
            , (3, 2), (3, 4)
            , (3, 7)
            , (4, 3), (4, 8)
            , (5, 1), (5, 9)
            , (7, 3), (7, 8), (7, 11)
            , (8, 4), (8, 7), (8, 12)
            , (9, 5), (9, 10)
            , (10, 9), (10, 11)
            , (11, 7), (11, 10), (11, 12)
            , (12, 8), (12, 11)
            ]

{-
    Starea de pornire.
-}
startState :: State
startState = 1

{-
     Stările terminale.
-}
terminalStates :: [State]
terminalStates = [8, 12]

{-
    Rata de învățare alfa și factorul de scalare a acesteia.
-}
learningRate, scaleFactor :: Float
learningRate = 0.1
scaleFactor  = 0.999

-------------------------------------------------------------------------------
-- Completați sub această linie.


--  === 1. Generarea căilor ===

{-
    *** TODO ***

    Întoarce toate stările vecine ale unei stări.
-}
neighborsOf :: State -> [State]
neighborsOf x = map  snd ( filter (\m -> (fst m) == x) neighbors )

{-
    *** TODO ***

    Construiește o cale aleatoare infinită, pe baza unui generator.

    Hint: `Data.List.iterate`, `System.Random.split`.
-}
-- Aceasta functie este pentru a intoarce un vecin in functie de 
-- numarul random pe care il dau.
getVecin n rand = (nei) !!  (fromIntegral (mod rand len) )
 where
  nei = neighborsOf n
  len = length (neighborsOf n)

-- Aceasta functie este pentru a genera o cale aleatoare infinita pe baza unui generator
randFromGen stare gen = stare : (randFromGen (getVecin stare (fst (next gen))) (snd (next gen)))
 
randomPath :: RandomGen g => g -> (Path, g)
randomPath g = ((randFromGen startState (fst gen2)), (snd gen2)) 
 where
  gen2 = split g
{-
    *** TODO ***

    Trunchiază o cale, eventual infinită, la prima stare terminală.
-}

caleTerminala cale infinita 
 | (elem (head infinita) terminalStates) == True = (cale ++ [(head infinita)]) 
 | otherwise = (caleTerminala (cale ++ [(head infinita)]) (tail infinita))

terminatePath :: Path -> Path
terminatePath lista = (caleTerminala [] lista) 

{-
    *** TODO ***

    Construiește o infinitate de căi infinite.
-}

caleInfinita g = (fst (randomPath g)) ++ (caleInfinita (snd (next (snd (randomPath g)))))

randomPaths :: RandomGen g => g -> [Path]
randomPaths g = [(fst (randomPath g))] ++ (randomPaths (snd (next (snd (randomPath g)))))


--  === 2. Estimarea utilităților fără diminuarea ratei de învățare ===

{-
    *** TODO ***

    Array cu conscințele specifice fiecărei stări.
-}
reinforcements :: Array State Float
reinforcements = array (1,nStates) [(x, (if x == 8 then -1 else (if x == 12 then 1 else 0))) | x <- [1..nStates]]

{-
    *** TODO ***

    Valorile inițiale ale stărilor, înaintea rulării algoritmului.
    Se construiesc pe baza array-ului de consecințe.
-}
initialEstimation :: Estimation
initialEstimation = array (1,nStates) [(x, (StateInfoC (reinforcements ! x) 0)) | x <- [1..nStates]]

{-
    *** TODO ***

    Lista de utilități provenite dintr-o estimare.
-}
values :: Estimation -> [Float]
values est = [(px (est ! y)) | y <- [1..nStates]]

{-
    *** TODO ***

    Reprezentarea sub formă de șir de caractere a unei estimări.
    Se va întrebuința forma bidimensională, ca în imaginile din enunț.
    De asemenea, utilitățile vor fi rotunjite la 2 zecimale, și vor
    avea semnul inclus.

    Hint: `Text.Printf`.

    Exemplu de rezultat:

    -0.07 +0.06 +0.20 +1.00
    -0.20 +0.00 -0.43 -1.00
    -0.32 -0.45 -0.56 -0.78

    Pentru a vizualiza corect caracterele de linie nouă, aplicați
    în interpretor funcția `putStrLn` asupra șirului obținut.
-}
-- Functie pentru o linie din tabel , urmand apoi sa leg aceste linii
functie1 es nr = (reverse (tail (reverse  (foldl (\x y -> x ++ (printf "%+.2f " (px (es ! y)))) "" [nr..(nr + (width - 1))]))))


showEstimation :: Estimation -> String
showEstimation es =  (reverse (tail (reverse (foldl (\x y -> x ++ (functie1 es y) ++ "\n") "" (reverse [1,(width + 1)..nStates])))))

{-
    *** TODO ***

    Actualizează o estimare în urmare parcurgerii unei căi.

    Hint: `Data.Array.accum`.
-}
-- Formula din enuntul temei
formula v ve alfa = (v + (alfa * (ve - v)))

-- Parcurg cu accum array-ul si modific pe parcurs in functie de calea data
-- urmand sa modific alfa a unei stari in functie de numarul de treceri
-- prin aceea stare.

updateEstimation :: Estimation -> Path -> Estimation
updateEstimation e cale = if (length cale) == 1 then e 
 else (updateEstimation (accum (\x y -> (StateInfoC (formula (px x) (px y) (scaledLearningRates !! ((py x) + 1))) ((py x) + 1))) e
 [(head cale, e ! (head (tail cale)))]) (tail cale))

{-
    *** TODO ***

    Obține un flux infinit de estimări rafinate succesiv, pe baza unui flux
    infinit de căi finite, încheiate în stări terminale.

    Hint: `Data.List.mapAccumL`.
-}
estimations :: [Path] -> [Estimation]
estimations infinitCai = snd (mapAccumL (\acc x -> let acc1 = (updateEstimation acc x) in (acc1,acc1)) initialEstimation infinitCai)

{-
    *** TODO ***

    Determină estimarea de rang dat ca parametru, pe baza unui generator.
-}
--trunchiez o lista infinita de cai infinite la o lista infinita de cai terminate in stari terminale
trunchiere lista = [(terminatePath x) | x <- lista]

estimate :: RandomGen g => Int -> g -> Estimation
estimate rang g = (last (take rang (estimations (trunchiere (randomPaths g)))))

{-
    *** TODO ***

    Pentru o stare, determină vecinul cu cea mai mare valoare estimată.

    Hint: `Data.Function.on`.
-}
--perechi pentru o stare (stare,valoare)
-- Aceasta functie returneaza o lista de perechi cu indexul starii vecine impreuna cu
-- valoarea acestuia
perechiStare est stare = [(x, (px (est ! x))) | x <- (neighborsOf stare)]

bestNeighborOf :: State -> Estimation -> State
bestNeighborOf stare estimare = fst (foldl (\x y -> if ((snd x) > (snd y)) then  x else  y) (head  (perechiStare estimare stare)) (perechiStare estimare stare))  

{-
    *** TODO ***

    Contruiește o cale începută în starea inițială, pe principiul alegerii 
    vecinului cu utilitata maximă.
-}
-- Construiesc un drum cel mai bun in functie de valori , cu un acumulator
drumBun es start acc 
 | (elem (bestNeighborOf start es) terminalStates) == True =  [(bestNeighborOf start es),start] ++ acc
 | otherwise = (drumBun es (bestNeighborOf start es) ([start] ++ acc)) 

bestPath :: Estimation -> Path
bestPath estim = (reverse (drumBun estim 1 []))


--  === 3. Estimarea utilităților cu diminuarea ratei de învățare ===

{-
    *** TODO ***

    Fluxul infinit al ratelor de învățare scalate:

    [ 1
    , learningRate
    , learningRate * scaleFactor
    , learningRate * scaleFactor^2
    , ...
    ]
-}
-- Vector [scaleFactor , scaleFactor^2 ..]
factorScalare = iterate (\x -> (x * scaleFactor)) 1

scaledLearningRates :: [Float]
scaledLearningRates = 1 : (map (learningRate *) factorScalare)

{-
    *** TODO ***

    Tip de date pentru reținerea atât a valorii estimate a unei stări,
    cât și a numărului de vizitări ale acesteia.
-}
-- px returneaza valoarea de tip Float si py returneaza numarul de cate ori a trecut prin
-- aceea stare.
data StateInfo = StateInfoC
 { px :: Float
 , py :: Int
 }deriving (Show)

