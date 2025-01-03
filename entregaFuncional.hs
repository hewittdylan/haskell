{-
    Entrega Programación Funcional - Dylan Hewitt

    Funcionamiento: 
    - Al ejecutar la aplicación nos pedirá introducir las preguntas del test como lista de tuplas, con el siguiente formato:
        [(Float, Int, Int)] 
        Siendo Float la puntuación de la pregunta, el número de respuestas poisbles, y el número de la correcta
        Ejemplo: [(1.0,3,1),(0.5,4,4),(1.0,3,2),(0.5,2,2)]
    - A continuación nos pedirá introducir los modelos del test (reordenaciones de las preguntas) como lista de listas:
        [[Int]]
        Aquí cada lista de enteros es una reordenación de las preguntas
        Ejemplo: [[2,1,0,3],[0,2,3,1],[3,2,1,0]]
    - Por último nos pedirá introducir respuestas de los alumnos, como lista de tuplas de la siguiente manera:
        [(String, Int, [Int])]
        Aquí String será el identificativo del alumno DNI/Nombre, Int el modelo que le ha tocado (1..n) y por último las respuestas que ha escogido para su modelo
        Ejemplo: [("Juan",3,[2,0,3,1]),("Maria",1,[3,4,1,2])]
        !!! Importante recalcar que si el alumno deja una respuesta en blanco esta se señala con un 0, y las respuestas van de 0 a n siendo n el número de alternativas de la pregunta

    Implementación:
    Primero tenemos que definir los tipos que vamos a emplear:
        - Pregunta: Guardaremos el valor de la pregunta, así como el número de opciones y la opción que sea correcta
        - Test: Guardamos una lista con las preguntas del test, así como una lista por cada modelo con una lista de índices para cada uno
        - RespuestaTest: Guardamos para cada respuesta el nombre (o DNI (String)), el índice del modelo y la respuesta que ha marcado en cada pregunta (0 si la deja en blanco)
        - Corrección: Guardaremos para cada corrección el nombre/DNI del alumno, y su nota final, tanto la total como sobre 10
        - Frecuencias: Guardamos para cada pregunta cuantas veces se ha acertado, fallado o dejado en blanco de forma absoluta y relativa
        - Estadísticas: Para el total de los alumnos vamos a calcular la siguiente información:
            * Puntuación media, número medio de preguntas respondidas, número de suspensos, aprobados, notables y sobresalientes,
              frecuencias por cada pregunta, pregunta más acertada, pregunta menos acertada, pregunta más dejada en blanco y menos dejada en blanco,
              preguntas con mejor y peor resultados, y preguntas más y menos veces dejada en blanco

    Definimos después las 2 funciones principales del programa, junto con funciones auxiliares:
        - corrige: Dado un test y unas respuestas al propio test devuelve la corrección de las respuestas con su respectiva calificación
        - (aux) calculaPuntuacion: Dada una pregunta y una respuesta sueltas, devuelve los puntos que suma o resta 
        - estadisticas: Dado un test y un conjunto de respuestas al test devuelve una serie de estadísticas sobre los mismos
        - (aux) reordenarRespuestas: reordena las preguntas del alumno según el modelo que le haya tocado
        - (aux) traspuesta: Hace la traspuesta de una matriz (lista de listas)
        - (aux) calcularFrecuencias: Para cada pregunta calcula cuantas veces se ha acertado, fallado o dejado en blanco
        - (aux) mejoresPeores: Devuelve la pregunta más, y la menos acertada
        - (aux) masMenosBlancos: Devuelve la pregunta más, y menos dejada en blanco

    Por último vamos a definir una función que permita la E/S de datos al usuario (main), el formato de ejemplo de los mismos debe ser:
        - preguntas del test: [(1.0,3,1),(0.5,4,4),(1.0,3,2),(0.5,2,2)]
        - modelos del test: [[2,1,0,3],[0,2,3,1],[3,2,1,0]]
        - las respuestas de los alumnos: [("Juan",3,[2,0,3,1]),("Maria",1,[3,4,1,2])]
            - En lugar de nombres podemos emplear los dnis, o cualquier otro String que distinga a los alumnos

        Vamos a añadir también otras 2 auxiliares de E/S : printEstadisticas y printFrecuencias que impriman los resultados

-}

import Data.List (maximumBy, minimumBy)
import Data.Function (on)

type Pregunta = (Float, Int, Int)
type Test = ([Pregunta], [[Int]])
type RespuestaTest = (String, Int, [Int])
type Correccion = (String, Float, Float)
type Frecuencias = [(Int, Int, Int, Float, Float, Float)]
type Estadisticas = (Float, Float, Int, Int, Int, Int, Frecuencias, Int, Int, Int, Int)

corrige :: Test -> RespuestaTest -> Correccion   -- Dado un Test corrige las respuestas de RespuestaTest devolviendo una Corrección
corrige (preguntas, modelos) (nombre, modelo, respuestas) =
    let 
        ordenPreguntas = modelos !! (modelo - 1) -- Sacamos el orden de las preguntas para el modelo del alumno
        preguntasOrdenadas = map (preguntas !!) ordenPreguntas -- Ordenamos las preguntas del test para que concuerden con las del alumno
        puntuacionTotal = sum (zipWith calculaPuntuacion preguntasOrdenadas respuestas) -- Usamos calculaPuntuación para calcular cuánto suma/resta cada pregunta
        puntuacionSobre10 = (puntuacionTotal / sum (map (\(valor, _, _) -> valor) preguntas)) * 10  -- Dividimos la puntuación total obtenida entre el valor máximo obtenible
    in (nombre, puntuacionTotal, puntuacionSobre10)

-- Calcula cuanto suma cada pregunta corrigiéndola
calculaPuntuacion :: (Float, Int, Int) -> Int -> Float
calculaPuntuacion (valor, nAlternativas, correcta) respuesta
    | respuesta == 0       = 0       -- Respuesta en blanco
    | respuesta == correcta = valor  -- Respuesta correcta
    | otherwise            = - (valor / fromIntegral (nAlternativas - 1)) -- Respuesta incorrecta

estadisticas :: Test -> [RespuestaTest] -> Estadisticas
estadisticas test respuestas =
    let correcciones = map (corrige test) respuestas  -- Vamos primero a corregir las respuestas con la función previamente definida
        notas = map (\(_, _, nota) -> nota) correcciones  -- Obtenemos la nota sobre 10 para cada una de ellas (no nos interesa el nombre)

        -- Métricas generales
        mediaNota = sum notas / fromIntegral (length notas)  -- Calculamos la nota media
        preguntasRespondidas = map (\(_, _, reps) -> length ( filter (/= 0) reps)) respuestas -- Contamos cuantas preguntas se han contestado (distinto de 0) en total
        mediaPreguntasRespondidas = sum (map fromIntegral preguntasRespondidas) / fromIntegral (length preguntasRespondidas) -- Calculamos la media de preguntas respondidas
        -- Calculamos ahora cuantos alumnos hay en cada tramo: suspenso / aprobado / notable / sobresaliente
        suspensos = length $ filter (< 5) notas
        aprobados = length $ filter (\x -> x >= 5 && x < 7) notas
        notables = length $ filter (\x -> x >= 7 && x < 9) notas
        sobresalientes = length $ filter (>= 9) notas

        -- Frecuencias por pregunta
        preguntas = fst test
        modelos = snd test
        respuestasOrdenadas = map (reordenarRespuestas modelos) respuestas
        respuestasPorPregunta = traspuesta (map (\(_, _, rs) -> rs) respuestas)  --traspuesta implementada debajo
        frecuencias = zipWith calcularFrecuencias respuestasPorPregunta preguntas

        -- Mejores, peores y preguntas en blanco
        (mejorPregunta, peorPregunta) = mejoresPeores frecuencias
        (masBlanco, menosBlanco) = masMenosBlancos frecuencias
    in (mediaNota, mediaPreguntasRespondidas, suspensos, aprobados, notables, sobresalientes, frecuencias, mejorPregunta, peorPregunta, masBlanco, menosBlanco)

-- Función para reordenar respuestas según el modelo
reordenarRespuestas :: [[Int]] -> RespuestaTest -> RespuestaTest
reordenarRespuestas modelos (nombre, modelo, respuestas) =
    let orden = modelos !! (modelo - 1)
        respuestasReordenadas = map (respuestas !!) orden
    in (nombre, modelo, respuestasReordenadas)

-- Calcula la traspuesta de una matriz
-- Ejemplo: [[1, 2, 3], [4, 5, 6], [7, 8, 9]] ---> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
traspuesta :: [[a]] -> [[a]]
traspuesta [] = []
traspuesta ([]:_) = [] -- Caso base: cuando alguna sublista está vacía
traspuesta xss = map head xss : traspuesta (map tail xss)

-- Cálculo de frecuencias para cada pregunta
calcularFrecuencias :: [Int] -> (Float, Int, Int) -> (Int, Int, Int, Float, Float, Float)
calcularFrecuencias respuestas (_, alternativas, correcta) =
    let totalRespuestas = length respuestas
        correctas = length $ filter (== correcta) respuestas
        erroneas = length $ filter (\x -> x /= correcta && x /= 0) respuestas
        enBlanco = length $ filter (== 0) respuestas
        correctasRel = fromIntegral correctas / fromIntegral totalRespuestas
        erroneasRel = fromIntegral erroneas / fromIntegral totalRespuestas
        enBlancoRel = fromIntegral enBlanco / fromIntegral totalRespuestas
    in (correctas, erroneas, enBlanco, correctasRel, erroneasRel, enBlancoRel)

-- Determinar la pregunta con más y con menos aciertos
-- Usado ChatGPT para programar una función que determine el índice del valor máximo y mínimo
mejoresPeores :: Frecuencias -> (Int, Int)
mejoresPeores frecuencias =
    let resultados = map (\(c, _, _, _, _, _) -> c) frecuencias
        mejor = fst $ maximumBy (compare `on` snd) (zip [1..] resultados)
        peor = fst $ minimumBy (compare `on` snd) (zip [1..] resultados)
    in (mejor, peor)

-- Determinar la pregunta más y menos veces dejada en blanco
masMenosBlancos :: Frecuencias -> (Int, Int)
masMenosBlancos frecuencias =
    let blancos = map (\(_, _, b, _, _, _) -> b) frecuencias
        masBlanco = fst $ maximumBy (compare `on` snd) (zip [1..] blancos)
        menosBlanco = fst $ minimumBy (compare `on` snd) (zip [1..] blancos)
    in (masBlanco, menosBlanco)


-- Programada con ayuda de ChatGPT, para conseguir emplear getLine junto con la función read
-- Y aprendido a usar mapM_
--{-
main :: IO ()
main = do
    putStrLn "Introduce las preguntas del test ([(Float, Int, Int)]):"
    preguntasInput <- getLine
    let testPreguntas = read preguntasInput :: [Pregunta]

    putStrLn "Introduce los modelos del test ([[Int]]):"
    modelosInput <- getLine
    let testModelos = read modelosInput :: [[Int]]

    putStrLn "Introduce las respuestas de los alumnos ([String, Int, [Int]]):"
    respuestasInput <- getLine
    let respuestas = read respuestasInput :: [RespuestaTest]

    let test = (testPreguntas, testModelos)

    putStrLn "\nCorrigiendo respuestas..."
    let correcciones = map (corrige test) respuestas
    mapM_ print correcciones

    printEstadisticas (estadisticas test respuestas)
--}

-- Main de ejemplo, para ahorrarse reescribir la entrada
{-
main :: IO ()
main = do
    let test = ([(1.0, 4, 3), (1.0, 3, 1), (0.5, 5, 4), (2.0, 4, 2), (1.5, 3, 2),
            (1.0, 4, 1), (0.5, 2, 2), (1.0, 3, 3), (1.0, 4, 4), (2.0, 5, 1)], -- Test
            [[1,0,2,3,4,5,6,7,8,9], [3,1,0,4,5,9,7,6,8,2], [5,9,4,6,8,7,0,1,2,3]])  -- Modelos
    let respuestas = [("Alumno1", 1, [3,1,4,2,2,1,2,3,4,1]),
                      ("Alumno2", 2, [2,0,1,3,3,1,2,3,4,1]),
                      ("Alumno3", 1, [1,1,3,1,2,3,2,2,3,1]),
                      ("Alumno4", 3, [2,3,4,1,4,3,2,1,2,1])]
    putStrLn "Corrigiendo respuestas..."
    let correcciones = map (corrige test) respuestas
    mapM_ print correcciones
    
    printEstadisticas (estadisticas test respuestas)
-}

printEstadisticas :: Estadisticas -> IO ()
printEstadisticas (mediaNota, mediaPreguntasRespondidas, suspensos, aprobados, notables, sobresalientes, frecuencias, mejorPregunta, peorPregunta, masBlanco, menosBlanco) = do
    putStrLn "\nEstadísticas del test:"
    putStrLn $ "1. Nota media de los alumnos: " ++ show mediaNota
    putStrLn $ "2. Número medio de preguntas respondidas: " ++ show mediaPreguntasRespondidas
    putStrLn $ "3. Número de suspensos: " ++ show suspensos
    putStrLn $ "4. Número de aprobados: " ++ show aprobados
    putStrLn $ "5. Número de notables: " ++ show notables
    putStrLn $ "6. Número de sobresalientes: " ++ show sobresalientes
    putStrLn "7. Frecuencias (Correctas, Erróneas, En blanco), (Relativas correctas, Relativas erróneas, Relativas en blanco):"
    mapM_ printFrecuencia (zip [1..] frecuencias)
    putStrLn $ "8. Pregunta con mejores resultados: " ++ show mejorPregunta
    putStrLn $ "9. Pregunta con peores resultados: " ++ show peorPregunta
    putStrLn $ "10. Pregunta más veces dejada en blanco: " ++ show masBlanco
    putStrLn $ "11. Pregunta menos veces dejada en blanco: " ++ show menosBlanco

printFrecuencia :: (Int, (Int, Int, Int, Float, Float, Float)) -> IO ()
printFrecuencia (n, (c, e, b, cr, er, br)) =
    putStrLn $ "   Pregunta " ++ show n ++ 
        ": (" ++ show c ++ ", " ++ show e ++ ", " ++ show b ++ 
        ") , (" ++ show cr ++ ", " ++ show er ++ ", " ++ show br ++ ")"
