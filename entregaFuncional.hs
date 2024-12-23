{-
    Entrega Programación Funcional - Dylan Hewitt Martínez

    Primero tenemos que definir los tipos que vamos a emplear:
        - Pregunta: Guardaremos el valor de la pregunta, así como el número de opciones y la opción que sea correcta
        - Test: Guardamos una lista con las preguntas del test, así como una lista por cada modelo con una lista de índices para cada uno
        - RespuestaTest: Guardamos para cada respuesta el nombre (o DNI (String)), el índice del modelo y la respuesta que ha marcado en cada pregunta (0 si la deja en blanco)
        - Corrección: Guardaremos para cada corrección el nombre/DNI del alumno, y su nota final, tanto la total como sobre 10
        - Estadísticas: Para el total de los alumnos vamos a calcular la siguiente información:
            * Puntuación media, número medio de preguntas respondidas, número de suspensos, aprobados, notables y sobresalientes
-}

type Pregunta = (Float, Int, Int)
type Test = ([Pregunta], [[Int]])
type RespuestaTest = (String, Int, [Int])
type Correccion = (String, Float, Float) 
type Estadisticas = (Float, Float, Int, Int, Int, Int) 

corrige :: Test -> RespuestaTest -> Correccion   -- Dado un Test corrige las respuestas de RespuestaTest devolviendo una Corrección
corrige (preguntas, modelos) (nombre, modelo, respuestas) =
    let ordenPreguntas = modelos !! (modelo - 1) -- Sacamos el orden de las preguntas para el modelo del alumno
        preguntasOrdenadas = map (preguntas !!) ordenPreguntas -- Ordenamos las preguntas del test para que concuerden con las del alumno
        puntuacionTotal = sum (zipWith (calculaPuntuacion) preguntasOrdenadas respuestas) -- Usamos calculaPuntuación para calcular cuánto suma/resta cada pregunta
        puntuacionSobre10 = (puntuacionTotal / sum (map (\(valor, _, _) -> valor) preguntas)) * 10  -- Dividimos la puntuación total obtenida entre el valor máximo obtenible
    in (nombre, puntuacionTotal, puntuacionSobre10)

calculaPuntuacion :: (Float, Int, Int) -> Int -> Float
calculaPuntuacion (valor, nAlternativas, correcta) respuesta
    | respuesta == 0       = 0       -- Respuesta en blanco
    | respuesta == correcta = valor  -- Respuesta correcta
    | otherwise            = - (valor / fromIntegral (nAlternativas - 1)) -- Respuesta incorrecta

estadisticas :: Test -> [RespuestaTest] -> Estadisticas
estadisticas test respuestas =
    let correcciones = map (corrige test) respuestas  -- Vamos primero a corregir las respuestas con la función previamente definida
        notas = map (\(_, _, nota) -> nota) correcciones  -- Obtenemos la nota sobre 10 para cada una de ellas (no nos interesa el nombre)
        --totalPreguntas = length (fst test)  -- Obtenemos el número total de preguntas
        mediaNota = sum notas / fromIntegral (length notas)  -- Calculamos la nota media
        preguntasRespondidas = map (\(_, _, reps) -> length ( filter (/= 0) reps)) respuestas -- Contamos cuantas preguntas se han contestado (distinto de 0) en total
        mediaPreguntasRespondidas = sum (map fromIntegral preguntasRespondidas) / fromIntegral (length preguntasRespondidas) -- Calculamos la media de preguntas respondidas
        -- Calculamos ahora cuantos alumnos hay en cada tramo: suspenso / aprobado / notable / sobresaliente
        suspensos = length $ filter (< 5) notas
        aprobados = length $ filter (\x -> x >= 5 && x < 7) notas
        notables = length $ filter (\x -> x >= 7 && x < 9) notas
        sobresalientes = length $ filter (>= 9) notas
    in (mediaNota, mediaPreguntasRespondidas, suspensos, aprobados, notables, sobresalientes)

-- Main de ejemplo
main :: IO ()
main = do
    let test = ([ (1.0, 3, 1), (0.5, 4, 4), (1.0, 3, 2), (0.5, 2, 2) ], -- Preguntas
                 [[2,1,0,3], [0,2,3,1], [3,2,1,0]])                     -- Modelos
    let respuestas = [("Juan", 3, [2, 0, 3, 1]), ("Maria", 1, [3, 4, 1, 2])]
    putStrLn "Corrigiendo respuestas..."
    mapM_ print (map (corrige test) respuestas)
    putStrLn "Calculando estadísticas..."
    print (estadisticas test respuestas)
