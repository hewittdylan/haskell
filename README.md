Práctica entregable de programación funcional para la asignatura de Programación Declarativa

Objetivo de la práctica: ayuda a la corrección de tests de respuestas alternativas.

Descripción: el objetivo de la práctica es definir algunas funciones Haskell que ayuden a corregir exámenes de test de respuestas alternativas, de acuerdo con las siguientes indicaciones:

• Como solo nos interesa la corrección, podemos ignorar el texto concreto de las preguntas del test.
Lo único que nos interesa manejar de un test es:

• El valor y el número de alternativas de cada pregunta (no tendrían por qué ser los mismos
para todas las preguntas).

• La respuesta correcta en cada pregunta.

• Los diferentes modelos del test, que consisten simplemente en reordenaciones de las preguntas, y que se identifican por un número.

• La respuesta de un alumno a un test viene determinada por tres datos:

• Identificador del alumno (por ejemplo, su nombre, o su DNI).

• Modelo de test al que se está respondiendo.

• Respuestas dadas a las preguntas (puede incluir respuestas en blanco).

• Para calcular la nota de un test hay que tener en cuenta que cada respuesta errónea puntúa como
-1/(N- 1), siendo N el número de alternativas a la pregunta en cuestión.
