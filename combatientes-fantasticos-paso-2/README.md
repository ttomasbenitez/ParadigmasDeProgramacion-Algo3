### Enunciado
Ahora que sabemos modelar a combatientes, que los podemos equipar y que pueden atacar con algún tipo de estrategia, queremos ir un paso más allá y empezar a modelar cómo combaten entre sí.
Pero también necesitamos agregar algunas funcionalidades necesarias para poder modelar estos combates y algunas funcionalidades extra que enriquecen y detallan más cómo funcionan los ataques.
Por último vamos a querer tener código que demuestre una propiedad que tienen las estrategias en nuestro sistema.
Ataques con más detalle
Vamos a agregar dos funcionalidades a los ataques: bonificaciones al daño y a la defensa y condición de distraído.
Bonificaciones al daño y a la defensa
Vamos a agregarle a los combatientes que puedan tener un bonificador de fuerza y un bonificador de agilidad.
El bonificador de fuerza se suma al daño del ataque.
El bonificador de agilidad se suma al daño que se absorbe.
Condición de distraído
Cuando alguien es atacado y no sufre daño (porque fue totalmente absorbido) el ataque distrae al atacado. Es decir, el atacado queda distraído.
Cuando se ataca a alguien distraído se le agrega 2 al daño de ese ataque.
Si el ataque logra dañarlo, deja de estar distraído (“lo despiertan de un bofetazo”).
Si el ataque no logra dañarlo, continúa distraído.
Fuera de combate
Antes de poder modelar un combate, necesitamos que un combatiente lo suficientemente herido deje de participar del mismo.
Para eso vamos a implementar dos funcionalidades:
Que cuando un combatiente #atacaA:, si está fuera de combate (es decir, si tiene vida menor o igual a cero), no hace nada
Que cuando un combatiente #ataca, no considere a enemigos que tienen vida menor o igual a cero
Que los combatientes combatan
Para que los combatientes puedan combatir vamos a crear un objeto que sepa llevar a cabo esta tarea: `OrquestadorDeCombates`. Le vamos a definir al `OrquestadorDeCombates` dos bandos y le vamos a pedir que desarrolle el combate con una cantidad máxima de rondas.

Como un combate puede durar muchísimo tiempo e incluso ser infinito, le vamos a dar este máximo de rondas de duración para que pueda cortar si se alarga mucho.

Luego de que el combate se desarrolle y finalice, queremos saber quién es el ganador y cuántas rondas duró (más adelante se explica qué es una ronda).
El ganador puede ser alguno de estos resultados:
Ganó el bando 1
Ganó el bando 2
Indeterminado	

Un bando gana cuando al menos un miembro de ese grupo tiene puntos de vida mayor a cero y el bando enemigo tiene a todos sus miembros con puntos de vida menor o igual a cero. Indeterminado es cuando estas condiciones no suceden porque el combate no tuvo suficiente tiempo para llegar a un ganador (porque le dimos poco tiempo o porque el combate dura infinito)

El resultado de un combate es una especie de informe que nos da una idea de qué sucedió y cómo.
Por el momento, luego de que se desarrolla un combate, lo que nos interesa de éste es saber dos cosas: quién ganó (o si no ganó nadie “indeterminado”) y cuántas rondas duró el combate (es decir, en cuántas ronda ganó el ganador, o durante cuántas rondas se desarrolló el combate sin que se produzca un ganador).
Cómo se desarrolla un combate
Un combate se desarrolla en rondas.
En cada ronda cada combatiente tiene la posibilidad de atacar (es decir, se le envía el mensaje #atacar). Es decir, todos los combatientes de todos los bandos reciben el mensaje #atacar una vez por ronda.
Una vez terminada la ronda, se vuelve a hacer otra y así sucesivamente hasta terminar el combate con alguna de las condiciones anteriormente mencionadas: o ganó uno de los dos bandos o pasó mucho tiempo.
El orden en que los combatientes reciben el mensaje #atacar si bien es importante y afecta el desarrollo del combate, no nos va a importar definirlo en esta etapa.
Hay estrategias mejores y peores
Por último queremos constatar que este sistema de combate exhibe una propiedad: que atacar al más herido es mejor que atacar al más sano. Y tenemos un test que lo prueba.
Este test les debería funcionar si todos los demás funcionan.
### Preguntas
¿Qué crítica le harías al protocolo de #estaHerido y #noEstaHerido? (en vez de tener solo el mensaje #estaHerido y hacer “#estaHerido not” para saber la negación)
¿Qué opinan de que para algunas funcionalidades tenemos 3 tests para el mismo comportamiento pero aplicando a cada uno de los combatientes (Arthas, Mankrik y Olgra)
¿Cómo modelaron el resultado de haber desarrollado un combate? ¿qué opciones consideraron y por qué se quedaron con la que entregaron y por qué descartaron a las otras?

Contestar estas preguntas en un archivo .txt e incluirlas en la entrega junto al .st.

Realización del ejercicio y qué evaluamos
Recomendamos ir haciendo andar los tests provistos de a uno a la vez. En el orden numérico propuesto. Algunos tests cuando los solucionen van a hacer andar no uno, sino varios a la vez. Especialmente los primeros. Eso está bien que así sea.
Hacer andar los tests de la categoría “fuerza y agilidad”, donde implementamos bonificadores al daño y a la armadura
Importante en esta parte es si reifican conceptos y utilizan buenos nombres.
Hacer andar los tests de la categoría “fuera de combate”, donde implementamos esa funcionalidad
Importante es cómo organizan el código y que sea claro.
Hacer andar los tests de la categoría “condición de distraído” donde implementamos esa funcionalidad
Importante es cómo organizan el código y que sea claro.
Extra: intentar quitar el código repetido en Mankrik entre `#estaHerido` y  `#noEstaHerido`. 
Hacer andar los tests de `OrquestadorDeCombatesTest` donde implementamos el objeto que va a desarrollar combates entre dos bandos
Acá hay tests que fallan porque hay mensajes sin implementar (como hicimos en el ejercicio anterior). Están en la categoría “para completar”. Importante, deben estar implementados correctamente, en caso contrario no sumarán puntos sobre los respectivos ítems.
Esta es la parte principal del ejercicio: cómo modelar al objeto que va a ser capaz de desarrollar combates.
Por último, llegamos a la estrella: el test que prueba una propiedad de todo este sistema: que hay una estrategia que es mejor que la otra
Este test les debería funcionar de una, no deberían tener que hacer nada. Porque está basado en los 5 items anteriores.

### Tips
No les vamos a dar tips. Así preguntan y se ayudan entre uds.

Bueno, si, les damos un tip: hablen entre uds, ayúdense, discutan. No está prohibido cooperar entre los grupos.

Bueno, pero en realidad hay algunas cosas que tal vez quieran que usar:

La coma `,` es un mensaje que se le puede enviar a una colección (los String también son colecciones) y como argumento va otra colección. Responde una nueva colección con ambas concatenadas.
Ejemplo: `#(1 2), #(3 4)` responde `#(1 2 3 4)`

`select:` recorre la colección y arma una nueva solamente con los elementos que respondieron True al evaluarse el bloque de código (le envia el elemento que está recorriendo como argumento)

`reject:` recorre la colección y arma una nueva con los elementos que no evalúan True en el bloque de código (al revés del mensaje anterior, select)

`1 to: 10 do: [ :indice | … ]` este mensaje evalúa el bloque 10 veces, enviandole el numero por el que va iterando como argumento.

`[...] whileTrue: [...]` los bloques de código entienden whileTrue:. El primer bloque debería devolver un booleano y si es verdadero, evalúa el bloque pasado como argumento y vuelve a evaluar el bloque inicial a ver si sigue dando True. Una vez que da False, el método termina.

Para crear denotative objects programáticamente (y eliminarlos):

Crear un objeto que tenga como padre a Serge 
`Serge createChildNamed: ‘Juan’, Serge nextCloneNumber asString.`
La última parte “nextCloneNumber” es para que tenga un nombre diferente: Juan1, Juan2, etc. Porque no se pueden crear denotative objects con el mismo nombre.

Pedirle todos los “hijos” a Serge y borrarlos.
`Serge children do: [ :each | each removeFromSystem ]`
