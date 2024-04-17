[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/8ZFO2SdJ)
# Algo 3 - Ejercicio de Git

## Sobre este ejercicio
Este ejercicio está pensado para que aprendan los conceptos básicos del manejo de repositorios git. En el archivo `gitreadme.md` pueden encontrar más información al respecto y los pasos que deben seguir para entregar este ejercicio y los que siguen, por lo que debe ser leido en su totalidad.

Recomendamos que usen el canal #git de discord para hacer consultas al respecto!

## Enunciado

Ahora que sabemos modelar a combatientes, que los podemos equipar y que pueden atacar con algún tipo de estrategia, pero en el siguiente ejercicio vamos a ir un poco más allá. Sin embargo, parece que hubo alguien de la cátedra que metió mano a nuestro trabajo anterior.
Quiso mejorar la expresividad de los tests metiendo algunos conceptos en mensajes, pero si bien los creó y los nombró, el muy irresponsable no los implementó. Así que eso es algo que vamos a tener que corregir.

Una de las mejoras a nuestro código anterior es que se modelaron las unidades de puntos de vida. Los puntos de vida no son un número, son una unidad de medida. Por eso van a ver código como: `20 * pv`. Eso instancia un objeto que representa 20 puntos de vida.

Por un tema de precedencia de mensajes en Smalltalk, recuerden que si tienen que sumar dos cantidades tienen que usar paréntesis en la segunda: `5 * pv + (1 * pv)`

Nuestra misión es volver a tener los tests de CombatientesTest pasando, para ello deben hacer file-in del código inicial del archivo CombatientesFantanticosPreludio y hacer andar los 11 tests de la suite (categorías “combatientes” y “atacar”) implementando mensajes para saber cuán herido o sano está un combatiente. No deben crear objetos nuevos para este ejercicio.

Por último, como este es un ejercicio de git, lo importante es que aprendan el proceso que van a usar durante el cuatrimestre y muy probablemente en sus trabajos, por lo que es vital que participen todos los integrantes del grupo y hagan al menos un commit cada uno. 
