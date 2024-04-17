[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/bsawMXWq)
# Stack

## Preludio

Esta parte es opcional pero altamente recomendada.

Como preludio de este ejercicio deberán implementar un stack y testearlo. Para ello, partirán del código inicial del archivo Stack-Preludio.st 

Nuestros desarrolladores se fueron de vacaciones y dejaron las cosas por la mitad, tenemos un solo test... ¡Y FALLA!

Por lo menos armaron una lista de las cosas que querían probar:
- que se pueda agregar un elemento (push)
- que se pueda sacar un elemento (pop)
- que el pop devuelve el último elemento
- que el stack es LIFO (el último que agregas, sale primero) 
- que se puede ver el último elemento sin removerlo

La recomendación será hacer de a un test y hacerlo pasar de la forma más sencilla, usando ifs.


## Primera Parte


Vamos a empezar de nuevo. Primero, hagan un file out del paquete que ya tenian y guárdenlo, habrá que incluírlo en la entrega. Luego, borren por completo el paquete Stack-Preludio (la primera de las columnas del browser).

Hacer file in del archivo Stack-Exercise.st.

Se encontrarán con algunos tests que fallan, tendrán que reimplementar el stack a partir de ellos. 

Les dejamos algunas ayudas: 

1. Primero hagan pasar todos los tests usando if y después aplique la técnica para sacar if que vimos. 
2. Si les sirve, utilicen una metáfora. Una muy útil es la de representar el juego de los bebés donde se apilan platos en una especie de torre de Hanoi.
Importante: Tampoco se puede usar handleo de excepciones para ocultar lo que sería en definitiva un if.


## Segunda Parte


El stack de la primera parte es utilizado para almacenar oraciones de cualquier longitud. Se debe implementar el mensaje find de SentenceFinderByPrefix que dado un prefijo se encarga de devolver todas las oraciones almacenadas en el Stack que contengan dicho prefijo. Por ej. si el prefijo es "Wint", y las oraciones en el Stack son "winter is coming", "winning is everything", "The winds of Winter" y "Winter is here" sólo debería devolver la última. 

El prefijo es case-sensitive, no puede ser vacío, ni contener espacios vacíos y el stack al terminar la operación de búsqueda debe de mantener las mismas oraciones en idéntico orden. 

Además de implementar "find", se debe testear dicha funcionalidad. Para ello escriba todos los tests que crea necesario en SentenceFinderByPrefixTest.

Aclaración: No se pueden agregar mensajes adicionales al Stack en esta parte.


## Extra


Se pide extender el modelo para que además de representar al stack ilimitado ya construido, se puedan construir instancias de un stack limitado. Es decir, uno de que tenga una cantidad limitada de celdas y que no se puedan pushear más elementos que los disponibles en su capacidad.

Se pide además analizar cuál de los modelos descriptos en clase creen que es más sencillo extender para representarla y hacerlo. 

Además se deberán agregar los casos de tests que hagan falta para probar el nuevo tipo de stack.

Aclaración: Pueden agregar mensajes adicionales al Stack en esta parte.
