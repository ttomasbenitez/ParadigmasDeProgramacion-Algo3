[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/p1IUhq17)
# Mars Rover

Somos parte del equipo que desarrolla los equipos de exploración remota de Marte en la NASA y tenemos que desarrollar el sistema que controle el Mars Rover. 

Para eso, se asume que la superficie de Marte es un plano y que se usan puntos para posicionar al Mars Rover en dicho plano, más un punto cardinal que indica hacia donde apunta. Debido a que Marte está muy lejos, siempre se le envían al Mars Rover un conjunto de comandos empaquetados en un String, donde cada carácter es un comando. 

Tener en cuenta que la comunicación puede tener problemas y pueden llegar comandos erróneos en cuyo caso se espera que no se sigan procesando los comandos restantes. 

Tener en cuenta que: 
- El Mars Rover siempre empieza en un punto inicial (x,y) y apuntando a un punto cardinal (N,S,E,O) 
- El Rover recibe una secuencia de caracteres que representan comandos sobre cómo moverse. 
- Los comandos pueden ser: 
  - f = mover hacia adelante un punto (forward) 
  - b = mover hacia atrás un punto (backwards) 
  - l = rotar 90 grados a la izquierda 
  - r = rotar 90 grados a la derecha 

Resolverlo por medio de TDD.


# Tips 
En smalltalk ya existen los puntos en el plano, bajo la clase **Point**. Podemos crear instancias de **Point** con el mensaje `@`. Por ejemplo `0@0`.
