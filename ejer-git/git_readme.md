# Introducción rápida a Git y Github

## Parte 1: Que es Git

![XKCD: Git](https://imgs.xkcd.com/comics/git.png)

Git es un Sistema de Control de Versiones Distribuido (DVCS) diseñado por Linus Torvalds en 2005. Es utilizado para guardar diferentes versiones de un archivo (o conjunto de archivos) para que cualquier versión sea recuperable cuando se desee, lo cual ayuda a los equipos de desarrollo a gestionar los cambios en el codigo fuente a lo largo del tiempo.

Que sea distribuido, significa que en lugar de tener un único espacio para todo el historial de versiones del software, en Git, la copia de trabajo del código de cada desarrollador es también un repositorio que puede albergar el historial completo de todos los cambios.

Veamoslo con más detenimiento como funciona git...

> Nota: A partir de este momento voy a asumir que ya tenes instalado git. Son programadores, ustedes pueden XD.

## Parte 2: Repositorios locales, commits

Git trabaja con la idea de repositorios. Un repositorio es el lugar donde se almacenan los archivos y su historial de cambios. Uno puede pensar en un repositorio como una carpeta con archivos, la parte del historial de cambios lo maneja internamente Git.

Un **repositorio local** es aquel que se guarda en la computadora del prorgamador. Los repositorios locales son útiles para trabajar en proyectos cuando no tenés una conección de internet, o para hacer cambios sin afectar el repositorio principal hasta que estes listo.

Para crear un repositorio local desde cero, se debe crear una carpeta, y luego inicializar git.

```bash
# Creamos una nueva carpeta
mkdir mi_nuevo_repositorio

# Cambiamos a ese nuevo directorio
cd ./mi_nuevo_repositorio

# Inicializamos el repositorio 
git init
```

Git piensa acerca de su data como una serie de fotogramas. Cuando uno quiere subir o modificar uno o varios archivos, debe sacar el fotograma del estado actual del repositorio. Eso es lo que se conoce como hacer un **commit**.

Para cada archivo en un commit, git guarda los cambios realizados a un archivo, o una referencia al nodo del commit anterior en caso de que no hubiera cambios:

![](https://git-scm.com/book/en/v2/images/snapshots.png)

Para modificar archivos en un repositorio, primero se deben añadir los archivos que se desean modificar/agregar/eliminar a lo que se conoce como la "Staging Area". Los estados de un archivo en git se resumen en la siguiente imagen:

![El ciclo de vida del estado de tus archivos](https://git-scm.com/book/en/v2/images/lifecycle.png)

Para mover un archivo modificado al Staging Area, se utiliza el siguiente comando:

```bash
# Si se desean agregar archivos puntuales
git add 'archivo_1' 'archivo_2' 'archivo_3' ...

# Si se desean agregar todos los archivos
git add .
```

Una vez que todos los archivos que se desean agregar al **commit** están listos, se procede a hacer:

```bash
# Realizar un commit con todos los elementos del Staging Area
git commit -m 'Mensaje que explique que cambios se realizaron'
```

En este punto, todos los cambios realizados ya fueron guardados en un nuevo commit.

## Parte 3: Repositorios remotos, Github y `git clone`

En la parte anterior vimos los repositorios locales, y como estos se parecian a una carpeta donde para guardar cambios uno hace **commits**. Un repositorio remoto no es muy diferente, solo que no se encuentra en la computadora del programador, sino en un servidor externo.

Un repositorio externo, es un repositorio almacenado en un servidor remoto, como Github. Los repositorios remotos permiten almacenar tu codigo en la nube, acceder a el desde cualquier lugar con una conección de internet, colaborar con otras personas y usar características como el rastreo de problemas (issues), los pull requests y la integración continua (en esta guía no ahondaremos mucho en estos temas, pero esta bueno que sepan que existen). Tambien son útiles como backup del código en caso de que la copia local se pierda o sea dañada o corrompida.

Github es un sitio creado para alojar el código de las aplicaciones de cualquier desarrollador utilizando Git. Fue comprada por Microsoft en 2018.

De hecho, seguramente estes leyendo esto desde Github jeje.

Trabajar con un repositorio remoto no es muy diferente a uno local. Para crear un repositorio remoto en Github, pueden [leer mas aquí](https://docs.github.com/es/get-started/quickstart/create-a-repo).

Una vez que ya tengan un repositorio en Github, para trabajar en el, primero deben crear una copia local en su computadora. `git clone` se encargara de copiar el repositorio a la maquina local, incluyendo todos los archivos, branches (veremos que es esto más adelante) y la historia de commits.

```bash
# Moverse al lugar donde clonaran el repositorio
cd directorio_donde_clonar

# Clonar el repo
git clone 'url_del_repositorio'
```

Una vez que se ha clonado el repositorio, se procede a trabajar como si se tratase de un repositorio local. Se procede a hacer cambios, una vez listos se pasan al staging area y para sellar todo se crea el commit.

Una vez que estamos gustosos con los nuevos commits realizados, hay que mandar los cambios al repositorio remoto. Para eso, existe el commando `git push`:

```bash
# La primera vez git te pide que seas mas especifico
git push -u origin master

# Luego de haber corrido una vez el commando con "-u", con esto basta
git push
```

Ahora, al trabajar con repositorios remotos, hay que tener en cuenta que otras personas tambien pueden estar trabajando en el y pusheando commits, lo cual puede hacer que nuestra copia local quede "desactualizada".

Para traer los ultimos cambios del repositorio, se usa el comando:

```bash
git pull
```

# El resumen

En esta guia aprendieron los conocimientos minimos de git y github que van a necesitar para la materia. Hay mucha mas profundidad en esto, pero no queremos marearlos.

Para prevenir esto, les dejamos una especie de cheatsheet de como trabajar en esta materia.

> :warning: **Para los que ya saben de git**: En esta materia solo utilizaremos un branch (main o master, elijan ustedes). La razon es que resolver conflictos de merge es bastante mas dificil en smalltalk que en otros lenguajes.

> :warning: **Para los que NO saben de git**: Todos sus commits quedan registrados, al igual que sus versiones. Si bien no vamos a hacer mucho uso de eso a lo largo de la materia, es un recordatorio para que no agreguen claves privadas a un repositorio, ni fotos con contenidos ajenos a la materia, porque ninguna de las dos podran ser borradas una vez pusheado el commit.

Por lo general, como primer paso todos los integrantes clonaran el repo de github usando `git clone`.

Eso se hará una sola vez por ejercicio.

Luego, los lineamientos generales es que cada vez que se pongan a trabajar, primero hacer un `git pull`.

Despues hacen filein de la imagen, resuelven el ejercicio, hacen file out y tiran el archivo nuevo dentro de la carpeta del repositorio.

Preparan el commit, y una vez hecho, se hace `git push` para subir los cambios a github.

En resumen:

```bash
# Una sola vez por ejercicio
git clone 'url_de_github'

# Cada vez que se sienten a hacer el ejercicio,
# por si llegara a haber cambios en el repo
git pull

# Hacen file in... resuelven el ejercicio o
# hacen algun avance...

git add .
git commit -m 'un resumen de que hicieron'

# Cuando ya tienen los commits como los quieren
# subimos los cambios a github
git push
```

Si siguen estos pasos, no deberian tener practicamente problemas al momento de usar github. Esperamos que les haya gustado, y cualquier duda pueden preguntar por el canal de `#git` de discord.

Exitos con los ejercicios!

![](https://media.istockphoto.com/id/1153678999/pt/vetorial/the-end-handwrite-title-on-red-round-bacground-old-movie-ending-screen-vector-illustration.jpg?s=170667a&w=0&k=20&c=WUYhB36jYuNDJ4HY8OIbJTGVVF_P18N-y1YAuXeVo-c=)
