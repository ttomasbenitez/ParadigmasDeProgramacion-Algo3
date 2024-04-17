!classDefinition: #CombatienteHumanoArthas category: 'CombatientesFantasticosPaso2'!
DenotativeObject subclass: #CombatienteHumanoArthas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CombatientesFantasticosPaso2'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CombatienteHumanoArthas class' category: 'CombatientesFantasticosPaso2'!
CombatienteHumanoArthas class
	instanceVariableNames: 'enemigos puntosDeVida dañoAInfligir dañoAAbsorber estrategia bonificadorDeAgilidad'!

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:05:04'!
atacar
	| enemigoElegido |
	self estaFueraDeCombate ifTrue: [^self].
	enemigos isEmpty ifTrue: [^self].
	enemigoElegido := estrategia value: (enemigos reject: [ :enemigo | enemigo estaFueraDeCombate ]).
	self atacarA: enemigoElegido.! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/15/2023 13:12:49'!
atacarA: unCombatiente 
	self estaFueraDeCombate ifTrue: [^self].
	unCombatiente recibirDaño: dañoAInfligir.! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'pal 8/23/2022 12:59:21'!
enemigos: unConjuntoDeEnemigos
	enemigos := unConjuntoDeEnemigos! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/15/2023 13:19:36'!
estaFueraDeCombate
	^ puntosDeVida <= (0*pv)! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/15/2023 12:57:36'!
estaHerido
	^ puntosDeVida < (20*pv).! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'JMB 9/11/2022 12:05:22'!
focalizarEnElMasHerido
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar detectMin: [:enemigo | enemigo puntosDeVida]].! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'JMB 9/11/2022 12:05:32'!
focalizarEnElMasSano
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar detectMax: [:enemigo | enemigo puntosDeVida]].! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:30:26'!
noEstaHerido

	^ puntosDeVida >= (20*pv)! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/16/2023 19:59:55'!
prepararseParaElCombate
	puntosDeVida := 20*pv.
	dañoAInfligir := 2*pv.
	dañoAAbsorber := 0*pv.
	enemigos := #().
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar anyOne ].
	
	bonificadorDeAgilidad := 1.! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'pal 8/23/2022 23:49:24'!
puntosDeVida
	^puntosDeVida! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:00:54'!
recibirDaño:  unaCantidadDeDaño
	| dañoADescontar |
	dañoADescontar := (unaCantidadDeDaño - (dañoAAbsorber + (bonificadorDeAgilidad *pv))) max: 0*pv.
	puntosDeVida := puntosDeVida - dañoADescontar.! !

!CombatienteHumanoArthas class methodsFor: 'combate' stamp: 'sv 4/15/2023 12:52:29'!
tienePuntosDeVida: posiblesPuntosDeVida
	^ puntosDeVida = posiblesPuntosDeVida.! !


!CombatienteHumanoArthas class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:13:56'!
equiparConArmaduraDeCuero
	dañoAAbsorber := 2*pv.! !

!CombatienteHumanoArthas class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:09'!
equiparConArmaduraDeMetal
	dañoAAbsorber := 6*pv.! !

!CombatienteHumanoArthas class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:22'!
equiparEspada
	dañoAInfligir := 8*pv.! !

!CombatienteHumanoArthas class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:34'!
equiparHacha
	dañoAInfligir := 10*pv.! !


!CombatienteHumanoArthas class methodsFor: '--** private fileout/in **--' stamp: 'sv 4/16/2023 20:36:15'!
initializeCollaboratorsFromFile
	"Auto generated method for loading purposes - Do not modify it"

	enemigos := #().
	puntosDeVida := (SimpleMeasure basicNew instVarAt: 1 put: 20; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	dañoAInfligir := (SimpleMeasure basicNew instVarAt: 1 put: 2; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	dañoAAbsorber := (SimpleMeasure basicNew instVarAt: 1 put: 0; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	estrategia := ((SerializableBlockClosure basicNew instVarAt: 1 put: CombatienteHumanoArthas; instVarAt: 2 put: '[ :argm4_1 |
	argm4_1 anyOne. ]'; instVarAt: 3 put: #(); yourself) asEvaluable).
	bonificadorDeAgilidad := 1.! !


!classDefinition: #CombatienteOrcoMankrik category: 'CombatientesFantasticosPaso2'!
DenotativeObject subclass: #CombatienteOrcoMankrik
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CombatientesFantasticosPaso2'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CombatienteOrcoMankrik class' category: 'CombatientesFantasticosPaso2'!
CombatienteOrcoMankrik class
	instanceVariableNames: 'bonificardorDeFuerza enemigos puntosDeVida dañoAInfligir dañoAAbsorber estrategia dañoPorEstarDistraido'!

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:05:19'!
atacar
	| enemigoElegido |
	self estaFueraDeCombate ifTrue: [^self].
	enemigos isEmpty ifTrue: [^self].
	enemigoElegido := estrategia value: (enemigos reject: [ :enemigo | enemigo estaFueraDeCombate ]).
	self atacarA: enemigoElegido.! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:01:04'!
atacarA: unCombatiente 
	
	self estaFueraDeCombate ifTrue: [^self].
	unCombatiente recibirDaño: dañoAInfligir + (bonificardorDeFuerza *pv).! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:21:56'!
despertarse
	dañoPorEstarDistraido := 0*pv.! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:21:34'!
distraerse
	dañoPorEstarDistraido := 2*pv.! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'pal 8/23/2022 23:51:54'!
enemigos: unConjuntoDeEnemigos
	enemigos := unConjuntoDeEnemigos! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/15/2023 13:19:40'!
estaFueraDeCombate
	^ puntosDeVida <= (0*pv)! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'JMB 4/12/2023 21:53:12'!
estaHerido
	^ puntosDeVida < (20*pv)! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'JMB 9/11/2022 12:03:12'!
focalizarEnElMasHerido
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar detectMin: [:enemigo | enemigo puntosDeVida]].! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'JMB 9/11/2022 12:03:31'!
focalizarEnElMasSano
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar detectMax: [:enemigo | enemigo puntosDeVida]].! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/16/2023 19:55:58'!
noEstaHerido
	^ self estaHerido not. "Para no repetir codigo"! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:00:02'!
prepararseParaElCombate
	puntosDeVida := 20*pv.
	dañoAInfligir := 2*pv.
	dañoAAbsorber := 0*pv.
	enemigos := #().
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar anyOne ].
	
	bonificardorDeFuerza := 2.! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'pal 8/23/2022 23:51:54'!
puntosDeVida
	^puntosDeVida! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:25:24'!
recibirDaño:  unaCantidadDeDaño
	| dañoADescontar |

	dañoADescontar := (unaCantidadDeDaño - dañoAAbsorber) max: 0*pv.

	(dañoADescontar <= 0) ifTrue: [ self distraerse ].
	(dañoADescontar <= 0) ifFalse: [
		dañoADescontar  := dañoADescontar + dañoPorEstarDistraido.
		self despertarse.
	].

	puntosDeVida := puntosDeVida - dañoADescontar.! !

!CombatienteOrcoMankrik class methodsFor: 'combate' stamp: 'sv 4/15/2023 12:41:00'!
tienePuntosDeVida: posiblesPuntosDeVida
	^ puntosDeVida = posiblesPuntosDeVida.! !


!CombatienteOrcoMankrik class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:13:56'!
equiparConArmaduraDeCuero
	dañoAAbsorber := 2*pv.! !

!CombatienteOrcoMankrik class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:09'!
equiparConArmaduraDeMetal
	dañoAAbsorber := 6*pv.! !

!CombatienteOrcoMankrik class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:22'!
equiparEspada
	dañoAInfligir := 8*pv.! !

!CombatienteOrcoMankrik class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:34'!
equiparHacha
	dañoAInfligir := 10*pv.! !


!CombatienteOrcoMankrik class methodsFor: '--** private fileout/in **--' stamp: 'sv 4/16/2023 20:36:15'!
initializeCollaboratorsFromFile
	"Auto generated method for loading purposes - Do not modify it"

	bonificardorDeFuerza := 2.
	enemigos := #().
	puntosDeVida := (SimpleMeasure basicNew instVarAt: 1 put: 20; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	dañoAInfligir := (SimpleMeasure basicNew instVarAt: 1 put: 2; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	dañoAAbsorber := (SimpleMeasure basicNew instVarAt: 1 put: 2; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	estrategia := ((SerializableBlockClosure basicNew instVarAt: 1 put: CombatienteOrcoMankrik; instVarAt: 2 put: '[ :argm4_1 |
	argm4_1 anyOne. ]'; instVarAt: 3 put: #(); yourself) asEvaluable).
	dañoPorEstarDistraido := (SimpleMeasure basicNew instVarAt: 1 put: 2; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).! !


!classDefinition: #CombatienteOrcoOlgra category: 'CombatientesFantasticosPaso2'!
DenotativeObject subclass: #CombatienteOrcoOlgra
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CombatientesFantasticosPaso2'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CombatienteOrcoOlgra class' category: 'CombatientesFantasticosPaso2'!
CombatienteOrcoOlgra class
	instanceVariableNames: 'distraido enemigos puntosDeVida dañoAInfligir dañoAAbsorber estrategia dañoPorEstarDistraido bonificadorDeFuerza'!

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:05:29'!
atacar
	| enemigoElegido |
	self estaFueraDeCombate ifTrue: [^self].
	enemigos isEmpty ifTrue: [^self].
	enemigoElegido := estrategia value: (enemigos reject: [ :enemigo | enemigo estaFueraDeCombate ]).
	self atacarA: enemigoElegido.! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:01:10'!
atacarA: unCombatiente 
	
	self estaFueraDeCombate ifTrue: [^self].
	unCombatiente recibirDaño: dañoAInfligir + (bonificadorDeFuerza *pv).! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:16:34'!
despertarse
	dañoPorEstarDistraido := 0*pv.! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:14:31'!
distraerse
	dañoPorEstarDistraido := 2*pv.! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'pal 8/23/2022 23:50:14'!
enemigos: unConjuntoDeEnemigos
	enemigos := unConjuntoDeEnemigos! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/15/2023 13:19:44'!
estaFueraDeCombate
	^ puntosDeVida <= (0*pv)! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/16/2023 19:49:56'!
estaHerido

	^ puntosDeVida < (20*pv).! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'JMB 9/11/2022 12:01:25'!
focalizarEnElMasHerido
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar detectMin: [:enemigo | enemigo puntosDeVida]].! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'JMB 9/11/2022 12:01:39'!
focalizarEnElMasSano
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar detectMax: [:enemigo | enemigo puntosDeVida]].! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:30:42'!
noEstaHerido
	^ puntosDeVida >= (20*pv).! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:00:33'!
prepararseParaElCombate
	puntosDeVida := 20*pv.
	dañoAInfligir := 2*pv.
	dañoAAbsorber := 0*pv.
	enemigos := #().
	estrategia := [:posiblesEnemigosAAtacar | posiblesEnemigosAAtacar anyOne ].
	
	bonificadorDeFuerza := 1.
	
	dañoPorEstarDistraido := 0*pv.! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'pal 8/23/2022 23:50:14'!
puntosDeVida
	^puntosDeVida! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'sv 4/15/2023 15:23:30'!
recibirDaño:  unaCantidadDeDaño
	| dañoADescontar |
	dañoADescontar := (unaCantidadDeDaño - dañoAAbsorber) max: 0*pv.
	(dañoADescontar <= 0) ifTrue: [ self distraerse ].
	(dañoADescontar <= 0) ifFalse: [
		dañoADescontar  := dañoADescontar + dañoPorEstarDistraido.
		self despertarse.
	].

	puntosDeVida := puntosDeVida - dañoADescontar.! !

!CombatienteOrcoOlgra class methodsFor: 'combate' stamp: 'JMB 4/11/2023 22:15:45'!
tienePuntosDeVida: unaCantidadDePuntosDeVida 
	^ puntosDeVida = unaCantidadDePuntosDeVida ! !


!CombatienteOrcoOlgra class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:13:56'!
equiparConArmaduraDeCuero
	dañoAAbsorber := 2*pv.! !

!CombatienteOrcoOlgra class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:09'!
equiparConArmaduraDeMetal
	dañoAAbsorber := 6*pv.! !

!CombatienteOrcoOlgra class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:22'!
equiparEspada
	dañoAInfligir := 8*pv.! !

!CombatienteOrcoOlgra class methodsFor: 'equipo' stamp: 'JMB 4/8/2023 00:14:34'!
equiparHacha
	dañoAInfligir := 10*pv.! !


!CombatienteOrcoOlgra class methodsFor: '--** private fileout/in **--' stamp: 'sv 4/16/2023 20:36:16'!
initializeCollaboratorsFromFile
	"Auto generated method for loading purposes - Do not modify it"

	distraido := nil.
	enemigos := #().
	puntosDeVida := (SimpleMeasure basicNew instVarAt: 1 put: 20; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	dañoAInfligir := (SimpleMeasure basicNew instVarAt: 1 put: 2; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	dañoAAbsorber := (SimpleMeasure basicNew instVarAt: 1 put: 0; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	estrategia := ((SerializableBlockClosure basicNew instVarAt: 1 put: CombatienteOrcoOlgra; instVarAt: 2 put: '[ :argm4_1 |
	argm4_1 anyOne. ]'; instVarAt: 3 put: #(); yourself) asEvaluable).
	dañoPorEstarDistraido := (SimpleMeasure basicNew instVarAt: 1 put: 0; instVarAt: 2 put: (BaseUnit basicNew instVarAt: 1 put: 'puntosDeVida'; instVarAt: 2 put: 'puntosDeVidas'; instVarAt: 3 put: 'pv'; yourself); yourself).
	bonificadorDeFuerza := 1.! !


!classDefinition: #CombatientesTest category: 'CombatientesFantasticosPaso2'!
DenotativeObject subclass: #CombatientesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CombatientesFantasticosPaso2'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CombatientesTest class' category: 'CombatientesFantasticosPaso2'!
CombatientesTest class
	instanceVariableNames: ''!

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'JMB 4/12/2023 21:22:38'!
arthasConHachaDejaFueraDeCombateAMankrik

	self combatienteHumanoArthas equiparHacha.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.
	
! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'JMB 4/12/2023 21:25:19'!
arthasConHachaDejaFueraDeCombateAOlgra

	self combatienteHumanoArthas equiparHacha.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'pal 8/25/2022 00:54:04'!
combatienteHumanoArthas
	^CombatienteHumanoArthas ! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'pal 8/25/2022 00:54:34'!
combatienteOrcoMankrik
	^CombatienteOrcoMankrik ! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'pal 8/25/2022 00:54:12'!
combatienteOrcoOlgra
	^CombatienteOrcoOlgra ! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'pal 8/25/2022 00:53:32'!
comenzarUnNuevoCombate
	CombatienteHumanoArthas prepararseParaElCombate .
	CombatienteOrcoOlgra prepararseParaElCombate .
	CombatienteOrcoMankrik prepararseParaElCombate .! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'JMB 4/12/2023 21:28:36'!
mankrikConHachaDejaFueraDeCombateAArthas

	self combatienteOrcoMankrik equiparHacha.
	
	self combatienteOrcoMankrik atacarA: self combatienteHumanoArthas.
	self combatienteOrcoMankrik atacarA: self combatienteHumanoArthas.
	self combatienteOrcoMankrik atacarA: self combatienteHumanoArthas.
! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'JMB 4/12/2023 21:24:04'!
olgraConHachaDejaFueraDeCombateAArthas

	self combatienteOrcoOlgra equiparHacha.
	
	self combatienteOrcoOlgra atacarA: self combatienteHumanoArthas.
	self combatienteOrcoOlgra atacarA: self combatienteHumanoArthas.
	self combatienteOrcoOlgra atacarA: self combatienteHumanoArthas.
! !

!CombatientesTest class methodsFor: 'preparacion de combates' stamp: 'JMB 4/12/2023 21:26:57'!
olgraConHachaDejaFueraDeCombateAMankrik

	self combatienteOrcoOlgra equiparHacha.
	
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
! !


!CombatientesTest class methodsFor: 'tests - combatientes' stamp: 'JMB 9/14/2022 00:20:06'!
test01CuandoArthasAtacaAOlgraLeHaceDaño
	
	self comenzarUnNuevoCombate.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 18*pv) description: 'Olgra debe tener 18 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - combatientes' stamp: 'JMB 9/14/2022 00:20:11'!
test02CuandoArthasAtacaMasDeUnaVezAOlgraLeHaceDañoEnCadaAtaque

	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 16*pv) description: 'Olgra debe tener 16 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - combatientes' stamp: 'JMB 4/8/2023 00:14:34'!
test03CuandoArthasAtacaConUnHachaHace10DeDaño

	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas equiparHacha.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 10*pv) description: 'Olgra debe tener 10 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - combatientes' stamp: 'JMB 4/8/2023 00:14:22'!
test04CuandoArthasAtacaConUnaEspadaHace8DeDaño

	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas equiparEspada.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 12*pv) description: 'Olgra debe tener 12 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - combatientes' stamp: 'JMB 4/8/2023 00:14:22'!
test05CuandoOlgraUsaUnaArmaduraDeCueroResuceLosAtaquesCon2DeDaño

	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas equiparEspada.
	self combatienteOrcoOlgra equiparConArmaduraDeCuero.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 14*pv) description: 'Olgra debe tener 14 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - combatientes' stamp: 'JMB 4/8/2023 00:14:22'!
test06CuandoOlgraUsaUnaArmaduraDeMetalResuceLosAtaquesCon6DeDaño

	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas equiparEspada.
	self combatienteOrcoOlgra equiparConArmaduraDeMetal.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 18*pv) description: 'Olgra debe tener 18 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - combatientes' stamp: 'JMB 4/8/2023 00:14:09'!
test07CuandoOlgraUsaUnaArmaduraResuceElDañoPeroNoRecuperaPuntosDeVida

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparConArmaduraDeMetal.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: self combatienteOrcoOlgra noEstaHerido description: 'Olgra debe tener los puntos de vida completos'.! !


!CombatientesTest class methodsFor: 'tests - atacar' stamp: 'JMB 4/13/2023 20:02:54'!
test08CuandoArthasNoTieneEnemigosNoAtacaANadie
	self comenzarUnNuevoCombate .
	
	self combatienteHumanoArthas atacar.
	
	Assert isTrue: self combatienteOrcoOlgra noEstaHerido description: 'Olgra debe tener los puntos de vida completos'.
	Assert isTrue: self combatienteOrcoMankrik noEstaHerido description: 'Mankrik debe tener los puntos de vida completos'.
	
	! !

!CombatientesTest class methodsFor: 'tests - atacar' stamp: 'JMB 4/13/2023 20:02:54'!
test09CuandoArthasTieneEnemigosAtacaAUno
	| soloOlgraEstaHerido soloMankrikEstaHerido |
	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas enemigos: {self combatienteOrcoOlgra . self combatienteOrcoMankrik .}.
	
	self combatienteHumanoArthas atacar.
	
	soloOlgraEstaHerido := self combatienteOrcoOlgra estaHerido and: [self combatienteOrcoMankrik noEstaHerido ].
	soloMankrikEstaHerido := self combatienteOrcoMankrik estaHerido and: [self combatienteOrcoOlgra noEstaHerido ].
	Assert isTrue: (soloOlgraEstaHerido or: [soloMankrikEstaHerido]).! !

!CombatientesTest class methodsFor: 'tests - atacar' stamp: 'JMB 4/13/2023 20:02:54'!
test10CuandoArthasTieneComoEstrategiaAtacarAlMasHeridoDañaAlQueEstaMasHerido
	
	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas enemigos: {self combatienteOrcoOlgra . self combatienteOrcoMankrik .}.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra .	
	self combatienteHumanoArthas focalizarEnElMasHerido.
	
	self combatienteHumanoArthas atacar.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 16*pv) description: 'Olgra debe tener 16 puntos de vida'.
	! !

!CombatientesTest class methodsFor: 'tests - atacar' stamp: 'JMB 4/13/2023 20:02:54'!
test11CuandoArthasTieneComoEstrategiaAtacarAlMasSanoDañaAlQueEstaMenosHerido
	
	self comenzarUnNuevoCombate.
	self combatienteHumanoArthas equiparEspada.
	self combatienteHumanoArthas enemigos: {self combatienteOrcoOlgra . self combatienteOrcoMankrik .}.
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.	
	self combatienteHumanoArthas focalizarEnElMasSano.
	
	self combatienteHumanoArthas atacar.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 12*pv) description: 'Olgra debe tener 12 puntos de vida'.
	! !


!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 9/14/2022 00:38:50'!
test12CuandoOlgraAtacaConSusPuñosAMankrikLeHace2DeDañoMas1PorFuerza
	
	self comenzarUnNuevoCombate.
	
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
	
	Assert isTrue: (self combatienteOrcoMankrik tienePuntosDeVida: 17*pv) description: 'Mankrik debe tener 17 puntos de vida (20 inicial - 2 por puños - 1 por fuerza)'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 9/14/2022 00:38:32'!
test13CuandoMankrikAtacaConSusPuñosAOlgraLeHace2DeDañoMas2PorFuerza
	
	self comenzarUnNuevoCombate.
	
	self combatienteOrcoMankrik atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 16*pv) description: 'Olgra debe tener 16 puntos de vida (20 inicial - 2 por puños - 2 por fuerza)'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 9/14/2022 00:39:42'!
test14CuandoMankrikAtacaAArthasLeHaceDañoReducidoEn1PorSuAgilidad
	
	self comenzarUnNuevoCombate.
	
	self combatienteOrcoMankrik atacarA: self combatienteHumanoArthas.

	Assert 
		isTrue: (self combatienteHumanoArthas tienePuntosDeVida: 17*pv) 
		description: 'Arthas debe tener 17 puntos de vida (20 inicial - 2 por puños - 2 por fuerza + 1 agilidad de Arthas)'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:34'!
test15CuandoOlgraAtacaConUnHachaHace10DeDañoMas1PorFuerza

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparHacha.
	
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
	
	Assert isTrue: (self combatienteOrcoMankrik tienePuntosDeVida: 9*pv) description: 'Olgra debe tener 9 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:22'!
test16CuandoOlgraAtacaConUnaEspadaHace8DeDañoMas1PorFuerza

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparEspada.
	
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
	
	Assert isTrue: (self combatienteOrcoMankrik tienePuntosDeVida: 11*pv) description: 'Mankrik debe tener 11 puntos de vida'.
	! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:34'!
test17CuandoMankrikAtacaConUnHachaHace10DeDañoMas2PorFuerza

	self comenzarUnNuevoCombate .
	self combatienteOrcoMankrik equiparHacha.
	
	self combatienteOrcoMankrik atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 8*pv) description: 'Olgra debe tener 8 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:22'!
test18CuandoMankrikAtacaConUnaEspadaHace8DeDañoMas2PorFuerza

	self comenzarUnNuevoCombate .
	self combatienteOrcoMankrik equiparEspada.
	
	self combatienteOrcoMankrik atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 10*pv) description: 'Olgra debe tener 10 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:22'!
test19CuandoMankrikUsaUnaArmaduraDeCueroResuceLosAtaquesCon2DeDaño

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparEspada.
	self combatienteOrcoMankrik equiparConArmaduraDeCuero.
	
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
	
	Assert isTrue: (self combatienteOrcoMankrik tienePuntosDeVida: 13*pv) description: 'Mankrik debe tener 13 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:22'!
test20CuandoMankrikUsaUnaArmaduraDeMetalResuceLosAtaquesCon6DeDaño

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparEspada.
	self combatienteOrcoMankrik equiparConArmaduraDeMetal.
	
	self combatienteOrcoOlgra atacarA: self combatienteOrcoMankrik.
	
	Assert isTrue: (self combatienteOrcoMankrik tienePuntosDeVida: 17*pv) description: 'Mankrik debe tener 17 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:22'!
test21CuandoArthasUsaUnaArmaduraDeCueroResuceLosAtaquesCon2DeDaño

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparEspada.
	self combatienteHumanoArthas equiparConArmaduraDeCuero.
	
	self combatienteOrcoOlgra atacarA: self combatienteHumanoArthas.
	
	Assert isTrue: (self combatienteHumanoArthas tienePuntosDeVida: 14*pv) description: 'Arthas debe tener 14 puntos de vida'.! !

!CombatientesTest class methodsFor: 'tests - fuerza y agilidad' stamp: 'JMB 4/8/2023 00:14:22'!
test22CuandoArthasUsaUnaArmaduraDeMetalResuceLosAtaquesCon6DeDaño

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparEspada.
	self combatienteHumanoArthas equiparConArmaduraDeMetal.
	
	self combatienteOrcoOlgra atacarA: self combatienteHumanoArthas.
	
	Assert isTrue: (self combatienteHumanoArthas tienePuntosDeVida: 18*pv) description: 'Arthas debe tener 18 puntos de vida'.! !


!CombatientesTest class methodsFor: 'tests - fuera de combate' stamp: 'JMB 4/12/2023 21:25:44'!
test23CuandoArthasEstaFueraDeCombateNoHaceDaño

	self comenzarUnNuevoCombate.
	self olgraConHachaDejaFueraDeCombateAArthas.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isFalse: self combatienteOrcoOlgra estaHerido.! !

!CombatientesTest class methodsFor: 'tests - fuera de combate' stamp: 'JMB 4/12/2023 21:25:40'!
test24CuandoOlgraEstaFueraDeCombateNoHaceDaño

	self comenzarUnNuevoCombate.
	self arthasConHachaDejaFueraDeCombateAOlgra. 
	
	self combatienteOrcoOlgra atacarA: self combatienteHumanoArthas.
	
	Assert isFalse: self combatienteHumanoArthas estaHerido.! !

!CombatientesTest class methodsFor: 'tests - fuera de combate' stamp: 'JMB 4/12/2023 21:25:59'!
test25CuandoMankrikEstaFueraDeCombateNoHaceDaño

	self comenzarUnNuevoCombate.
	self arthasConHachaDejaFueraDeCombateAMankrik.
	
	self combatienteOrcoMankrik atacarA: self combatienteHumanoArthas.
	
	Assert isFalse: self combatienteHumanoArthas estaHerido.! !

!CombatientesTest class methodsFor: 'tests - fuera de combate' stamp: 'JMB 4/13/2023 20:02:54'!
test26ArthasNoAtacaALosEnemigosFueraDeCombate

	self comenzarUnNuevoCombate .
	self combatienteHumanoArthas focalizarEnElMasHerido.	
	self arthasConHachaDejaFueraDeCombateAMankrik.
	self combatienteHumanoArthas enemigos: {self combatienteOrcoOlgra . self combatienteOrcoMankrik .}.
	
	self combatienteHumanoArthas atacar.
	
	Assert isTrue: self combatienteOrcoOlgra estaHerido.! !

!CombatientesTest class methodsFor: 'tests - fuera de combate' stamp: 'JMB 4/13/2023 20:02:54'!
test27OlgraNoAtacaALosEnemigosFueraDeCombate

	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra focalizarEnElMasHerido.
	self olgraConHachaDejaFueraDeCombateAMankrik.
	self combatienteOrcoOlgra enemigos: {self combatienteHumanoArthas. self combatienteOrcoMankrik .}.
	
	self combatienteOrcoOlgra atacar.
	
	Assert isTrue: self combatienteHumanoArthas estaHerido.! !

!CombatientesTest class methodsFor: 'tests - fuera de combate' stamp: 'JMB 4/13/2023 20:02:54'!
test28MankrikNoAtacaALosEnemigosFueraDeCombate

	self comenzarUnNuevoCombate .
	self combatienteOrcoMankrik focalizarEnElMasHerido.
	self mankrikConHachaDejaFueraDeCombateAArthas.
	self combatienteOrcoMankrik enemigos: {self combatienteHumanoArthas. self combatienteOrcoOlgra .}.
	
	self combatienteOrcoMankrik atacar.
	
	Assert isTrue: self combatienteOrcoOlgra estaHerido.! !


!CombatientesTest class methodsFor: 'tests - condicion de distraido' stamp: 'JMB 4/8/2023 00:14:22'!
test29CuandoOlgraEstaDistraidoLeHacen2MasDeDaño
	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparConArmaduraDeCuero.	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	self combatienteHumanoArthas equiparEspada.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert 
		isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 12*pv) 
		description: 'Olgra debe tener 12 puntos de vida: 20 (inicial) - 8 (por espada) + 2 (armadura de cuero) - 2 (daño por distraido)'.! !

!CombatientesTest class methodsFor: 'tests - condicion de distraido' stamp: 'JMB 4/8/2023 00:13:56'!
test30CuandoElAtaqueNoInflingeDañoOlgraNoRecibeDañoPorEstarDistradido
	self comenzarUnNuevoCombate .
	self combatienteOrcoOlgra equiparConArmaduraDeCuero.	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	
	Assert isTrue: self combatienteOrcoOlgra noEstaHerido description: 'Olgra debe tener los puntos de vida completos'.! !

!CombatientesTest class methodsFor: 'tests - condicion de distraido' stamp: 'JMB 4/8/2023 00:14:22'!
test31CuandoMankrikEstaDistraidoLeHacen2MasDeDaño
	self comenzarUnNuevoCombate .
	self combatienteOrcoMankrik equiparConArmaduraDeCuero.	
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.
	self combatienteHumanoArthas equiparEspada.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.
	
	Assert 
		isTrue: (self combatienteOrcoMankrik tienePuntosDeVida: 12*pv) 
		description: 'Mankrik debe tener 12 puntos de vida: 20 (inicial) - 8 (por espada) + 2 (armadura de cuero) - 2 (daño por distraido)'.! !

!CombatientesTest class methodsFor: 'tests - condicion de distraido' stamp: 'JMB 4/8/2023 00:13:56'!
test32CuandoElAtaqueNoInflingeDañoMankrikNoRecibeDañoPorEstarDistradido
	self comenzarUnNuevoCombate .
	self combatienteOrcoMankrik equiparConArmaduraDeCuero.	
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.
	
	self combatienteHumanoArthas atacarA: self combatienteOrcoMankrik.
	
	Assert isTrue: self combatienteOrcoMankrik noEstaHerido description: 'Mankrik debe tener los puntos de vida completos'.! !


!CombatientesTest class methodsFor: '--** private fileout/in **--' stamp: 'JMB 9/14/2022 00:19:03'!
initialize

	"Este método está para crear unidades. No se debe tocar"

	| puntosDeVida |
	puntosDeVida := BaseUnit named: 'puntosDeVida' sign: 'pv'.

	Smalltalk at: #pv put: puntosDeVida.
	Smalltalk at: #puntosDeVida put: puntosDeVida.! !


!classDefinition: #EstrategiasTest category: 'CombatientesFantasticosPaso2'!
DenotativeObject subclass: #EstrategiasTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CombatientesFantasticosPaso2'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'EstrategiasTest class' category: 'CombatientesFantasticosPaso2'!
EstrategiasTest class
	instanceVariableNames: ''!

!EstrategiasTest class methodsFor: 'combates' stamp: 'JMB 4/8/2023 00:14:34'!
orquestadorParaArthasUsandoUnHachaYConFocoEnElMasHeridoVsOrcos
	
	OrquestadorDeCombatesTest prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasHerido. 
	self combatienteHumanoArthas equiparHacha.
	OrquestadorDeCombatesTest 
		establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
	^ OrquestadorDeCombatesTest orquestadorDeCombates! !

!EstrategiasTest class methodsFor: 'combates' stamp: 'JMB 4/8/2023 00:14:34'!
orquestadorParaArthasUsandoUnHachaYConFocoEnElMasSanoVsOrcos
	
	OrquestadorDeCombatesTest prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasSano. 
	self combatienteHumanoArthas equiparHacha.
	OrquestadorDeCombatesTest 
		establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
	^ OrquestadorDeCombatesTest orquestadorDeCombates! !

!EstrategiasTest class methodsFor: 'combates' stamp: 'JMB 9/15/2022 00:10:15'!
puntosDeVidaDelGanadorFocalizandoEnElMasHerido
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaArthasUsandoUnHachaYConFocoEnElMasHeridoVsOrcos.
	orquestadorDeCombates desarrollarDuranteRondas: 100.
	^ self combatienteHumanoArthas puntosDeVida! !

!EstrategiasTest class methodsFor: 'combates' stamp: 'JMB 9/15/2022 00:10:08'!
puntosDeVidaDelGanadorFocalizandoEnElMasSano
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaArthasUsandoUnHachaYConFocoEnElMasSanoVsOrcos.
	orquestadorDeCombates desarrollarDuranteRondas: 100.
	^ self combatienteHumanoArthas puntosDeVida! !


!EstrategiasTest class methodsFor: 'combatientes' stamp: 'JMB 9/14/2022 23:10:44'!
combatienteHumanoArthas
	^CombatienteHumanoArthas ! !

!EstrategiasTest class methodsFor: 'combatientes' stamp: 'JMB 9/15/2022 00:01:56'!
combatienteOrcoMankrik
	^CombatienteOrcoMankrik ! !

!EstrategiasTest class methodsFor: 'combatientes' stamp: 'JMB 9/14/2022 23:11:07'!
combatienteOrcoOlgra
	^CombatienteOrcoOlgra ! !


!EstrategiasTest class methodsFor: 'tests' stamp: 'JMB 9/15/2022 00:09:22'!
test01FocalizarEnElMasHeridoEsMejorQueEnElMasSano
	
	Assert isTrue: self puntosDeVidaDelGanadorFocalizandoEnElMasHerido > self puntosDeVidaDelGanadorFocalizandoEnElMasSano.! !


!classDefinition: #OrquestadorDeCombates category: 'CombatientesFantasticosPaso2'!
DenotativeObject subclass: #OrquestadorDeCombates
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CombatientesFantasticosPaso2'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'OrquestadorDeCombates class' category: 'CombatientesFantasticosPaso2'!
OrquestadorDeCombates class
	instanceVariableNames: 'bando2 bando1 rondaDeCombate'!

!OrquestadorDeCombates class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:35:40'!
bandoEstaDerrotado: unBando
	 ^ unBando allSatisfy: [ :combatiente | combatiente estaFueraDeCombate ].
	! !

!OrquestadorDeCombates class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:32:00'!
desarrollarDuranteRondas: numeroDeRondasMaxima
	[ ((self bandoEstaDerrotado: bando1) not) and: ((self bandoEstaDerrotado: bando2) not) and: (rondaDeCombate < numeroDeRondasMaxima ) ] whileTrue:
	[
		rondaDeCombate := rondaDeCombate + 1.
		bando1 do: [ :combatienteBando1 | combatienteBando1 atacar ].
		bando2 do: [ :combatienteBando2 | combatienteBando2 atacar ].
	].

	^ self.! !

!OrquestadorDeCombates class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:33:52'!
establecerCombateEntreBando1: unBando1 contraBando2: unBando2
	rondaDeCombate := 0.
	bando1 := unBando1.
	bando2 := unBando2.
	bando1 do: [ :combatienteBando1 | combatienteBando1 enemigos: bando2 ].
	bando2 do: [ :combatienteBando2 | combatienteBando2 enemigos: bando1 ].! !

!OrquestadorDeCombates class methodsFor: 'combate' stamp: 'sv 4/16/2023 20:33:13'!
ganador
	| ganador |

	(self bandoEstaDerrotado: bando1) ifTrue: [ ganador := #bando2. ^ ganador ]. 
	(self bandoEstaDerrotado: bando2) ifTrue: [ ganador := #bando1.  ^ ganador ]. 
	ganador := #indeterminado.
	^ ganador.! !

!OrquestadorDeCombates class methodsFor: 'combate' stamp: 'sv 4/16/2023 19:46:39'!
rondasDeCombate
	^ rondaDeCombate.! !


!OrquestadorDeCombates class methodsFor: '--** private fileout/in **--' stamp: 'sv 4/16/2023 20:36:16'!
initializeCollaboratorsFromFile
	"Auto generated method for loading purposes - Do not modify it"

	bando2 := ((Array new: 2) at: 1 put: CombatienteOrcoOlgra; at: 2 put: CombatienteOrcoMankrik; yourself).
	bando1 := ((Array new: 1) at: 1 put: CombatienteHumanoArthas; yourself).
	rondaDeCombate := 4.! !


!classDefinition: #OrquestadorDeCombatesTest category: 'CombatientesFantasticosPaso2'!
DenotativeObject subclass: #OrquestadorDeCombatesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CombatientesFantasticosPaso2'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'OrquestadorDeCombatesTest class' category: 'CombatientesFantasticosPaso2'!
OrquestadorDeCombatesTest class
	instanceVariableNames: 'bando1 bando2'!

!OrquestadorDeCombatesTest class methodsFor: 'para completar' stamp: 'sv 4/16/2023 20:06:16'!
assertElResultadoIndicaQueGanoElBando1: resultadoDeUnCombate alcanzadaLaRonda: unNumeroDeRondaEsperado

	Assert isTrue: [ (resultadoDeUnCombate ganador = #bando1) and: (resultadoDeUnCombate rondasDeCombate = unNumeroDeRondaEsperado) ] description: 'ganador debe ser #bando1 y rondasDeCombate debe ser igual a unNumeroDeRondaEsperado'! !

!OrquestadorDeCombatesTest class methodsFor: 'para completar' stamp: 'sv 4/16/2023 20:06:03'!
assertElResultadoIndicaQueGanoElBando2: resultadoDeUnCombate alcanzadaLaRonda: unNumeroDeRondaEsperado

	Assert isTrue: [ (resultadoDeUnCombate ganador = #bando2) and: (resultadoDeUnCombate rondasDeCombate = unNumeroDeRondaEsperado) ] description: 'ganador debe ser #bando2 y rondasDeCombate debe ser igual a unNumeroDeRondaEsperado'! !

!OrquestadorDeCombatesTest class methodsFor: 'para completar' stamp: 'sv 4/16/2023 20:06:29'!
assertElResultadoIndicaQueNoHayGanador: resultadoDeUnCombate  alcanzadaLaRonda: unNumeroDeRondaEsperado

	Assert isTrue: [ (resultadoDeUnCombate ganador = #indeterminado) and: (resultadoDeUnCombate rondasDeCombate = unNumeroDeRondaEsperado) ] description: 'ganador debe ser #indeterminado y rondasDeCombate debe ser igual a unNumeroDeRondaEsperado'! !

!OrquestadorDeCombatesTest class methodsFor: 'para completar' stamp: 'sv 4/15/2023 16:24:25'!
establecerCombateEntreBando1: unBando1 contraBando2: unBando2
	self orquestadorDeCombates establecerCombateEntreBando1: unBando1 contraBando2: unBando2! !

!OrquestadorDeCombatesTest class methodsFor: 'para completar' stamp: 'sv 4/15/2023 16:09:12'!
orquestadorDeCombates
	^ OrquestadorDeCombates.! !


!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 9/15/2022 00:30:13'!
orquestadorParaCombateArthasConFocoEnElMasSanoVsOrcos	
	
	self prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasSano. 
	self establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 4/8/2023 00:14:09'!
orquestadorParaCombateArthasInmuneYFocoEnElMasHeridoVsOrcos	
	
	self prepararCombatientes.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	self combatienteHumanoArthas equiparConArmaduraDeMetal. 
	self combatienteHumanoArthas focalizarEnElMasHerido. 
	self establecerCombateEntreBando1:  {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 9/15/2022 00:30:37'!
orquestadorParaCombateArthasVsOlgra	
	
	self prepararCombatientes. 
	self establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra }.
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 9/15/2022 00:30:51'!
orquestadorParaCombateArthasVsOrcos
	self prepararCombatientes.
	self establecerCombateEntreBando1: { self combatienteHumanoArthas } contraBando2: {self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 4/8/2023 00:14:22'!
orquestadorParaCombateManoAManoConBando2Inmune	
	
	self prepararCombatientes.
	self combatienteOrcoOlgra equiparEspada.
	self establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra }.
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 9/15/2022 00:31:13'!
orquestadorParaCombateOrcosVsArthas
	self prepararCombatientes.
	self establecerCombateEntreBando1: {self combatienteOrcoOlgra. self combatienteOrcoMankrik } contraBando2: { self combatienteHumanoArthas }.
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 9/15/2022 00:31:43'!
orquestadorParaCombateOrcosVsArthasConFocoEnElMasSano	
	
	self prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasSano. 
	self establecerCombateEntreBando1: { self combatienteOrcoOlgra. self combatienteOrcoMankrik } contraBando2: {self combatienteHumanoArthas }. 
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 4/8/2023 00:14:09'!
orquestadorParaCombateOrcosVsArthasInmuneYFocoEnElMasHerido	
	
	self prepararCombatientes.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	self combatienteHumanoArthas equiparConArmaduraDeMetal. 
	self combatienteHumanoArthas focalizarEnElMasHerido. 
	self establecerCombateEntreBando1: { self combatienteOrcoOlgra. self combatienteOrcoMankrik } contraBando2: {self combatienteHumanoArthas }.
	^ self orquestadorDeCombates! !

!OrquestadorDeCombatesTest class methodsFor: 'combates' stamp: 'JMB 9/12/2022 20:49:44'!
prepararCombatientes

	self combatienteHumanoArthas prepararseParaElCombate. 
	self combatienteOrcoOlgra prepararseParaElCombate.
	self combatienteOrcoMankrik prepararseParaElCombate. ! !


!OrquestadorDeCombatesTest class methodsFor: 'combatientes' stamp: 'JMB 9/12/2022 20:49:23'!
combatienteHumanoArthas

	^ CombatienteHumanoArthas! !

!OrquestadorDeCombatesTest class methodsFor: 'combatientes' stamp: 'JMB 9/12/2022 20:49:44'!
combatienteOrcoMankrik

	^ CombatienteOrcoMankrik! !

!OrquestadorDeCombatesTest class methodsFor: 'combatientes' stamp: 'JMB 9/12/2022 20:48:43'!
combatienteOrcoOlgra

	^ CombatienteOrcoOlgra! !


!OrquestadorDeCombatesTest class methodsFor: 'tests - mano a mano' stamp: 'JMB 9/15/2022 00:30:37'!
test01AlDesarrollarUnCombateSinRondasLosCombatientesNoSeAtacan
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	orquestadorDeCombates desarrollarDuranteRondas: 0.
	
	Assert isTrue: self combatienteHumanoArthas noEstaHerido.
	Assert isTrue: self combatienteOrcoOlgra noEstaHerido.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - mano a mano' stamp: 'JMB 9/15/2022 00:30:37'!
test02AlDesarrollarUnCombateManoAManoDeUnaRondaLosBandosSeAtacanUnaVez
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	orquestadorDeCombates desarrollarDuranteRondas: 1.
	
	Assert isTrue: (self combatienteHumanoArthas tienePuntosDeVida: 18*pv) description: 'Arthas debe recibir un puñetazo (2 de daño)'.
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 18*pv) description: 'Olgra debe recibir un puñetazo (2 de daño)'.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - mano a mano' stamp: 'JMB 9/15/2022 00:30:37'!
test03AlDesarrollarUnCombateManoAManoConMasDeUnaRondaLosBandosSeAtacanCadaVez
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	orquestadorDeCombates desarrollarDuranteRondas: 2.
	
	Assert isTrue: (self combatienteHumanoArthas tienePuntosDeVida: 16*pv) description: 'Arthas debe recibir dos puñetazos (4 de daño)'.
	Assert isTrue: (self combatienteOrcoOlgra tienePuntosDeVida: 16*pv) description: 'Olgra debe recibir dos puñetazos (4 de daño)'.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - mano a mano' stamp: 'JMB 9/15/2022 00:30:37'!
test04AlDesarrollarUnCombateManoAManoSinGanadoresElResultadoEsIndeterminado
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 0.
	
	self assertElResultadoIndicaQueNoHayGanador: resultado alcanzadaLaRonda: 0.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - mano a mano' stamp: 'JMB 9/15/2022 00:30:37'!
test05AlDesarrollarUnCombateManoAManoGanaElBando1CuandoDejaFueraDeCombateASuRival
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 10.
	
	self assertElResultadoIndicaQueGanoElBando1: resultado alcanzadaLaRonda: 10.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - mano a mano' stamp: 'JMB 9/15/2022 00:30:37'!
test06AlDesarrollarUnCombateCuandoHayUnGanadorFinalizaEnLaRondaQueElPerdedorQuedoFueraDeCombate
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 11.
	
	self assertElResultadoIndicaQueGanoElBando1: resultado alcanzadaLaRonda: 10.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - mano a mano' stamp: 'JMB 9/15/2022 00:31:33'!
test07AlDesarrollarUnCombateManoAManoGanaElBando2CuandoDejaFueraDeCombateASuRival
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateManoAManoConBando2Inmune.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 100.
	
	self assertElResultadoIndicaQueGanoElBando2: resultado alcanzadaLaRonda: 3.! !


!OrquestadorDeCombatesTest class methodsFor: 'tests - muches vs muches' stamp: 'JMB 9/15/2022 00:30:13'!
test08AlDesarrollarUnCombateLosDelBando1PuedenAtacarACualquieraDelBando2
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasConFocoEnElMasSanoVsOrcos.
	
	orquestadorDeCombates desarrollarDuranteRondas: 2.
	
	Assert isTrue: self combatienteOrcoOlgra estaHerido. 
	Assert isTrue: self combatienteOrcoMankrik estaHerido. ! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - muches vs muches' stamp: 'JMB 9/15/2022 00:31:13'!
test09AlDesarrollarUnCombateAtacanTodesLosDelBando1EnCadaRonda
	"
		En cada ronda Olgra daña a Arthas en 2  (2 puños + 1 por fuerza - 1 por agilidad de Arthas) y  Mankrik 3 (2 puños + 2 por fuerza - 1 por agilidad de Arthas) 
	"
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateOrcosVsArthas.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 4.
	
	self assertElResultadoIndicaQueGanoElBando1: resultado alcanzadaLaRonda: 4.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - muches vs muches' stamp: 'JMB 9/15/2022 00:31:43'!
test10AlDesarrollarUnCombateLosDelBando2PuedenAtacarACualquieraDelBando1
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateOrcosVsArthasConFocoEnElMasSano.
	
	orquestadorDeCombates desarrollarDuranteRondas: 2.
	
	Assert isTrue: self combatienteOrcoOlgra estaHerido. 
	Assert isTrue: self combatienteOrcoMankrik estaHerido. ! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - muches vs muches' stamp: 'JMB 9/15/2022 00:30:51'!
test11AlDesarrollarUnCombateAtacanTodesLosDelBando2EnCadaRonda
	"
		En cada ronda Olgra daña a Arthas en 2  (2 puños + 1 por fuerza - 1 por agilidad de Arthas) y  Mankrik 3 (2 puños + 2 por fuerza - 1 por agilidad de Arthas) 
	"
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOrcos.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 4.
	
	self assertElResultadoIndicaQueGanoElBando2: resultado alcanzadaLaRonda: 4.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - muches vs muches' stamp: 'JMB 9/15/2022 00:30:25'!
test12AlDesarrollarUnCombateLosDelBando1NoGananSiTodosLosDelBando2NoEstanFueraDeCombate
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasInmuneYFocoEnElMasHeridoVsOrcos.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 10.
	
	self assertElResultadoIndicaQueNoHayGanador: resultado alcanzadaLaRonda: 10.! !

!OrquestadorDeCombatesTest class methodsFor: 'tests - muches vs muches' stamp: 'JMB 9/15/2022 00:31:54'!
test13AlDesarrollarUnCombateLosDelBando2NoGananSiTodosLosDelBando1NoEstanFueraDeCombate
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateOrcosVsArthasInmuneYFocoEnElMasHerido.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 10.
	
	self assertElResultadoIndicaQueNoHayGanador: resultado alcanzadaLaRonda: 10.! !


!OrquestadorDeCombatesTest class methodsFor: '--** private fileout/in **--' stamp: 'sv 4/16/2023 20:36:16'!
initializeCollaboratorsFromFile
	"Auto generated method for loading purposes - Do not modify it"

	bando1 := ((Array new: 1) at: 1 put: CombatienteHumanoArthas; yourself).
	bando2 := ((Array new: 1) at: 1 put: CombatienteOrcoOlgra; yourself).! !

CombatienteHumanoArthas initializeAfterFileIn!
CombatienteOrcoMankrik initializeAfterFileIn!
CombatienteOrcoOlgra initializeAfterFileIn!
CombatientesTest initialize!
OrquestadorDeCombates initializeAfterFileIn!
OrquestadorDeCombatesTest initializeAfterFileIn!
