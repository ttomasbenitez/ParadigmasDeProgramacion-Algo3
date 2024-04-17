!classDefinition: #AtacarTest category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
TestCase subclass: #AtacarTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!AtacarTest methodsFor: 'tests' stamp: 'SF 10/17/2022 19:20:49'!
hacerQue: combatiente1 quedeMalDebidoA: combatiente2

	[combatiente1 puntosDeVida <= 0] whileFalse: [
		combatiente2 atacarA: combatiente1]! !

!AtacarTest methodsFor: 'tests' stamp: 'JMB 3/5/2023 21:55:28'!
nuevoCombatienteHumanoide

	^ Combatiente humanoide! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:50'!
test01CuandoUnCombatienteNoTieneEnemigosNoAtacaANadie
	| combatiente alguien |
	combatiente := self nuevoCombatienteHumanoide.
	alguien := self nuevoCombatienteHumanoide.
	
	combatiente usarPoder.
	
	self assert: (alguien noEstaHerido).
	
	! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:51'!
test02CuandoUnCombatienteTieneEnemigosAtacaAUno
	| combatiente enemigo1 enemigo2 enemigo1Herido enemigo2Herido |
	combatiente := self nuevoCombatienteHumanoide.
	enemigo1 := self nuevoCombatienteHumanoide.
	enemigo2 := self nuevoCombatienteHumanoide.
	combatiente enemigos: {enemigo1. enemigo2.}.
	
	combatiente usarPoder.
	
	enemigo1Herido := enemigo1 estaHerido and: [enemigo2 noEstaHerido].
	enemigo2Herido := enemigo2 estaHerido and: [enemigo1 noEstaHerido].
	self assert: (enemigo1Herido or: [enemigo2Herido]).
	
	! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:51'!
test03UnCombatientePuedeTenerComoEstrategiaAtacarAlMasHerido
	| combatiente enemigo1 enemigo2 enemigo2HeridoDosVeces |
	combatiente := self nuevoCombatienteHumanoide.
	enemigo1 := self nuevoCombatienteHumanoide.
	enemigo2 := self nuevoCombatienteHumanoide.
	combatiente enemigos: {enemigo1. enemigo2.}.
	combatiente atacarA: enemigo2.
	
	combatiente focalizarEnElMasHerido.
	combatiente usarPoder.
	
	enemigo2HeridoDosVeces := enemigo2 tienePuntosDeVida: 16 * pv.
	self assert: enemigo2HeridoDosVeces.
	
	! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:50'!
test04UnCombatientePuedeTenerComoEstrategiaAtacarAlMasSano
	| combatiente enemigo1 enemigo2 elEnemigo1EstaHerido |
	combatiente := self nuevoCombatienteHumanoide.
	enemigo1 := self nuevoCombatienteHumanoide.
	enemigo2 := self nuevoCombatienteHumanoide.
	combatiente enemigos: {enemigo1. enemigo2.}.
	combatiente atacarA: enemigo2.
	
	combatiente focalizarEnElMasSano.
	combatiente usarPoder.
	
	elEnemigo1EstaHerido := enemigo1 tienePuntosDeVida: 18*pv.
	self assert: elEnemigo1EstaHerido.
	
	! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:50'!
test05ElEnemigoFocalizadoMasSanoCambiaDinamicamente
	| combatiente enemigo1 enemigo2 |
	combatiente := self nuevoCombatienteHumanoide.
	enemigo1 := self nuevoCombatienteHumanoide.
	enemigo2 := self nuevoCombatienteHumanoide.
	combatiente enemigos: {enemigo1. enemigo2.}.
	combatiente atacarA: enemigo2.

	combatiente equiparConUnaEspada.	
	combatiente focalizarEnElMasSano.
	combatiente usarPoder.
	combatiente usarPoder.
	
	self assert: (enemigo1 tienePuntosDeVida: 12*pv).
	self assert: (enemigo2 tienePuntosDeVida: 10*pv).
	
	! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:51'!
test06ElEnemigoFocalizadoMasHeridoCambiaDinamicamente
	| combatiente enemigo1 enemigo2 |
	combatiente := self nuevoCombatienteHumanoide.
	enemigo1 := self nuevoCombatienteHumanoide.
	enemigo2 := self nuevoCombatienteHumanoide.
	combatiente enemigos: {enemigo1. enemigo2.}.
	combatiente atacarA: enemigo2.

	combatiente equiparConUnaEspada.	
	combatiente focalizarEnElMasHerido.
	combatiente usarPoder.
	combatiente atacarA: enemigo1.
	combatiente atacarA: enemigo1.
	combatiente usarPoder.
	
	self assert: (enemigo1 tienePuntosDeVida: -4*pv).
	self assert: (enemigo2 tienePuntosDeVida: 10*pv).
	
	! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:51'!
test07CuandoUnEnemigoEstaVencidoNoEsAtacadoMas
	| combatiente enemigoVencido enemigo2 |
	combatiente := self nuevoCombatienteHumanoide.
	enemigoVencido := self nuevoCombatienteHumanoide.
	enemigo2 := self nuevoCombatienteHumanoide.
	combatiente enemigos: {enemigoVencido. enemigo2.}.
	combatiente equiparConUnaEspada.	
	combatiente atacarA: enemigoVencido.
	combatiente atacarA: enemigoVencido.
	combatiente atacarA: enemigoVencido.

	combatiente focalizarEnElMasHerido.
	combatiente usarPoder.
	
	self assert: (enemigo2 tienePuntosDeVida: 12*pv).
	
	! !

!AtacarTest methodsFor: 'tests' stamp: 'NT 5/16/2023 21:31:51'!
test08CuandoUnCombatienteEstaVencidoNoAtaca
	| combatiente enemigo |
	combatiente := self nuevoCombatienteHumanoide.
	enemigo := self nuevoCombatienteHumanoide.
	combatiente enemigos: {enemigo}.
	self hacerQue: combatiente quedeMalDebidoA: enemigo.
	
	combatiente usarPoder.
	
	self assert: (enemigo tienePuntosDeVida: 20*pv).
	
	! !


!classDefinition: #CombatesFantasticosTest category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
TestCase subclass: #CombatesFantasticosTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!CombatesFantasticosTest methodsFor: 'tests' stamp: 'JMB 3/5/2023 21:52:13'!
test01UnDragonLeGanaA8CombatientesEn3Rondas
	| bandoCombatientes bandoDragon orquestador resultado |
	bandoDragon := {self nuevoCombatienteDragon}.
	bandoCombatientes := self ochoCombatientes.
	orquestador := OrquestadorDeCombates 	establecerCombateEntreBando1: bandoDragon contraBando2: bandoCombatientes.
	resultado := orquestador desarrollarDuranteRondas: 3.
	self assert: (resultado tieneDeGanadorAlBando1).! !

!CombatesFantasticosTest methodsFor: 'tests' stamp: 'JMB 3/5/2023 21:52:13'!
test02UnDragonLeGanaA8CombatientesArmados
	| bandoCombatientes bandoDragon orquestador resultado |
	bandoDragon := {self nuevoCombatienteDragon}.
	bandoCombatientes := self combatientesArmados: 8.
	orquestador := OrquestadorDeCombates 	establecerCombateEntreBando1: bandoDragon contraBando2: bandoCombatientes.
	resultado := orquestador desarrollarDuranteRondas: 10.
	self assert: (resultado tieneDeGanadorAlBando1).! !

!CombatesFantasticosTest methodsFor: 'tests' stamp: 'JMB 3/5/2023 21:52:13'!
test03UnDragonPierdeContra9CombatientesArmados
	| bandoCombatientes bandoDragon orquestador resultado |
	bandoDragon := {self nuevoCombatienteDragon}.
	bandoCombatientes := self combatientesArmados: 9.
	orquestador := OrquestadorDeCombates 	establecerCombateEntreBando1: bandoDragon contraBando2: bandoCombatientes.
	resultado := orquestador desarrollarDuranteRondas: 10.
	self assert: (resultado tieneDeGanadorAlBando2).! !

!CombatesFantasticosTest methodsFor: 'tests' stamp: 'JMB 3/5/2023 21:52:13'!
test04UnDragonContra4CuradoresNoTermina
	| bandoCuradores bandoDragon orquestador resultado |
	bandoDragon := {self nuevoCombatienteDragon}.
	bandoCuradores := self cuatroCuradores.
	orquestador := OrquestadorDeCombates 	establecerCombateEntreBando1: bandoDragon contraBando2: bandoCuradores.
	resultado := orquestador desarrollarDuranteRondas: 1000.
	self assert: resultado finalizoSinGanadores.! !


!CombatesFantasticosTest methodsFor: 'test-objects' stamp: 'JMB 3/5/2023 21:53:00'!
combatientesArmados: cantidad
	^(1 to: cantidad) collect: [ :numero |
		| combatiente |
		combatiente := self nuevoCombatienteHumanoide.
		combatiente equiparConUnaArmaduraDeCuero.
		combatiente equiparConUnaLanza.
		combatiente.
	].! !

!CombatesFantasticosTest methodsFor: 'test-objects' stamp: 'JMB 3/5/2023 21:52:34'!
cuatroCuradores
	^(1 to: 4) collect: [ :numero | self nuevoCombatienteCurador ].! !

!CombatesFantasticosTest methodsFor: 'test-objects' stamp: 'JMB 3/5/2023 21:52:34'!
nuevoCombatienteCurador

	^ Combatiente curador! !

!CombatesFantasticosTest methodsFor: 'test-objects' stamp: 'JMB 3/5/2023 21:52:13'!
nuevoCombatienteDragon

	^ Combatiente dragon! !

!CombatesFantasticosTest methodsFor: 'test-objects' stamp: 'JMB 3/5/2023 21:55:38'!
nuevoCombatienteHumanoide

	^ Combatiente humanoide! !

!CombatesFantasticosTest methodsFor: 'test-objects' stamp: 'JMB 3/5/2023 21:53:00'!
ochoCombatientes
	^(1 to: 8) collect: [ :numero | self nuevoCombatienteHumanoide ].! !


!classDefinition: #CombatientesFantasticosTest category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
TestCase subclass: #CombatientesFantasticosTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!CombatientesFantasticosTest methodsFor: 'test-objects' stamp: 'SF 2/13/2023 19:38:59'!
nuevoCombatienteCurador
	^Combatiente curador.! !

!CombatientesFantasticosTest methodsFor: 'test-objects' stamp: 'NR 2/12/2023 19:21:28'!
nuevoCombatienteDragon
	^Combatiente dragon.! !

!CombatientesFantasticosTest methodsFor: 'test-objects' stamp: 'JMB 3/5/2023 21:55:47'!
nuevoCombatienteHumanoide

	^ Combatiente humanoide! !

!CombatientesFantasticosTest methodsFor: 'test-objects' stamp: 'NR 2/12/2023 19:21:22'!
nuevoCombatienteIlusionista
	^Combatiente ilusionista.! !

!CombatientesFantasticosTest methodsFor: 'test-objects' stamp: 'SF 2/16/2023 18:10:57'!
nuevoCombatienteOgro
	^Combatiente ogro.! !


!CombatientesFantasticosTest methodsFor: 'tests-01-puntos-de-vida-iniciales' stamp: 'NT 5/16/2023 20:50:07'!
assertQueElCombatiente: unCombatiente comienzaConUnaCantidadDePuntosDeVida: unaCantidadDePuntosDeVidaIniciales

	self assert: (unCombatiente tienePuntosDeVida: unaCantidadDePuntosDeVidaIniciales * pv).! !

!CombatientesFantasticosTest methodsFor: 'tests-01-puntos-de-vida-iniciales' stamp: 'NT 5/16/2023 20:52:33'!
test01UnDragonComienzaCon200PuntosDeVida
	
	self assertQueElCombatiente: self nuevoCombatienteDragon comienzaConUnaCantidadDePuntosDeVida: 200.! !

!CombatientesFantasticosTest methodsFor: 'tests-01-puntos-de-vida-iniciales' stamp: 'NT 5/16/2023 20:52:38'!
test02UnIlusionistaComienzaCon20PuntosDeVida
	
	self assertQueElCombatiente: self nuevoCombatienteIlusionista comienzaConUnaCantidadDePuntosDeVida: 20.! !

!CombatientesFantasticosTest methodsFor: 'tests-01-puntos-de-vida-iniciales' stamp: 'NT 5/16/2023 20:51:47'!
test03UnCuradorComienzaCon20PuntosDeVida
	
	self assertQueElCombatiente: self nuevoCombatienteCurador comienzaConUnaCantidadDePuntosDeVida: 20.! !

!CombatientesFantasticosTest methodsFor: 'tests-01-puntos-de-vida-iniciales' stamp: 'NT 5/16/2023 20:52:21'!
test04UnOgroComienzaCon60PuntosDeVida

	self assertQueElCombatiente: self nuevoCombatienteOgro comienzaConUnaCantidadDePuntosDeVida: 60.! !


!CombatientesFantasticosTest methodsFor: 'tests-02-dragon' stamp: 'NT 5/16/2023 21:31:50'!
assertQueCuandoAtacaElDragon: unDragon ConLosEnemigos: unosEnemigos LaCantidadDeEnemigosAtacadosEs: unaCantidadDeAtacados yQueLaCantidadDeEnemigosNoAtacadosEs: unaCantidadDeNoAtacados 
	
	unDragon enemigos: unosEnemigos.
	unDragon usarPoder.
	
	self assert: unaCantidadDeAtacados equals: (unosEnemigos count: [: unEnemigo | unEnemigo tienePuntosDeVida: 12 * pv]).
	self assert: unaCantidadDeNoAtacados equals: (unosEnemigos count: [: unEnemigo | unEnemigo estaHerido not]).! !

!CombatientesFantasticosTest methodsFor: 'tests-02-dragon' stamp: 'NT 5/16/2023 21:04:44'!
test05UnDragonHace8DeDaño

	| dragon victima enemigos |
	dragon := self nuevoCombatienteDragon.
	victima := self nuevoCombatienteHumanoide.
	enemigos := {victima}.
	
	self 
	assertQueCuandoAtacaElDragon: dragon ConLosEnemigos: enemigos LaCantidadDeEnemigosAtacadosEs: 1 yQueLaCantidadDeEnemigosNoAtacadosEs: 0. ! !

!CombatientesFantasticosTest methodsFor: 'tests-02-dragon' stamp: 'NT 5/16/2023 21:06:31'!
test06UnDragonAtacaAMasDeUnEnemigo

	| dragon victima1 victima2 enemigos |
	dragon := self nuevoCombatienteDragon.
	victima1 := self nuevoCombatienteHumanoide.
	victima2 := self nuevoCombatienteHumanoide.
	enemigos := {victima1. victima2}.
	
	self 
	assertQueCuandoAtacaElDragon: dragon ConLosEnemigos: enemigos LaCantidadDeEnemigosAtacadosEs: 2 yQueLaCantidadDeEnemigosNoAtacadosEs: 0.! !

!CombatientesFantasticosTest methodsFor: 'tests-02-dragon' stamp: 'NT 5/16/2023 21:06:55'!
test07UnDragonAtacaHastaUnMaximoDe8Enemigos

	| dragon enemigos |
	dragon := self nuevoCombatienteDragon.
	enemigos := 	(1 to: 9) collect: [ : unNumero | self nuevoCombatienteHumanoide ]. 
	
	self 
	assertQueCuandoAtacaElDragon: dragon ConLosEnemigos: enemigos LaCantidadDeEnemigosAtacadosEs: 8 yQueLaCantidadDeEnemigosNoAtacadosEs: 1.! !


!CombatientesFantasticosTest methodsFor: 'tests-03-curador' stamp: 'NT 5/16/2023 21:31:51'!
assertQueLosAliados: aliados deUnCurador: unCurador tienenUnaCantidadDePuntosDeVida: unaCantidadDePuntosDeVida

	unCurador aliados: aliados.
	unCurador usarPoder.

	self assert: (aliados allSatisfy: [ : unAliado | unAliado tienePuntosDeVida: unaCantidadDePuntosDeVida*pv ]).! !

!CombatientesFantasticosTest methodsFor: 'tests-03-curador' stamp: 'NT 5/16/2023 21:31:51'!
test08UnCuradorCura2DeDañoASusAliados
	| curador aliado enemigo aliados |
	aliado := self nuevoCombatienteHumanoide.	
	enemigo := self nuevoCombatienteHumanoide.
	enemigo enemigos: {aliado}.
	enemigo usarPoder.
	enemigo usarPoder.
	
	aliados := {aliado}.
	
	curador := self nuevoCombatienteCurador.
	
	self assertQueLosAliados: aliados deUnCurador: curador tienenUnaCantidadDePuntosDeVida: 18.! !

!CombatientesFantasticosTest methodsFor: 'tests-03-curador' stamp: 'NT 5/16/2023 21:01:13'!
test09UnCuradorNoCuraMasDeLosPuntosDeVidaIniciales
	| curador aliados |
	aliados := {self nuevoCombatienteDragon}.
	
	curador := self nuevoCombatienteCurador.
	
	self assertQueLosAliados: aliados deUnCurador: curador tienenUnaCantidadDePuntosDeVida: 200.! !

!CombatientesFantasticosTest methodsFor: 'tests-03-curador' stamp: 'NT 5/16/2023 21:31:50'!
test10UnCuradorCuraATodosSusAliados
	| curador aliados |
	aliados := (1 to: 10) collect: [ : unNumero | 
		| aliado enemigo |
		enemigo := self nuevoCombatienteHumanoide.
		aliado := self nuevoCombatienteHumanoide.
		enemigo enemigos: {aliado}.
		enemigo usarPoder.
		aliado.
	].

	curador := self nuevoCombatienteCurador.
	
	self assertQueLosAliados: aliados deUnCurador: curador tienenUnaCantidadDePuntosDeVida: 20.! !


!CombatientesFantasticosTest methodsFor: '[No hace falta quitar codigo repetido] tests-03-curador' stamp: 'NT 5/16/2023 21:31:50'!
test11UnCuradorNoDañaAUnEnemigo
	| curador enemigo |
	curador := self nuevoCombatienteCurador.
	enemigo := self nuevoCombatienteHumanoide.
	curador enemigos: {enemigo}.

	curador usarPoder.
	
	self assert: enemigo noEstaHerido.! !


!CombatientesFantasticosTest methodsFor: '[No hace falta quitar codigo repetido] tests-04-ilusionista' stamp: 'NT 5/16/2023 21:31:51'!
test12UnIlusionistaContraUnSoloEnemigoNoHaceNada

	|  ilusionista enemigo |
	ilusionista := self nuevoCombatienteIlusionista .
	enemigo := self nuevoCombatienteHumanoide.
	ilusionista enemigos: {enemigo}.
	
	ilusionista usarPoder.
	
	self assert: enemigo noEstaHerido.! !

!CombatientesFantasticosTest methodsFor: '[No hace falta quitar codigo repetido] tests-04-ilusionista' stamp: 'JMB 5/14/2023 12:50:02'!
test13UnIlusionistaAlAtacarEngañaASusVictimasParaQueAtaquenAOtroEnemigo

	|  ilusionista enemigoEngañado enemigoASerAtacadoPorElEngañado |
	ilusionista := self nuevoCombatienteIlusionista .
	enemigoEngañado := self nuevoCombatienteHumanoide.
	enemigoASerAtacadoPorElEngañado := self nuevoCombatienteHumanoide.
	ilusionista enemigos: {enemigoEngañado. enemigoASerAtacadoPorElEngañado}.
	
	ilusionista atacarA: enemigoEngañado.
	
	self assert: enemigoEngañado noEstaHerido. 
	self assert: enemigoASerAtacadoPorElEngañado estaHerido.! !


!CombatientesFantasticosTest methodsFor: '[No hace falta quitar codigo repetido] tests-05-ogro' stamp: 'NT 5/16/2023 21:31:50'!
test14UnOgroNoDañaAUnEnemigoCuandoAtacaLaPrimeraVez
	| ogro enemigo |
	ogro := self nuevoCombatienteOgro.
	enemigo := self nuevoCombatienteHumanoide.
	ogro enemigos: {enemigo}.

	ogro usarPoder.
	
	self assert: enemigo noEstaHerido.! !

!CombatientesFantasticosTest methodsFor: '[No hace falta quitar codigo repetido] tests-05-ogro' stamp: 'NT 5/16/2023 21:31:50'!
test15UnOgroDañaAUnEnemigoCuandoAtacaLuegoDePrepararse
	| ogro enemigo |
	ogro := self nuevoCombatienteOgro.
	enemigo := self nuevoCombatienteHumanoide.
	ogro enemigos: {enemigo}.
	ogro usarPoder.
	
	ogro usarPoder.
	
	self assert: enemigo estaHerido.! !

!CombatientesFantasticosTest methodsFor: '[No hace falta quitar codigo repetido] tests-05-ogro' stamp: 'NT 5/16/2023 21:31:51'!
test16UnOgroHace20DeDañoCuandoAtacaLuegoDePrepararse
	| ogro enemigo |
	ogro := self nuevoCombatienteOgro.
	enemigo := self nuevoCombatienteHumanoide.
	enemigo equiparConUnaArmaduraDeCuero.
	ogro enemigos: {enemigo}.
	ogro usarPoder.
	
	ogro usarPoder.
	
	self assert: (enemigo tienePuntosDeVida: 2 * pv).! !

!CombatientesFantasticosTest methodsFor: '[No hace falta quitar codigo repetido] tests-05-ogro' stamp: 'NT 5/16/2023 21:31:51'!
test17UnOgroLuegoDeAtacarTieneQueVolverAPrepararse
	| ogro enemigo |
	ogro := self nuevoCombatienteOgro.
	enemigo := self nuevoCombatienteHumanoide.
	enemigo equiparConUnaArmaduraDeCuero.
	ogro enemigos: {enemigo}.
	ogro usarPoder.
	ogro usarPoder.
	
	ogro usarPoder.
	
	self assert: enemigo estaFueraDeCombate not.! !


!classDefinition: #CombatientesTest category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
TestCase subclass: #CombatientesTest
	instanceVariableNames: 'atacante atacado'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 10/18/2022 21:08:35'!
test01LosPuñosHacen2DeDaño
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 18*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 10/18/2022 21:04:54'!
test02MultiplesAtaquesVanBajandoMasVida
	atacante atacarA: atacado.
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 16*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:58:02'!
test03UnHachaHace10DeDaño
	atacante equiparConUnHacha.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 10*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:57:31'!
test04UnEspadaHace8DeDaño
	atacante equiparConUnaEspada.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 12*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:58:33'!
test05UnMartilloHace8DeDaño
	atacante equiparConUnMartillo.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 12*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:57:20'!
test06UnaDagaHace4DeDaño
	atacante equiparConUnaDaga.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 16*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:58:21'!
test07UnaLanzaHace8DeDaño
	atacante equiparConUnaLanza.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 12*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:58:48'!
test08UnPaloHace6DeDaño
	atacante equiparConUnPalo.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 14*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:58:02'!
test09LaArmaduraDeMetalAbsorve6DeDaño 
	atacante equiparConUnHacha.
	atacado equiparConUnaArmaduraDeMetal.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 16*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:58:02'!
test10LaArmaduraDeAnillosAbsorve4DeDaño 
	atacante equiparConUnHacha.
	atacado equiparConUnaArmaduraDeAnillos.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 14*pv).! !

!CombatientesTest methodsFor: 'tests' stamp: 'JMB 3/4/2023 12:58:02'!
test11LaArmaduraDeCueroAbsorve2DeDaño 
	atacante equiparConUnHacha.
	atacado equiparConUnaArmaduraDeCuero.
	
	atacante atacarA: atacado.
	
	self assert: (atacado tienePuntosDeVida: 12*pv).! !


!CombatientesTest methodsFor: 'setUp/tearDown' stamp: 'JMB 3/5/2023 21:56:02'!
setUp
	atacante := Combatiente humanoide.
	atacado := Combatiente humanoide.

	! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CombatientesTest class' category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
CombatientesTest class
	instanceVariableNames: ''!

!CombatientesTest class methodsFor: 'class initialization' stamp: 'JMB 10/16/2022 11:11:15'!
initialize

	"Este método está para crear unidades. No se debe tocar"

	| puntosDeVida |
	super initialize.
	puntosDeVida := BaseUnit named: 'puntosDeVida' sign: 'pv'.

	Smalltalk at: #pv put: puntosDeVida.
	Smalltalk at: #puntosDeVida put: puntosDeVida.! !


!classDefinition: #EstrategiasTest category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
TestCase subclass: #EstrategiasTest
	instanceVariableNames: 'arthas mankrik olgra'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!EstrategiasTest methodsFor: 'combatientes' stamp: 'SF 10/11/2022 22:03:48'!
combatienteHumanoArthas
	^arthas! !

!EstrategiasTest methodsFor: 'combatientes' stamp: 'SF 10/11/2022 22:04:48'!
combatienteOrcoMankrik
	^mankrik! !

!EstrategiasTest methodsFor: 'combatientes' stamp: 'SF 10/11/2022 22:04:54'!
combatienteOrcoOlgra
	^olgra! !


!EstrategiasTest methodsFor: 'tests' stamp: 'JMB 10/18/2022 21:34:26'!
test01FocalizarEnElMasHeridoEsMejorQueEnElMasSano
	
	self assert: self puntosDeVidaDelGanadorFocalizandoEnElMasHerido > self puntosDeVidaDelGanadorFocalizandoEnElMasSano.! !


!EstrategiasTest methodsFor: 'combates' stamp: 'JMB 3/4/2023 12:58:02'!
orquestadorParaArthasUsandoUnHachaYConFocoEnElMasHeridoVsOrcos
	
	self prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasHerido. 
	self combatienteHumanoArthas equiparConUnHacha.
	^OrquestadorDeCombates
		establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
! !

!EstrategiasTest methodsFor: 'combates' stamp: 'JMB 3/4/2023 12:58:02'!
orquestadorParaArthasUsandoUnHachaYConFocoEnElMasSanoVsOrcos
	
	self prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasSano. 
	self combatienteHumanoArthas equiparConUnHacha.
	^OrquestadorDeCombates
			establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
! !

!EstrategiasTest methodsFor: 'combates' stamp: 'SF 10/10/2022 18:32:49'!
puntosDeVidaDelGanadorFocalizandoEnElMasHerido
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaArthasUsandoUnHachaYConFocoEnElMasHeridoVsOrcos.
	orquestadorDeCombates desarrollarDuranteRondas: 100.
	^ self combatienteHumanoArthas puntosDeVida! !

!EstrategiasTest methodsFor: 'combates' stamp: 'SF 10/10/2022 18:32:49'!
puntosDeVidaDelGanadorFocalizandoEnElMasSano
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaArthasUsandoUnHachaYConFocoEnElMasSanoVsOrcos.
	orquestadorDeCombates desarrollarDuranteRondas: 100.
	^ self combatienteHumanoArthas puntosDeVida! !


!EstrategiasTest methodsFor: 'private' stamp: 'JMB 3/5/2023 21:59:55'!
prepararCombatientes
	arthas := Combatiente humanoide.
	olgra := Combatiente humanoide.
	mankrik := Combatiente humanoide.! !


!classDefinition: #OrquestadorDeCombatesTest category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
TestCase subclass: #OrquestadorDeCombatesTest
	instanceVariableNames: 'arthas olgra mankrik'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'SF 10/10/2022 19:37:28'!
orquestadorParaCombateArthasConFocoEnElMasSanoVsOrcos	
	
	self prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasSano. 
	^self establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'JMB 3/4/2023 12:56:47'!
orquestadorParaCombateArthasInmuneYFocoEnElMasHeridoVsOrcos	
	
	self prepararCombatientes.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	self combatienteHumanoArthas equiparConUnaArmaduraDeMetal. 
	self combatienteHumanoArthas focalizarEnElMasHerido. 
	^self establecerCombateEntreBando1:  {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'SF 10/9/2022 12:55:39'!
orquestadorParaCombateArthasVsOlgra	
	
	self prepararCombatientes. 
	^OrquestadorDeCombates establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra }.
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'SF 10/11/2022 21:54:06'!
orquestadorParaCombateArthasVsOrcos
	self prepararCombatientes.
	^self establecerCombateEntreBando1: { self combatienteHumanoArthas } contraBando2: {self combatienteOrcoOlgra. self combatienteOrcoMankrik }.
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'JMB 3/4/2023 12:57:31'!
orquestadorParaCombateManoAManoConBando2Inmune	
	
	self prepararCombatientes.
	self combatienteOrcoOlgra equiparConUnaEspada.
	^self establecerCombateEntreBando1: {self combatienteHumanoArthas } contraBando2: { self combatienteOrcoOlgra }.
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'SF 10/10/2022 19:46:34'!
orquestadorParaCombateOrcosVsArthas
	self prepararCombatientes.
	^self establecerCombateEntreBando1: {self combatienteOrcoOlgra. self combatienteOrcoMankrik } contraBando2: { self combatienteHumanoArthas }.
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'SF 10/11/2022 21:53:53'!
orquestadorParaCombateOrcosVsArthasConFocoEnElMasSano	
	
	self prepararCombatientes.
	self combatienteHumanoArthas focalizarEnElMasSano. 
	^self establecerCombateEntreBando1: { self combatienteOrcoOlgra. self combatienteOrcoMankrik } contraBando2: {self combatienteHumanoArthas }. 
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'JMB 3/4/2023 12:56:47'!
orquestadorParaCombateOrcosVsArthasInmuneYFocoEnElMasHerido	
	
	self prepararCombatientes.
	self combatienteHumanoArthas atacarA: self combatienteOrcoOlgra.
	self combatienteHumanoArthas equiparConUnaArmaduraDeMetal. 
	self combatienteHumanoArthas focalizarEnElMasHerido. 
	^self establecerCombateEntreBando1: { self combatienteOrcoOlgra. self combatienteOrcoMankrik } contraBando2: {self combatienteHumanoArthas }.
! !

!OrquestadorDeCombatesTest methodsFor: 'combates' stamp: 'JMB 3/5/2023 21:58:31'!
prepararCombatientes

	arthas := Combatiente humanoide. 
	olgra := Combatiente humanoide. 
	mankrik := Combatiente humanoide. ! !


!OrquestadorDeCombatesTest methodsFor: 'tests - mano a mano' stamp: 'JMB 10/18/2022 21:35:14'!
test01AlDesarrollarUnCombateSinRondasLosCombatientesNoSeAtacan
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	orquestadorDeCombates desarrollarDuranteRondas: 0.
	
	self assert: self combatienteHumanoArthas noEstaHerido.
	self assert: self combatienteOrcoOlgra noEstaHerido.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - mano a mano' stamp: 'JMB 10/18/2022 21:35:32'!
test02AlDesarrollarUnCombateManoAManoDeUnaRondaLosBandosSeAtacanUnaVez
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	orquestadorDeCombates desarrollarDuranteRondas: 1.
	
	self assert: (self combatienteHumanoArthas tienePuntosDeVida: 18*pv) description: 'Arthas debe recibir un puñetazo (2 de daño)'.
	self assert: (self combatienteOrcoOlgra tienePuntosDeVida: 18*pv) description: 'Olgra debe recibir un puñetazo (2 de daño)'.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - mano a mano' stamp: 'JMB 10/18/2022 21:35:47'!
test03AlDesarrollarUnCombateManoAManoConMasDeUnaRondaLosBandosSeAtacanCadaVez
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	orquestadorDeCombates desarrollarDuranteRondas: 2.
	
	self assert: (self combatienteHumanoArthas tienePuntosDeVida: 16*pv) description: 'Arthas debe recibir dos puñetazos (4 de daño)'.
	self assert: (self combatienteOrcoOlgra tienePuntosDeVida: 16*pv) description: 'Olgra debe recibir dos puñetazos (4 de daño)'.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - mano a mano' stamp: 'SF 10/9/2022 12:33:46'!
test04AlDesarrollarUnCombateManoAManoSinGanadoresElResultadoEsIndeterminado
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 0.
	
	self assertElResultadoIndicaQueNoHayGanador: resultado alcanzadaLaRonda: 0.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - mano a mano' stamp: 'SF 10/9/2022 12:33:45'!
test05AlDesarrollarUnCombateManoAManoGanaElBando1CuandoDejaFueraDeCombateASuRival
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 10.
	
	self assertElResultadoIndicaQueGanoElBando1: resultado alcanzadaLaRonda: 10.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - mano a mano' stamp: 'SF 10/9/2022 12:33:45'!
test06AlDesarrollarUnCombateCuandoHayUnGanadorFinalizaEnLaRondaQueElPerdedorQuedoFueraDeCombate
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOlgra.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 11.
	
	self assertElResultadoIndicaQueGanoElBando1: resultado alcanzadaLaRonda: 10.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - mano a mano' stamp: 'SF 10/9/2022 12:33:46'!
test07AlDesarrollarUnCombateManoAManoGanaElBando2CuandoDejaFueraDeCombateASuRival
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateManoAManoConBando2Inmune.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 100.
	
	self assertElResultadoIndicaQueGanoElBando2: resultado alcanzadaLaRonda: 3.! !


!OrquestadorDeCombatesTest methodsFor: 'tests - muches vs muches' stamp: 'JMB 10/18/2022 21:36:08'!
test08AlDesarrollarUnCombateLosDelBando1PuedenAtacarACualquieraDelBando2
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateArthasConFocoEnElMasSanoVsOrcos.
	
	orquestadorDeCombates desarrollarDuranteRondas: 2.
	
	self assert: self combatienteOrcoOlgra estaHerido. 
	self assert: self combatienteOrcoMankrik estaHerido. ! !

!OrquestadorDeCombatesTest methodsFor: 'tests - muches vs muches' stamp: 'SF 10/11/2022 21:53:23'!
test09AlDesarrollarUnCombateAtacanTodesLosDelBando1EnCadaRonda
	"
		En cada ronda cada combatiente daña al que esta solo por 2 de daño cada uno
	"
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateOrcosVsArthas.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 5.
	
	self assertElResultadoIndicaQueGanoElBando1: resultado alcanzadaLaRonda: 5.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - muches vs muches' stamp: 'JMB 10/18/2022 21:36:18'!
test10AlDesarrollarUnCombateLosDelBando2PuedenAtacarACualquieraDelBando1
	
	| orquestadorDeCombates |
	orquestadorDeCombates := self orquestadorParaCombateOrcosVsArthasConFocoEnElMasSano.
	
	orquestadorDeCombates desarrollarDuranteRondas: 2.
	
	self assert: self combatienteOrcoOlgra estaHerido. 
	self assert: self combatienteOrcoMankrik estaHerido. ! !

!OrquestadorDeCombatesTest methodsFor: 'tests - muches vs muches' stamp: 'SF 10/11/2022 21:55:18'!
test11AlDesarrollarUnCombateAtacanTodesLosDelBando2EnCadaRonda
	"
		En cada ronda cada combatiente daña en 2 al del otro bando. 4 de daño en total por turno.
	"
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasVsOrcos.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 5.
	
	self assertElResultadoIndicaQueGanoElBando2: resultado alcanzadaLaRonda: 5.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - muches vs muches' stamp: 'SF 10/9/2022 12:33:46'!
test12AlDesarrollarUnCombateLosDelBando1NoGananSiTodosLosDelBando2NoEstanFueraDeCombate
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateArthasInmuneYFocoEnElMasHeridoVsOrcos.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 10.
	
	self assertElResultadoIndicaQueNoHayGanador: resultado alcanzadaLaRonda: 10.! !

!OrquestadorDeCombatesTest methodsFor: 'tests - muches vs muches' stamp: 'SF 10/9/2022 12:33:46'!
test13AlDesarrollarUnCombateLosDelBando2NoGananSiTodosLosDelBando1NoEstanFueraDeCombate
	
	| orquestadorDeCombates resultado |
	orquestadorDeCombates := self orquestadorParaCombateOrcosVsArthasInmuneYFocoEnElMasHerido.
	
	resultado := orquestadorDeCombates desarrollarDuranteRondas: 10.
	
	self assertElResultadoIndicaQueNoHayGanador: resultado alcanzadaLaRonda: 10.! !


!OrquestadorDeCombatesTest methodsFor: 'para completar' stamp: 'JMB 10/18/2022 21:36:30'!
assertElResultadoIndicaQueGanoElBando1: resultadoDeUnCombate alcanzadaLaRonda: unNumeroDeRondaEsperado

	self assert: (resultadoDeUnCombate tieneDeGanadorAlBando1Realizando: unNumeroDeRondaEsperado)! !

!OrquestadorDeCombatesTest methodsFor: 'para completar' stamp: 'JMB 10/18/2022 21:36:36'!
assertElResultadoIndicaQueGanoElBando2: resultadoDeUnCombate alcanzadaLaRonda: unNumeroDeRondaEsperado

	self assert: (resultadoDeUnCombate tieneDeGanadorAlBando2Realizando: unNumeroDeRondaEsperado)! !

!OrquestadorDeCombatesTest methodsFor: 'para completar' stamp: 'JMB 10/18/2022 21:36:41'!
assertElResultadoIndicaQueNoHayGanador: resultadoDeUnCombate  alcanzadaLaRonda: unNumeroDeRondaEsperado

	self assert:  (resultadoDeUnCombate finalizoSinGanadoresRealizando: unNumeroDeRondaEsperado)! !

!OrquestadorDeCombatesTest methodsFor: 'para completar' stamp: 'SF 10/10/2022 18:42:57'!
establecerCombateEntreBando1: unBando1 contraBando2: unBando2

	^OrquestadorDeCombates establecerCombateEntreBando1: unBando1 contraBando2: unBando2.! !


!OrquestadorDeCombatesTest methodsFor: 'combatientes' stamp: 'SF 10/10/2022 19:26:44'!
combatienteHumanoArthas

	^arthas.! !

!OrquestadorDeCombatesTest methodsFor: 'combatientes' stamp: 'SF 10/10/2022 19:26:34'!
combatienteOrcoMankrik

	^mankrik.! !

!OrquestadorDeCombatesTest methodsFor: 'combatientes' stamp: 'SF 10/10/2022 19:26:39'!
combatienteOrcoOlgra

	^olgra.! !


!classDefinition: #Combatiente category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Object subclass: #Combatiente
	instanceVariableNames: 'puntosDeVida puntosDeVidaIniciales enemigos estrategia dañoAAbsorber dañoAInfligir dañoAInfligirPorArma aliados'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 13:23:58'!
equiparConUnHacha
	dañoAInfligirPorArma := 10 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 13:23:58'!
equiparConUnMartillo
	dañoAInfligirPorArma := 8 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 13:23:58'!
equiparConUnPalo
	dañoAInfligirPorArma _ 6 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 12:56:21'!
equiparConUnaArmaduraDeAnillos
	dañoAAbsorber _ 4 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 12:56:34'!
equiparConUnaArmaduraDeCuero
	dañoAAbsorber _ 2 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 12:56:47'!
equiparConUnaArmaduraDeMetal
	dañoAAbsorber := 6 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 13:23:58'!
equiparConUnaDaga
	dañoAInfligirPorArma _ 4 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 13:23:58'!
equiparConUnaEspada
	dañoAInfligirPorArma := 8 * pv.! !

!Combatiente methodsFor: 'equipo' stamp: 'JMB 3/4/2023 13:23:58'!
equiparConUnaLanza
	dañoAInfligirPorArma := 8 * pv.! !


!Combatiente methodsFor: 'private' stamp: 'NT 5/16/2023 19:55:25'!
dañoAInfligir
	self subclassResponsibility.! !

!Combatiente methodsFor: 'private' stamp: 'SF 7/13/2022 22:24:44'!
puntosDeVida
	^puntosDeVida! !

!Combatiente methodsFor: 'private' stamp: 'NT 5/16/2023 19:39:49'!
puntosDeVidaIniciales
	^puntosDeVidaIniciales.! !


!Combatiente methodsFor: 'estrategia' stamp: 'SF 10/15/2022 18:28:31'!
estrategiaACualquiera

	^ [:candidatos | candidatos anyOne]! !

!Combatiente methodsFor: 'estrategia' stamp: 'SF 8/21/2022 10:33:13'!
focalizarEnElMasHerido
	estrategia := [:candidatos | candidatos detectMin: [:enemigo | enemigo puntosDeVida]].! !

!Combatiente methodsFor: 'estrategia' stamp: 'SF 8/21/2022 10:39:02'!
focalizarEnElMasSano
	estrategia := [:candidatos | candidatos detectMax: [:enemigo | enemigo puntosDeVida]].! !


!Combatiente methodsFor: 'combate' stamp: 'NT 5/16/2023 21:18:23'!
aliados: luchadoresAliados 
	aliados := luchadoresAliados.! !

!Combatiente methodsFor: 'combate' stamp: 'NT 5/16/2023 20:20:29'!
atacarA: unCombatiente
	self subclassResponsibility.! !

!Combatiente methodsFor: 'combate' stamp: 'SF 8/20/2022 11:56:44'!
enemigos: unConjuntoDeEnemigos
	enemigos := unConjuntoDeEnemigos! !

!Combatiente methodsFor: 'combate' stamp: 'SF 10/10/2022 11:06:47'!
estaFueraDeCombate
	^puntosDeVida <= (0*pv).! !

!Combatiente methodsFor: 'combate' stamp: 'JMB 3/4/2023 20:27:11'!
estaHerido
	^puntosDeVida < self puntosDeVidaIniciales! !

!Combatiente methodsFor: 'combate' stamp: 'SF 10/9/2022 19:18:00'!
noEstaHerido
	^self estaHerido not.! !

!Combatiente methodsFor: 'combate' stamp: 'NT 5/16/2023 21:19:12'!
recibirCuracion: unaCantidadDePuntosDeVidaACurar
	
	puntosDeVida := puntosDeVida + unaCantidadDePuntosDeVidaACurar.
	puntosDeVida := puntosDeVida min: self puntosDeVidaIniciales.! !

!Combatiente methodsFor: 'combate' stamp: 'NT 5/16/2023 21:44:00'!
recibirDaño: unaCantidadDeDañoARecibir  
	| dañoADescontar |
	dañoADescontar := (unaCantidadDeDañoARecibir - dañoAAbsorber) max: 0*pv.
	puntosDeVida := puntosDeVida - dañoADescontar.! !

!Combatiente methodsFor: 'combate' stamp: 'SF 10/9/2022 12:56:52'!
tienePuntosDeVida: unaCantidadDePuntosDeVida
	^puntosDeVida = unaCantidadDePuntosDeVida.! !

!Combatiente methodsFor: 'combate' stamp: 'NT 5/16/2023 21:31:50'!
usarPoder
	self subclassResponsibility.
	! !


!Combatiente methodsFor: 'initialization' stamp: 'NT 5/16/2023 21:36:56'!
initializeConPuntosDeVida: unosPuntosDeVida yDañoAInfligir: unDañoAInfligir yDañoAInfligirPorArma: unDañoAInfligirPorArma
	
	puntosDeVidaIniciales := unosPuntosDeVida*pv.
	puntosDeVida := unosPuntosDeVida*pv.
	enemigos := OrderedCollection new.
	estrategia := self estrategiaACualquiera.
	dañoAInfligir := unDañoAInfligir*pv.
	dañoAInfligirPorArma := unDañoAInfligirPorArma*pv.
	dañoAAbsorber _ 0.	
	aliados := #().! !


!Combatiente methodsFor: 'printing' stamp: 'JMB 3/4/2023 13:42:41'!
printOn: aStream
	aStream 
		nextPutAll: 'un'; 
		space;
		nextPutAll: self class name;
		nextPutAll: ' ('.
	
	puntosDeVida printOn: aStream.
	aStream nextPut: $/.
	self puntosDeVidaIniciales printOn: aStream.
	aStream nextPut: $).! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Combatiente class' category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Combatiente class
	instanceVariableNames: ''!

!Combatiente class methodsFor: 'instance creation' stamp: 'NT 5/16/2023 19:57:47'!
curador
	^Curador new initializeConPuntosDeVida: 20 yDañoAInfligir: 0 yDañoAInfligirPorArma: 2.! !

!Combatiente class methodsFor: 'instance creation' stamp: 'NT 5/16/2023 19:27:10'!
dragon
	^Dragon new initializeConPuntosDeVida: 200 yDañoAInfligir: 8 yDañoAInfligirPorArma: 0.! !

!Combatiente class methodsFor: 'instance creation' stamp: 'NT 5/16/2023 19:27:28'!
humanoide
	^Humanoide new initializeConPuntosDeVida: 20 yDañoAInfligir: 0 yDañoAInfligirPorArma: 2.! !

!Combatiente class methodsFor: 'instance creation' stamp: 'NT 5/16/2023 19:27:51'!
ilusionista
	^Ilusionista new initializeConPuntosDeVida: 20 yDañoAInfligir: 0 yDañoAInfligirPorArma: 2.! !

!Combatiente class methodsFor: 'instance creation' stamp: 'NT 5/16/2023 19:28:19'!
ogro
	^Ogro new initializeConPuntosDeVida: 60 yDañoAInfligir: 20 yDañoAInfligirPorArma: 0.! !


!classDefinition: #Curador category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Combatiente subclass: #Curador
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!Curador methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 20:23:53'!
atacarA: unCombatiente
	self estaFueraDeCombate ifTrue: [^self].

	unCombatiente recibirDaño: self dañoAInfligir.! !

!Curador methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 19:30:49'!
dañoAInfligir
	^dañoAInfligirPorArma.! !

!Curador methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 21:31:50'!
usarPoder
	self estaFueraDeCombate ifTrue: [^self].
	
	aliados do: [ :compañero | compañero recibirCuracion: 2*pv].
	
	^self.! !


!classDefinition: #Dragon category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Combatiente subclass: #Dragon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!Dragon methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 20:23:37'!
atacarA: unCombatiente
	self estaFueraDeCombate ifTrue: [^self].

	unCombatiente recibirDaño: self dañoAInfligir.! !

!Dragon methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 19:31:25'!
dañoAInfligir
	^dañoAInfligir.! !

!Dragon methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 21:31:50'!
usarPoder
	| enemigosAAtacar enemigosActivos | 
	self estaFueraDeCombate ifTrue: [^self].
	enemigosActivos := enemigos select: [:enemigo | enemigo estaFueraDeCombate not].
	enemigosActivos isEmpty ifTrue: [^self].
	
	enemigosAAtacar := enemigosActivos firstAvailable: 8.
	
	enemigosAAtacar do: [ :enemigoAAtacar | self atacarA: enemigoAAtacar ].
	
	^self.! !


!classDefinition: #Humanoide category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Combatiente subclass: #Humanoide
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!Humanoide methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 21:43:42'!
atacarA: unCombatiente
	self estaFueraDeCombate ifTrue: [^self].

	unCombatiente recibirDaño: self dañoAInfligir.! !

!Humanoide methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 19:31:12'!
dañoAInfligir
	^dañoAInfligirPorArma.! !

!Humanoide methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 21:43:38'!
usarPoder
	| contrincanteSeleccionado enemigosActivos |
	
	self estaFueraDeCombate ifTrue: [^self].
	enemigosActivos := enemigos select: [:enemigo | enemigo estaFueraDeCombate not].
	enemigosActivos isEmpty ifTrue: [^self].
	
	contrincanteSeleccionado := estrategia value: enemigosActivos.
	self atacarA: contrincanteSeleccionado.
	^self.! !


!classDefinition: #Ilusionista category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Combatiente subclass: #Ilusionista
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!Ilusionista methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 20:20:07'!
atacarA: unCombatiente
	| victima aliadosDeUnCombatienteASerAtacado |
	
	aliadosDeUnCombatienteASerAtacado := enemigos select: [ :enemigo | (enemigo = unCombatiente)not ].
	aliadosDeUnCombatienteASerAtacado ifEmpty: [^ self].
	
	victima :=  aliadosDeUnCombatienteASerAtacado anyOne.
	unCombatiente atacarA: victima.
	
	^ self.! !

!Ilusionista methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 19:40:38'!
dañoAInfligir! !

!Ilusionista methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 21:31:50'!
usarPoder
	| contrincanteSeleccionado enemigosActivos |
	self estaFueraDeCombate ifTrue: [^self].
	
	enemigosActivos := enemigos select: [:enemigo | enemigo estaFueraDeCombate not].
	enemigosActivos isEmpty ifTrue: [^self].

	contrincanteSeleccionado := estrategia value: enemigosActivos.
	self atacarA: contrincanteSeleccionado.
	^self.! !


!classDefinition: #Ogro category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Combatiente subclass: #Ogro
	instanceVariableNames: 'yaAtaco'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!Ogro methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 20:07:01'!
atacarA: unCombatiente
	self estaFueraDeCombate ifTrue: [^self].

	unCombatiente recibirDaño: self dañoAInfligir.! !

!Ogro methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 19:31:34'!
dañoAInfligir
	^dañoAInfligir.! !

!Ogro methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 21:36:35'!
initialize
	yaAtaco := false.! !

!Ogro methodsFor: 'as yet unclassified' stamp: 'NT 5/16/2023 21:37:31'!
usarPoder
	| contrincanteSeleccionado enemigosActivos |
	
	self estaFueraDeCombate ifTrue: [^self].
	
	enemigosActivos := enemigos select: [:enemigo | enemigo estaFueraDeCombate not].
	enemigosActivos isEmpty ifTrue: [^self].

	
	yaAtaco ifTrue: [
		contrincanteSeleccionado := estrategia value: enemigosActivos.
		self atacarA: contrincanteSeleccionado.
		yaAtaco := false.
		^self.
		].
	yaAtaco := true.
	^self.
	! !


!classDefinition: #OrquestadorDeCombates category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Object subclass: #OrquestadorDeCombates
	instanceVariableNames: 'bando1 bando2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!OrquestadorDeCombates methodsFor: 'initializing' stamp: 'SF 10/9/2022 12:50:01'!
initializeEstablecerCombateEntreBando1: unConjuntoDeCombatientes contraBando2: otroConjuntoDeCombatientes
	bando1 := unConjuntoDeCombatientes.
	bando2 := otroConjuntoDeCombatientes.! !


!OrquestadorDeCombates methodsFor: 'public' stamp: 'NT 5/16/2023 21:31:51'!
desarrollarDuranteRondas: maximoDeRondas
	| rondaActual |
	rondaActual := 1.
	[rondaActual <= maximoDeRondas and: [self noHayGanador]] whileTrue: [
		bando1 do: [ :combatiente | combatiente usarPoder].
		bando2 do: [ :combatiente | combatiente usarPoder].
		rondaActual := rondaActual + 1.
	].
	rondaActual := rondaActual - 1.
	self esBando1Ganador ifTrue: [^ResultadoDeCombate conGanadorBando1EnRonda: rondaActual].
	self esBando2Ganador ifTrue: [^ResultadoDeCombate conGanadorBando2EnRonda: rondaActual].
	^ResultadoDeCombate indeterminadoEnRonda: rondaActual.! !


!OrquestadorDeCombates methodsFor: 'private' stamp: 'SF 10/10/2022 11:02:11'!
esBando1Ganador
	^(self hayAlMenosUnCombatienteEnCombateEn: bando1) and: [self todosEstanFueraDeCombateEn: bando2].
	! !

!OrquestadorDeCombates methodsFor: 'private' stamp: 'SF 10/10/2022 11:19:32'!
esBando2Ganador
	^(self hayAlMenosUnCombatienteEnCombateEn: bando2) and: [self todosEstanFueraDeCombateEn: bando1].! !

!OrquestadorDeCombates methodsFor: 'private' stamp: 'SF 10/10/2022 11:03:52'!
hayAlMenosUnCombatienteEnCombateEn: unConjuntoDeCombatientes
	^unConjuntoDeCombatientes anySatisfy: [:combatiente | combatiente estaFueraDeCombate not].! !

!OrquestadorDeCombates methodsFor: 'private' stamp: 'SF 10/10/2022 11:18:59'!
noHayGanador
	^self esBando1Ganador not and: [self esBando2Ganador not].! !

!OrquestadorDeCombates methodsFor: 'private' stamp: 'SF 10/10/2022 11:07:32'!
todosEstanFueraDeCombateEn: unConjuntoDeCombatientes
	^unConjuntoDeCombatientes allSatisfy: [:combatiente | combatiente estaFueraDeCombate].! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'OrquestadorDeCombates class' category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
OrquestadorDeCombates class
	instanceVariableNames: ''!

!OrquestadorDeCombates class methodsFor: 'instance creation' stamp: 'SF 2/24/2023 21:20:26'!
establecerCombateEntreBando1: bando1 contraBando2: bando2
	self relacionar: bando1 con: bando2.
	^self new initializeEstablecerCombateEntreBando1: bando1 contraBando2: bando2 ! !


!OrquestadorDeCombates class methodsFor: 'as yet unclassified' stamp: 'SF 10/9/2022 19:29:24'!
enemistar: unBandoDeCombatientes con: otroBandoDeCombatientes
	unBandoDeCombatientes do: [ :combatiente | combatiente enemigos: otroBandoDeCombatientes ].
	otroBandoDeCombatientes do: [ :combatiente | combatiente enemigos: unBandoDeCombatientes ].! !

!OrquestadorDeCombates class methodsFor: 'as yet unclassified' stamp: 'SF 2/24/2023 21:20:26'!
relacionar: unBandoDeCombatientes con: otroBandoDeCombatientes
	unBandoDeCombatientes do: [ :combatiente | 
		combatiente enemigos: otroBandoDeCombatientes.
		combatiente aliados: unBandoDeCombatientes.].
	otroBandoDeCombatientes do: [ :combatiente | 
		combatiente enemigos: unBandoDeCombatientes.
		combatiente aliados: otroBandoDeCombatientes].! !


!classDefinition: #ResultadoDeCombate category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
Object subclass: #ResultadoDeCombate
	instanceVariableNames: 'numeroDeRonda ganador'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!

!ResultadoDeCombate methodsFor: 'public' stamp: 'JMB 3/4/2023 13:03:59'!
finalizoSinGanadores

	^ ganador = self elGanadorEsIndeterminado! !

!ResultadoDeCombate methodsFor: 'public' stamp: 'JMB 3/4/2023 13:03:59'!
finalizoSinGanadoresRealizando: unaCantidadDeRondas
	^self finalizoSinGanadores and: [numeroDeRonda = unaCantidadDeRondas].! !

!ResultadoDeCombate methodsFor: 'public' stamp: 'SF 2/21/2023 13:51:09'!
tieneDeGanadorAlBando1
	^self elBando1Ganó = ganador.! !

!ResultadoDeCombate methodsFor: 'public' stamp: 'SF 2/21/2023 13:51:45'!
tieneDeGanadorAlBando1Realizando: unaCantidadDeRondas
	^self tieneDeGanadorAlBando1 and: [numeroDeRonda = unaCantidadDeRondas].! !

!ResultadoDeCombate methodsFor: 'public' stamp: 'SF 2/21/2023 14:49:32'!
tieneDeGanadorAlBando2
	^self elBando2Ganó = ganador.! !

!ResultadoDeCombate methodsFor: 'public' stamp: 'SF 2/21/2023 14:50:12'!
tieneDeGanadorAlBando2Realizando: unaCantidadDeRondas
	^self tieneDeGanadorAlBando2 and: [numeroDeRonda = unaCantidadDeRondas].! !


!ResultadoDeCombate methodsFor: 'initialization' stamp: 'SF 10/10/2022 11:10:22'!
initializeConGanadorBando1EnRonda: unNumeroDeRonda 
	ganador := self elBando1Ganó.
	numeroDeRonda := unNumeroDeRonda.! !

!ResultadoDeCombate methodsFor: 'initialization' stamp: 'SF 10/10/2022 19:02:08'!
initializeConGanadorBando2EnRonda: unNumeroDeRonda 
	ganador := self elBando2Ganó.
	numeroDeRonda := unNumeroDeRonda.! !

!ResultadoDeCombate methodsFor: 'initialization' stamp: 'SF 10/10/2022 11:15:24'!
initializeIndeterminadoEnRonda: unaCantidadDeRondas
	ganador := self elGanadorEsIndeterminado.
	numeroDeRonda := unaCantidadDeRondas.! !


!ResultadoDeCombate methodsFor: 'private' stamp: 'SF 10/10/2022 11:10:22'!
elBando1Ganó

	^ 'bando1'! !

!ResultadoDeCombate methodsFor: 'private' stamp: 'SF 10/10/2022 18:50:09'!
elBando2Ganó

	^ 'bando2'! !

!ResultadoDeCombate methodsFor: 'private' stamp: 'SF 10/10/2022 11:13:38'!
elGanadorEsIndeterminado
	^'indeterminado'.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ResultadoDeCombate class' category: 'AlgoIII-2023-1C-1Parcial-CombatientesFantasticos-P3'!
ResultadoDeCombate class
	instanceVariableNames: ''!

!ResultadoDeCombate class methodsFor: 'instance creation' stamp: 'SF 10/10/2022 11:08:18'!
conGanadorBando1EnRonda: unNumeroDeRonda
	^self new initializeConGanadorBando1EnRonda: unNumeroDeRonda! !

!ResultadoDeCombate class methodsFor: 'instance creation' stamp: 'SF 10/10/2022 18:57:31'!
conGanadorBando2EnRonda: aSmallInteger 
	^self new initializeConGanadorBando2EnRonda: aSmallInteger ! !

!ResultadoDeCombate class methodsFor: 'instance creation' stamp: 'SF 10/10/2022 11:12:29'!
indeterminadoEnRonda: unaCantidadDeRondas
	^self new initializeIndeterminadoEnRonda: unaCantidadDeRondas! !

CombatientesTest initialize!