!classDefinition: #MarsRoverTest category: 'MarsRover'!
TestCase subclass: #MarsRoverTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MarsRover'!

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
assertQueUnRoverConPosicionInicial: unaPosicionInicial yUnaDireccionInicial: unaDireccionInicial realizaLosComandos: comandosARealizar yFinalizaEnLaPosicion: posicionEsperada yEnDireccion: direccionEsperada
	|rover|
	
	rover := MarsRover conPosicion: unaPosicionInicial yApuntandoEnDireccion: unaDireccionInicial.
	
	rover mover: comandosARealizar.
	
	self assert: (rover estaEnPosicion: posicionEsperada) equals: true.
	self assert: (rover apuntaEnDireccion: direccionEsperada) equals: true.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:26:30'!
test01SiSeEnviaUnComandoInvalidoSeLevantaUnError
	|rover movimientos|
	
	rover := MarsRover conPosicion: 0@0 yApuntandoEnDireccion: Norte new.
	movimientos := 'ffjkaq'.
	
	self
		should: [ rover mover: movimientos ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = 'Comando invalido' ].! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:41:00'!
test02SiSeEnvianComandosVaciosNoHaceNada
	
	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Norte new realizaLosComandos: '' yFinalizaEnLaPosicion: 0@0 yEnDireccion: Norte new.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
test03SiSeEnvianXComandosHaciaDelanteSeAvanzanXPosiciones

	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Norte new realizaLosComandos: 'ffffff' yFinalizaEnLaPosicion: 0@6 yEnDireccion: Norte new.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
test04SiSeEnvianXComandosHaciaAtrasSeRetrocedenXPosiciones
	
	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Norte new realizaLosComandos: 'bbbbbb' yFinalizaEnLaPosicion: 0@-6 yEnDireccion: Norte new.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
test05SiSeEnvianXComandosHaciaLaIzquierdaSeActualizaLaDireccionCorrectamente
	
	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Norte new realizaLosComandos: 'lllll' yFinalizaEnLaPosicion: 0@0 yEnDireccion: Oeste new.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
test06SiSeEnvianXComandosHaciaLaDerechaSeActualizaLaDireccionCorrectamente

	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Norte new realizaLosComandos: 'rrrrr' yFinalizaEnLaPosicion: 0@0 yEnDireccion: Este new.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
test07SiSeApuntaEnDireccionSSeAvanzaApuntandoAlSur
	
	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Sur new realizaLosComandos: 'f' yFinalizaEnLaPosicion: 0@-1 yEnDireccion: Sur new.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
test08SiSeApuntaEnDireccionESeAvanzaApuntandoAlE
	
	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Este new realizaLosComandos: 'f' yFinalizaEnLaPosicion: 1@0 yEnDireccion: Este new.! !

!MarsRoverTest methodsFor: 'testing' stamp: 'nt 5/28/2023 18:38:39'!
test09SiSeApuntaEnDireccionOSeAvanzaApuntandoAlO
	
	self assertQueUnRoverConPosicionInicial: 0@0 yUnaDireccionInicial: Oeste new realizaLosComandos: 'f' yFinalizaEnLaPosicion: -1@0 yEnDireccion: Oeste new.! !


!classDefinition: #Direccion category: 'MarsRover'!
Object subclass: #Direccion
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MarsRover'!

!Direccion methodsFor: 'comparing' stamp: 'nt 5/28/2023 18:06:02'!
= unaDireccion
	^(unaDireccion isKindOf: self class).! !


!Direccion methodsFor: 'operaciones' stamp: 'nt 5/28/2023 17:59:47'!
direccionADerecha
	self subclassResponsibility.! !

!Direccion methodsFor: 'operaciones' stamp: 'nt 5/28/2023 18:00:04'!
direccionAIzquierda
	self subclassResponsibility.! !

!Direccion methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:16:59'!
distanciaALaPosicionAEspalda
	self subclassResponsibility.! !

!Direccion methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:17:38'!
distanciaALaPosicionAlFrente
	self subclassResponsibility.! !


!classDefinition: #Este category: 'MarsRover'!
Direccion subclass: #Este
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MarsRover'!

!Este methodsFor: 'operaciones' stamp: 'nt 5/28/2023 17:59:47'!
direccionADerecha
	^Sur new.! !

!Este methodsFor: 'operaciones' stamp: 'nt 5/28/2023 18:00:04'!
direccionAIzquierda
	^Norte new.! !

!Este methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:16:59'!
distanciaALaPosicionAEspalda
	^(-1@0).! !

!Este methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:17:38'!
distanciaALaPosicionAlFrente
	^ (1@0).! !


!classDefinition: #Norte category: 'MarsRover'!
Direccion subclass: #Norte
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MarsRover'!

!Norte methodsFor: 'operaciones' stamp: 'nt 5/28/2023 17:59:47'!
direccionADerecha
	^Este new.! !

!Norte methodsFor: 'operaciones' stamp: 'nt 5/28/2023 18:00:04'!
direccionAIzquierda
	^Oeste new.! !

!Norte methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:16:59'!
distanciaALaPosicionAEspalda
	^(0@-1).! !

!Norte methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:17:38'!
distanciaALaPosicionAlFrente
	^(0@1).! !


!classDefinition: #Oeste category: 'MarsRover'!
Direccion subclass: #Oeste
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MarsRover'!

!Oeste methodsFor: 'operaciones' stamp: 'nt 5/28/2023 17:59:47'!
direccionADerecha
	^Norte new.! !

!Oeste methodsFor: 'operaciones' stamp: 'nt 5/28/2023 18:00:04'!
direccionAIzquierda
	^Sur new.! !

!Oeste methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:16:59'!
distanciaALaPosicionAEspalda
	^(1@0).! !

!Oeste methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:17:38'!
distanciaALaPosicionAlFrente
	^(-1@0).! !


!classDefinition: #Sur category: 'MarsRover'!
Direccion subclass: #Sur
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MarsRover'!

!Sur methodsFor: 'operaciones' stamp: 'nt 5/28/2023 17:59:47'!
direccionADerecha
	^Este new.! !

!Sur methodsFor: 'operaciones' stamp: 'nt 5/28/2023 18:00:04'!
direccionAIzquierda
	^Oeste new.! !

!Sur methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:16:59'!
distanciaALaPosicionAEspalda
	^(0@1).! !

!Sur methodsFor: 'operaciones' stamp: 'nt 5/30/2023 15:17:38'!
distanciaALaPosicionAlFrente
	^ (0@-1).! !


!classDefinition: #MarsRover category: 'MarsRover'!
Object subclass: #MarsRover
	instanceVariableNames: 'posicion direccion comandosValidos'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MarsRover'!

!MarsRover methodsFor: 'initialization' stamp: 'nt 5/28/2023 18:16:47'!
initializeConPosicion: unaPosicionInicial yApuntandoEnDireccion: unaDireccionInicial
	posicion := unaPosicionInicial.
	direccion := 	unaDireccionInicial.
	comandosValidos := Set with: $f with: $b with: $l with: $r.! !


!MarsRover methodsFor: 'testing' stamp: 'TB 5/28/2023 16:33:14'!
apuntaEnDireccion: direccionEsperada
	^(direccion = direccionEsperada).! !

!MarsRover methodsFor: 'testing' stamp: 'TB 5/28/2023 16:32:46'!
estaEnPosicion: posicionEsperada
	^(posicion = posicionEsperada).! !


!MarsRover methodsFor: 'movimiento' stamp: 'nt 5/28/2023 18:16:13'!
mover: comandosARealizar	
	(comandosARealizar allSatisfy: [ :unComando | (comandosValidos includes: unComando) ]) ifFalse: [self error: self class errorComandoInvalido ].
	
	comandosARealizar do: [ :unComando | 
		(unComando = $f) ifTrue: [ self avanzar ].
		(unComando = $b) ifTrue: [ self retroceder].
		(unComando = $l) ifTrue: [	self girarAIzquierda	].
		(unComando = $r) ifTrue: [ self girarADerecha].
		 ].! !


!MarsRover methodsFor: 'private' stamp: 'nt 5/30/2023 15:17:38'!
avanzar
	posicion := posicion + direccion distanciaALaPosicionAlFrente.
	! !

!MarsRover methodsFor: 'private' stamp: 'nt 5/28/2023 18:12:09'!
girarADerecha
	direccion := direccion direccionADerecha.! !

!MarsRover methodsFor: 'private' stamp: 'nt 5/28/2023 18:11:47'!
girarAIzquierda
	direccion := direccion direccionAIzquierda.! !

!MarsRover methodsFor: 'private' stamp: 'nt 5/30/2023 15:16:59'!
retroceder
	posicion := posicion + direccion distanciaALaPosicionAEspalda.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MarsRover class' category: 'MarsRover'!
MarsRover class
	instanceVariableNames: ''!

!MarsRover class methodsFor: 'initialization' stamp: 'TB 5/28/2023 16:30:12'!
conPosicion: unaPosicionInicial yApuntandoEnDireccion: unaDireccionInicial
	^self new initializeConPosicion: unaPosicionInicial yApuntandoEnDireccion: unaDireccionInicial.! !


!MarsRover class methodsFor: 'error' stamp: 'TB 5/28/2023 16:19:02'!
errorComandoInvalido
	^'Comando invalido'.! !
