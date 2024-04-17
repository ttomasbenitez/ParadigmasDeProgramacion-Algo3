!classDefinition: #CantSuspend category: 'CodigoRepetido-Ejercicio'!
Error subclass: #CantSuspend
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CodigoRepetido-Ejercicio'!


!classDefinition: #NotFound category: 'CodigoRepetido-Ejercicio'!
Error subclass: #NotFound
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CodigoRepetido-Ejercicio'!


!classDefinition: #CustomerBookTest category: 'CodigoRepetido-Ejercicio'!
TestCase subclass: #CustomerBookTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CodigoRepetido-Ejercicio'!

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:18:58'!
assertSobre: customerBook queLosUsuariosTotalesSonUsuariosActivos: expectedActiveCustomers yUsuariosSuspendidos: expectedSuspendedCustomers
	| expectedNumberOfCustomers |
	
	expectedNumberOfCustomers  := expectedActiveCustomers + expectedSuspendedCustomers.

	self assert: expectedActiveCustomers equals: customerBook numberOfActiveCustomers.
	self assert: expectedSuspendedCustomers equals: customerBook numberOfSuspendedCustomers.
	self assert: expectedNumberOfCustomers equals: customerBook numberOfCustomers.! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 15:19:33'!
assertSobreUn: customerBook realizaUnaAccion: unaAccion enNoMenosDe: unaCantidadDeMilisegundos
	| millisecondsBeforeRunning millisecondsAfterRunning |

	millisecondsBeforeRunning := Time millisecondClockValue * millisecond.
	unaAccion value: customerBook.
	millisecondsAfterRunning := Time millisecondClockValue * millisecond.
	
	self assert: (millisecondsAfterRunning-millisecondsBeforeRunning) < (unaCantidadDeMilisegundos * millisecond)	
! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 15:09:37'!
assertSobreUn: customerBook realizarUnaAccion: unaAccion levantaUnError: unError yVerificaQue: unAssert 

	[ unaAccion value: customerBook . self fail ]
		on: unError
		do: [ :anError | unAssert ].! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:24:54'!
crearYAgregaA: unNombre
	| customerBook |

	customerBook := CustomerBook new.
	customerBook addCustomerNamed: unNombre.
	^ customerBook.! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 15:19:44'!
test01AddingCustomerShouldNotTakeMoreThan50Milliseconds
	| customerBook |
	
	customerBook := CustomerBook new.

	self assertSobreUn: customerBook realizaUnaAccion: [ :unCustomerBook | unCustomerBook addCustomerNamed: 'John Lennon' ] enNoMenosDe: 50.

! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:33:27'!
test02RemovingCustomerShouldNotTakeMoreThan100Milliseconds
	| customerBook paulMcCartney |

	paulMcCartney := 'Paul McCartney'.
	customerBook := self crearYAgregaA: paulMcCartney.

	self assertSobreUn: customerBook realizaUnaAccion: [ :unCustomerBook | unCustomerBook removeCustomerNamed: paulMcCartney ] enNoMenosDe: 100.! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 15:10:19'!
test03CanNotAddACustomerWithEmptyName 
	| customerBook |
		
	customerBook := CustomerBook new.

	self assertSobreUn: customerBook 
	realizarUnaAccion: [ :unCustomerBook | unCustomerBook addCustomerNamed: '' ]
	levantaUnError: Error
	yVerificaQue: [ :anError | 
				self assert: anError messageText = CustomerBook customerCanNotBeEmptyErrorMessage.
				self assert: customerBook isEmpty ].! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:33:36'!
test04CanNotRemoveAnInvalidCustomer
	| customerBook johnLennon |

	johnLennon := 'John Lennon'.
	customerBook := self crearYAgregaA: johnLennon.
	
	self assertSobreUn: customerBook 
	realizarUnaAccion: [ :unCustomerBook | unCustomerBook removeCustomerNamed: 'Paul McCartney']
	levantaUnError: NotFound
	yVerificaQue: [ :anError | 
				self assert: customerBook numberOfCustomers = 1.
				self assert: (customerBook includesCustomerNamed: johnLennon) ]
! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:33:39'!
test05SuspendingACustomerShouldNotRemoveItFromCustomerBook
	| customerBook paulMcCartney|

	paulMcCartney := 'Paul McCartney'.
	customerBook := self crearYAgregaA: paulMcCartney.

	customerBook suspendCustomerNamed: paulMcCartney.
	
	self assertSobre: customerBook queLosUsuariosTotalesSonUsuariosActivos: 0 yUsuariosSuspendidos: 1.
	self assert: (customerBook includesCustomerNamed: paulMcCartney).
! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:33:42'!
test06RemovingASuspendedCustomerShouldRemoveItFromCustomerBook
	| customerBook paulMcCartney|

	paulMcCartney := 'Paul McCartney'.
	customerBook := self crearYAgregaA: paulMcCartney.

	customerBook suspendCustomerNamed: paulMcCartney.
	customerBook removeCustomerNamed: paulMcCartney.
	
	self assertSobre: customerBook queLosUsuariosTotalesSonUsuariosActivos: 0 yUsuariosSuspendidos: 0.
	self deny: (customerBook includesCustomerNamed: paulMcCartney).
! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:30:22'!
test07CanNotSuspendAnInvalidCustomer
	| customerBook johnLennon |

	johnLennon := 'John Lennon'.
	customerBook := self crearYAgregaA: johnLennon.
	
	self assertSobreUn: customerBook 
	realizarUnaAccion: [ :unCustomerBook | unCustomerBook suspendCustomerNamed: 'Ringo Starr']
	levantaUnError: CantSuspend
	yVerificaQue: [ :anError | 
				self assert: customerBook numberOfCustomers = 1.
				self assert: (customerBook includesCustomerNamed: johnLennon) ]
! !

!CustomerBookTest methodsFor: 'testing' stamp: 'sv 4/23/2023 17:33:23'!
test08CanNotSuspendAnAlreadySuspendedCustomer
	| customerBook johnLennon |

	johnLennon := 'John Lennon'.
	customerBook := self crearYAgregaA: johnLennon.

	customerBook suspendCustomerNamed: johnLennon.

	self assertSobreUn: customerBook 
	realizarUnaAccion: [ :unCustomerBook | unCustomerBook suspendCustomerNamed: johnLennon]
	levantaUnError: CantSuspend
	yVerificaQue: [ :anError | 
				self assert: customerBook numberOfCustomers = 1.
				self assert: (customerBook includesCustomerNamed: johnLennon) ]! !


!classDefinition: #CustomerBook category: 'CodigoRepetido-Ejercicio'!
Object subclass: #CustomerBook
	instanceVariableNames: 'suspended active'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'CodigoRepetido-Ejercicio'!

!CustomerBook methodsFor: 'testing' stamp: 'sv 4/23/2023 18:06:04'!
includesAsActiveCustomerNamed: aName

	^ self includesOn: active CustomerNamed: aName.! !

!CustomerBook methodsFor: 'testing' stamp: 'sv 4/23/2023 18:06:01'!
includesAsSuspendedCustomerNamed: aName

	^ self includesOn: suspended CustomerNamed: aName.! !

!CustomerBook methodsFor: 'testing' stamp: 'sv 4/23/2023 16:56:13'!
includesCustomerNamed: aName

	^ (self includesAsActiveCustomerNamed: aName) or: [ self includesAsSuspendedCustomerNamed: aName ]! !

!CustomerBook methodsFor: 'testing' stamp: 'sv 4/23/2023 16:54:17'!
includesOn: aCustomerGroup CustomerNamed: aName
	^ aCustomerGroup includes: aName! !

!CustomerBook methodsFor: 'testing' stamp: 'sv 4/23/2023 18:06:33'!
isEmpty

	^ active isEmpty and: [ suspended isEmpty ]! !

!CustomerBook methodsFor: 'testing' stamp: 'sv 4/23/2023 16:33:53'!
numberOf: customers

	^ customers size! !


!CustomerBook methodsFor: 'initialization' stamp: 'sv 4/23/2023 15:31:26'!
initialize
	super initialize.
	active := OrderedCollection new.
	suspended := OrderedCollection new.! !


!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 15:32:46'!
addCustomerNamed: aName

	aName isEmpty ifTrue: [ self signalCustomerNameCannotBeEmpty ].

	(self includesCustomerNamed: aName) ifTrue: [ self signalCustomerAlreadyExists ].

	active add: aName ! !

!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 15:36:34'!
numberOfActiveCustomers

	^ self numberOf: active! !

!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 15:34:40'!
numberOfCustomers

	^ self numberOfActiveCustomers + self numberOfSuspendedCustomers.! !

!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 15:36:48'!
numberOfSuspendedCustomers

	^ self numberOf: suspended! !

!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 16:59:44'!
removeCustomerNamed: aName 

	(self includesAsActiveCustomerNamed: aName) ifTrue: [ ^ active remove: aName ].
	(self includesAsSuspendedCustomerNamed: aName) ifTrue: [ ^ suspended remove: aName ].
	^ NotFound signal.
! !

!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 15:39:53'!
signalCustomerAlreadyExists 

	self signalCustomer: self class customerAlreadyExistsErrorMessage! !

!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 15:39:18'!
signalCustomerNameCannotBeEmpty 

	self signalCustomer: self class customerCanNotBeEmptyErrorMessage.! !

!CustomerBook methodsFor: 'customer management' stamp: 'sv 4/23/2023 18:07:58'!
suspendCustomerNamed: aName 
	
	(self includesAsActiveCustomerNamed: aName) ifFalse: [ ^ CantSuspend signal ].
	
	active remove: aName.
	suspended add: aName.
! !


!CustomerBook methodsFor: 'as yet unclassified' stamp: 'sv 4/23/2023 16:00:17'!
signalCustomer: aSignal

	self error: aSignal.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CustomerBook class' category: 'CodigoRepetido-Ejercicio'!
CustomerBook class
	instanceVariableNames: ''!

!CustomerBook class methodsFor: 'error messages' stamp: 'NR 4/30/2020 09:05:18'!
customerAlreadyExistsErrorMessage

	^'Customer Already Exists'! !

!CustomerBook class methodsFor: 'error messages' stamp: 'NR 4/30/2020 09:05:25'!
customerCanNotBeEmptyErrorMessage

	^'Customer Name Cannot Be Empty'! !
