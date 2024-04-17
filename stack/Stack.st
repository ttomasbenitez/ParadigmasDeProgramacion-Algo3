!classDefinition: #OOStackTest category: 'Stack-Exercise'!
TestCase subclass: #OOStackTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:39:34'!
test11StackShouldBeEmptyWhenCreated
	| stackToTest |

	stackToTest := OOStack withMax: 1.

	self StackShouldBeEmptyWhenCreated: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:40:14'!
test12PushAddElementsToTheStack
	| stackToTest |

	stackToTest := OOStack withMax: 1.

	self PushAddElementsToTheStack: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:41:15'!
test13PopRemovesElementsFromTheStack
	| stackToTest |

	stackToTest := OOStack withMax: 1.
	
	self PopRemovesElementsFromTheStack: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:42:35'!
test14PopReturnsLastPushedObject
	| stackToTest |

	stackToTest := OOStack withMax: 1.
	
	self PopReturnsLastPushedObject: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:43:22'!
test15StackBehavesLIFO
	| stackToTest |

	stackToTest := OOStack withMax: 2.
	
	self StackBehavesLIFO: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:42:43'!
test16TopReturnsLastPushedObject
	| stackToTest |

	stackToTest := OOStack withMax: 1.
	
	self TopReturnsLastPushedObject: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:42:46'!
test17TopDoesNotRemoveObjectFromStack
	| stackToTest |

	stackToTest := OOStack withMax: 1.
	
	self TopDoesNotRemoveObjectFromStack: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:42:49'!
test18CanNotPopWhenThereAreNoObjectsInTheStack
	| stackToTest |

	stackToTest := OOStack withMax: 1.

	self CanNotPopWhenThereAreNoObjectsInTheStack: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:42:53'!
test19CanNotPopWhenThereAreNoObjectsInTheStackAndTheStackHadObjects
	| stackToTest |

	stackToTest := OOStack withMax: 1.
	
	self CanNotPopWhenThereAreNoObjectsInTheStackAndTheStackHadObjects: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:42:56'!
test20CanNotTopWhenThereAreNoObjectsInTheStack
	| stackToTest |

	stackToTest := OOStack withMax: 1.
	
	self CanNotTopWhenThereAreNoObjectsInTheStack: stackToTest.! !

!OOStackTest methodsFor: 'LimitedStackTests' stamp: 'sv 5/7/2023 22:46:38'!
test21CanNotPushWhenItsFull
	| stackToTest |

	stackToTest := OOStack withMax: 1.
	stackToTest push: 'something'.
	
	self
		should: [ stackToTest push: 'somethingMore'. ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = OOStack stackFullErrorDescription ]
	
	! !


!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:46:50'!
CanNotPopWhenThereAreNoObjectsInTheStack: aStackToTest

	self
		should: [ aStackToTest pop ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = OOStack stackEmptyErrorDescription ]
		
! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:47:28'!
CanNotPopWhenThereAreNoObjectsInTheStackAndTheStackHadObjects: aStackToTest

	aStackToTest push: 'something'.
	aStackToTest pop.
	
	self
		should: [ aStackToTest pop ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = OOStack stackEmptyErrorDescription ]
		
! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:46:58'!
CanNotTopWhenThereAreNoObjectsInTheStack: aStackToTest

	self
		should: [ aStackToTest top ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = OOStack stackEmptyErrorDescription ]
		
! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:21:34'!
PopRemovesElementsFromTheStack: aStackToTest

	aStackToTest push: 'something'.
	aStackToTest pop.
	
	self assert: aStackToTest isEmpty! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:47:17'!
PopReturnsLastPushedObject: aStackToTest
	| pushedObject |

	pushedObject := 'something'.
	aStackToTest push: pushedObject.
	
	self assert: aStackToTest pop = pushedObject! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:21:23'!
PushAddElementsToTheStack: aStackToTest

	aStackToTest push: 'something'.
	
	self deny: aStackToTest isEmpty! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:47:12'!
StackBehavesLIFO: aStackToTest
	| firstPushedObject secondPushedObject |

	firstPushedObject := 'first'.
	secondPushedObject := 'second'.
	
	aStackToTest push: firstPushedObject.
	aStackToTest push: secondPushedObject.
	
	self assert: aStackToTest pop = secondPushedObject.
	self assert: aStackToTest pop = firstPushedObject.
	self assert: aStackToTest isEmpty 
	! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:21:41'!
StackShouldBeEmptyWhenCreated: aStackToTest
	
	self assert: aStackToTest isEmpty! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:47:08'!
TopDoesNotRemoveObjectFromStack: aStackToTest
	| pushedObject |

	pushedObject := 'something'.
	
	aStackToTest push: pushedObject.
	
	self assert: aStackToTest size = 1.
	aStackToTest top.
	self assert: aStackToTest size = 1.
	! !

!OOStackTest methodsFor: 'GenericTest' stamp: 'sv 5/7/2023 22:47:06'!
TopReturnsLastPushedObject: aStackToTest
	| pushedObject |

	pushedObject := 'something'.
	
	aStackToTest push: pushedObject.
	
	self assert: aStackToTest top = pushedObject.
	! !


!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:33:33'!
test01StackShouldBeEmptyWhenCreated
	| stackToTest |

	stackToTest := OOStack new.

	self StackShouldBeEmptyWhenCreated: stackToTest.! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:27:49'!
test02PushAddElementsToTheStack
	| stackToTest |

	stackToTest := OOStack new.

	self PushAddElementsToTheStack: stackToTest.! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:40'!
test03PopRemovesElementsFromTheStack
	| stackToTest |

	stackToTest := OOStack new.
	
	self PopRemovesElementsFromTheStack: stackToTest.! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:42'!
test04PopReturnsLastPushedObject
	| stackToTest |

	stackToTest := OOStack new.
	
	self PopReturnsLastPushedObject: stackToTest.

	! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:44'!
test05StackBehavesLIFO
	| stackToTest |

	stackToTest := OOStack new.
	
	self StackBehavesLIFO: stackToTest.
	! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:46'!
test06TopReturnsLastPushedObject
	| stackToTest |

	stackToTest := OOStack new.
	
	self TopReturnsLastPushedObject: stackToTest.
	! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:48'!
test07TopDoesNotRemoveObjectFromStack
	| stackToTest |

	stackToTest := OOStack new.
	
	self TopDoesNotRemoveObjectFromStack: stackToTest.
	! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:50'!
test08CanNotPopWhenThereAreNoObjectsInTheStack
	| stackToTest |

	stackToTest := OOStack new.

	self CanNotPopWhenThereAreNoObjectsInTheStack: stackToTest.
		
! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:52'!
test09CanNotPopWhenThereAreNoObjectsInTheStackAndTheStackHadObjects
	| stackToTest |

	stackToTest := OOStack new.
	
	self CanNotPopWhenThereAreNoObjectsInTheStackAndTheStackHadObjects: stackToTest.
		
! !

!OOStackTest methodsFor: 'UnlimitedStackTests' stamp: 'sv 5/7/2023 22:39:54'!
test10CanNotTopWhenThereAreNoObjectsInTheStack
	| stackToTest |

	stackToTest := OOStack new.
	
	self CanNotTopWhenThereAreNoObjectsInTheStack: stackToTest.
		
! !


!classDefinition: #SentenceFinderByPrefixTest category: 'Stack-Exercise'!
TestCase subclass: #SentenceFinderByPrefixTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!SentenceFinderByPrefixTest methodsFor: 'setUp' stamp: 'TB 5/7/2023 18:29:55'!
crearStackCon: oracionesAAniadir
	| stack |
	stack := OOStack new.
	
	oracionesAAniadir do: [
		:Oracion | stack push: Oracion.
		].
	
	^stack.! !


!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 17:48:46'!
test01ElPrefijoNoPuedeSerVacio
	| stack finder |

	stack := OOStack new.
	finder := SentenceFinderByPrefix withStack: stack.
	
	self should: [ finder findWithAPrefix: '' ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText = EmptyPrefix InvalidPrefixErrorDescription ].
	
	! !

!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 18:05:28'!
test02ElPrefijoNoPuedeContenerEspaciosVacios
	| stack finder |

	stack := OOStack new.
	finder := SentenceFinderByPrefix withStack: stack.
	
	self should: [ finder findWithAPrefix: 'W a' ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: anError messageText = EmptySpacesPrefix InvalidPrefixErrorDescription ].
	
	
	! !

!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 18:31:14'!
test03ElStackMantieneElOrden
	| stack finder cuarto primero segundo tercero |
	primero := 'Winter is here'.
	segundo := 'winter is coming'.
	tercero := 'winning is everything'.
	cuarto := 'the winds of winter'.
	
	stack := self crearStackCon: {primero . segundo . tercero . cuarto }.

	finder := SentenceFinderByPrefix withStack: stack.
	finder findWithAPrefix: 'Wint'.
	
	self assert: (stack pop) equals: cuarto.
	self assert: (stack pop) equals: tercero.
	self assert: (stack pop) equals: segundo.
	self assert: (stack pop) equals: primero.	
	
	! !

!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 18:32:27'!
test04EncuentraCorrectamenteElUnicoStringCumplidor
	| stack finder cumplidor primero segundo tercero |
	
	primero := 'the winds of winter'.
	segundo := 'winter is coming'.
	tercero := 'winning is everything'.
	cumplidor := 'Winter is here'.
	
	stack := self crearStackCon: {primero . segundo . cumplidor . tercero }.
	finder := SentenceFinderByPrefix withStack: stack.
	
	self assert: (finder findWithAPrefix: 'Wint') first equals: cumplidor.! !

!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 18:33:34'!
test05EncuentraCorrectamenteMultiplesStringsCumplidores
	|stack finder cumplidor1 cumplidor2 cumplidor3 noCumplidor1 noCumplidor2 noCumplidor3 cumplidores cumplidoresDetectados |
	
	cumplidor1 := 'Boca'.
	cumplidor2 := 'Boca Unidos'.
	cumplidor3 := 'Boca Sucia'.
	noCumplidor1 := 'river'.
	noCumplidor2 := 'boca'.
	noCumplidor3 := 'Banfield'.
	
	stack := self crearStackCon: {cumplidor1 . noCumplidor1 . noCumplidor3 . cumplidor3 . noCumplidor2 . cumplidor2 }.
	finder := SentenceFinderByPrefix withStack: stack.
	cumplidores := {cumplidor2. cumplidor3 . cumplidor1}.
	cumplidoresDetectados := finder findWithAPrefix: 'Boca'.
	
	self assert: (cumplidoresDetectados size) equals: cumplidores size.
	self assert: cumplidoresDetectados includes:  cumplidor1.
	self assert: cumplidoresDetectados includes:  cumplidor2.
	self assert: cumplidoresDetectados includes:  cumplidor3.
	! !

!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 18:34:15'!
test06ElSentenceFinderByPrefixEsCaseSensitive
	| stack finder cumplidor primero segundo |
	
	primero := 'Wint'.
	segundo := 'wiNt'.
	cumplidor := 'wint'.
	
	stack := self crearStackCon: {primero . segundo . cumplidor }.
	finder := SentenceFinderByPrefix withStack: stack.
	
	self assert: (finder findWithAPrefix: 'wint') first equals: cumplidor.! !

!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 17:02:50'!
test07SentenceFinderByPrefixNoDevuelveNadaSiElStackEstaVacio
	| stack finder |
	
	stack := OOStack new.
	finder := SentenceFinderByPrefix withStack: stack.
	
	self assert: (finder findWithAPrefix: 'wint') isEmpty.! !

!SentenceFinderByPrefixTest methodsFor: 'tests' stamp: 'TB 5/7/2023 18:35:11'!
test08SentenceFinderByPrefixNoDevuelveNadaSiNoEncuentraCoincidencias
	| stack finder primero segundo tercero cuarto |
	
	primero := 'the winds of winter'.
	segundo := 'winter is coming'.
	tercero := 'winning is everything'.
	cuarto := 'winter is here'.
	
	stack := self crearStackCon: {primero . segundo . tercero . cuarto}.
	finder := SentenceFinderByPrefix withStack: stack.
	
	self assert: (finder findWithAPrefix: 'AAA') isEmpty.! !


!classDefinition: #OOStack category: 'Stack-Exercise'!
Object subclass: #OOStack
	instanceVariableNames: 'topObject size'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!OOStack methodsFor: 'initialization' stamp: 'sv 5/7/2023 21:41:13'!
initialize
	size := 0.
	topObject := NullStackObject new.! !


!OOStack methodsFor: 'testing' stamp: 'sv 5/7/2023 21:27:45'!
isEmpty
	^ size = 0. ! !

!OOStack methodsFor: 'testing' stamp: 'sv 5/7/2023 21:28:14'!
size
	^ size.! !


!OOStack methodsFor: 'operations' stamp: 'sv 5/7/2023 21:27:54'!
pop
	| poppedObject |
	
	poppedObject := topObject.
	topObject := poppedObject neighbour.
	size := size - 1.
	
	^ poppedObject value.! !

!OOStack methodsFor: 'operations' stamp: 'sv 5/7/2023 21:40:28'!
push: anElement
	| previousTopObject |

	size := size + 1.
	previousTopObject := topObject.
	topObject := RealStackObject with: anElement nextTo: previousTopObject.
	! !

!OOStack methodsFor: 'operations' stamp: 'sv 5/7/2023 21:31:06'!
top
	^ topObject value.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'OOStack class' category: 'Stack-Exercise'!
OOStack class
	instanceVariableNames: ''!

!OOStack class methodsFor: 'error descriptions' stamp: 'HernanWilkinson 5/7/2012 11:51'!
stackEmptyErrorDescription
	
	^ 'Stack is empty'! !

!OOStack class methodsFor: 'error descriptions' stamp: 'sv 5/7/2023 21:25:46'!
stackFullErrorDescription
	
	^ 'Stack is full'! !


!OOStack class methodsFor: 'as yet unclassified' stamp: 'sv 5/7/2023 21:47:25'!
withMax: aMaxSize
	^ OOLimitedStack new initializeWithMax: aMaxSize.	! !


!classDefinition: #OOLimitedStack category: 'Stack-Exercise'!
OOStack subclass: #OOLimitedStack
	instanceVariableNames: 'maxSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!OOLimitedStack methodsFor: 'initialization' stamp: 'sv 5/7/2023 21:39:36'!
initializeWithMax: aMaxSize
	maxSize := aMaxSize.
	size := 0.
	topObject := NullStackObject new.! !


!OOLimitedStack methodsFor: 'operations' stamp: 'sv 5/7/2023 22:12:06'!
push: anElement
	
	(size = maxSize) ifTrue: [ ^ self error: self class stackFullErrorDescription ].

	^ super push: anElement.! !


!classDefinition: #Prefix category: 'Stack-Exercise'!
Object subclass: #Prefix
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Prefix class' category: 'Stack-Exercise'!
Prefix class
	instanceVariableNames: ''!

!Prefix class methodsFor: 'error' stamp: 'TB 5/7/2023 18:03:49'!
isInvalid: aValueToValidate
	self subclassResponsibility.! !


!Prefix class methodsFor: 'initialization' stamp: 'TB 5/7/2023 18:04:54'!
withValue: aValue
	
	self subclasses detect: [ :aPrefixSubclass | aPrefixSubclass isInvalid: aValue ] 
	ifFound: [ :aPrefixSubclassDetected | ^self error: aPrefixSubclassDetected InvalidPrefixErrorDescription ]
	ifNone: [^ValidPrefix new initializeWith: aValue.].
	
	! !


!classDefinition: #EmptyPrefix category: 'Stack-Exercise'!
Prefix subclass: #EmptyPrefix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'EmptyPrefix class' category: 'Stack-Exercise'!
EmptyPrefix class
	instanceVariableNames: ''!

!EmptyPrefix class methodsFor: 'initialization' stamp: 'sv 5/7/2023 21:31:26'!
isInvalid: aValueToValidate
	^ aValueToValidate = ''.! !


!EmptyPrefix class methodsFor: 'errors' stamp: 'sv 5/7/2023 21:31:23'!
InvalidPrefixErrorDescription
	^ 'Prefix can not be empty'.! !


!classDefinition: #EmptySpacesPrefix category: 'Stack-Exercise'!
Prefix subclass: #EmptySpacesPrefix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'EmptySpacesPrefix class' category: 'Stack-Exercise'!
EmptySpacesPrefix class
	instanceVariableNames: ''!

!EmptySpacesPrefix class methodsFor: 'errors' stamp: 'TB 5/7/2023 17:55:08'!
InvalidPrefixErrorDescription
	^'Prefix can not contain empty spaces'.! !


!EmptySpacesPrefix class methodsFor: 'initialization' stamp: 'TB 5/7/2023 18:07:21'!
isInvalid: aValueToValidate
	^aValueToValidate includesSubString: ' '.! !


!classDefinition: #ValidPrefix category: 'Stack-Exercise'!
Prefix subclass: #ValidPrefix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!ValidPrefix methodsFor: 'initialization' stamp: 'TB 5/7/2023 18:08:02'!
initializeWith: aValue
	value := aValue.! !


!ValidPrefix methodsFor: 'operations' stamp: 'TB 5/7/2023 18:12:25'!
matchesPrefix: aSentence
	^aSentence is: value substringAt: 1.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ValidPrefix class' category: 'Stack-Exercise'!
ValidPrefix class
	instanceVariableNames: ''!

!ValidPrefix class methodsFor: 'initialization' stamp: 'TB 5/7/2023 18:03:29'!
isInvalid: aValueToValidate
	^false.! !


!classDefinition: #SentenceFinderByPrefix category: 'Stack-Exercise'!
Object subclass: #SentenceFinderByPrefix
	instanceVariableNames: 'stack'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!SentenceFinderByPrefix methodsFor: 'as yet unclassified' stamp: 'sv 5/7/2023 22:51:44'!
findWithAPrefix: aPrefix 
	| aux_stack aux_sentence matchs prefix|
	
	prefix := Prefix withValue: aPrefix.
	aux_stack := OOStack new.
	matchs := OrderedCollection new.
	
	self forStackObjectInStack: stack Do: [
		aux_sentence := stack pop.
		(prefix matchesPrefix: aux_sentence) ifTrue: [ matchs add: aux_sentence ].
		aux_stack push: aux_sentence.
	].
	
	self forStackObjectInStack: aux_stack Do: [
		stack push: aux_stack pop.
	].

	^ matchs.! !

!SentenceFinderByPrefix methodsFor: 'as yet unclassified' stamp: 'TB 5/7/2023 18:16:13'!
forStackObjectInStack: aStack Do: aClosure	
	
	[(aStack isEmpty) not] whileTrue: aClosure.! !

!SentenceFinderByPrefix methodsFor: 'as yet unclassified' stamp: 'TB 5/5/2023 18:58:19'!
initializeWithAStack: aStack
	stack := aStack.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SentenceFinderByPrefix class' category: 'Stack-Exercise'!
SentenceFinderByPrefix class
	instanceVariableNames: ''!

!SentenceFinderByPrefix class methodsFor: 'initialization' stamp: 'TB 5/5/2023 18:57:27'!
withStack: aStack
	^SentenceFinderByPrefix new initializeWithAStack: aStack.! !


!classDefinition: #StackObject category: 'Stack-Exercise'!
Object subclass: #StackObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!StackObject methodsFor: 'operations' stamp: 'TB 5/5/2023 11:16:37'!
neighbour
	self subclassResponsibility.! !

!StackObject methodsFor: 'operations' stamp: 'TB 5/5/2023 11:17:46'!
value
	self subclassResponsibility.! !


!classDefinition: #NullStackObject category: 'Stack-Exercise'!
StackObject subclass: #NullStackObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!NullStackObject methodsFor: 'operations' stamp: 'sv 5/7/2023 22:52:02'!
neighbour
	^ self error: OOStack stackEmptyErrorDescription.! !

!NullStackObject methodsFor: 'operations' stamp: 'sv 5/7/2023 22:52:04'!
value
	^ self error: OOStack stackEmptyErrorDescription.! !


!classDefinition: #RealStackObject category: 'Stack-Exercise'!
StackObject subclass: #RealStackObject
	instanceVariableNames: 'value neighbour'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Exercise'!

!RealStackObject methodsFor: 'operations' stamp: 'sv 5/7/2023 22:52:11'!
neighbour
	^ neighbour.! !

!RealStackObject methodsFor: 'operations' stamp: 'sv 5/7/2023 22:52:09'!
value
	^ value.! !


!RealStackObject methodsFor: 'initialization' stamp: 'TB 5/5/2023 11:21:25'!
initializeWith: something nextTo: aNeighbour
	value := something.
	neighbour := aNeighbour.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'RealStackObject class' category: 'Stack-Exercise'!
RealStackObject class
	instanceVariableNames: ''!

!RealStackObject class methodsFor: 'initialization' stamp: 'TB 5/5/2023 11:21:46'!
with: something nextTo: aNeighbour
	^RealStackObject new initializeWith: something nextTo: aNeighbour.! !
