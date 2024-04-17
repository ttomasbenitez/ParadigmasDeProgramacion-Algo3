!classDefinition: #StackTest category: 'Stack-Preludio'!
TestCase subclass: #StackTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Preludio'!

!StackTest methodsFor: 'tests' stamp: 'sv 5/7/2023 22:56:20'!
test01StackShouldBeEmptyWhenCreated
	| stack |
	
	stack := Stack new.
	
	self assert: stack isEmpty! !

!StackTest methodsFor: 'tests' stamp: 'sv 5/7/2023 22:56:23'!
test02StackShouldAddANewItemAndShouldntBeEmpty
	| stack |
	
	stack := Stack new.
	stack push: 1.
	
	self assert: stack isEmpty not! !

!StackTest methodsFor: 'tests' stamp: 'sv 5/7/2023 22:58:31'!
test03StackShouldPopANewItemAndShouldBeEmpty
	| stack elemento |
	
	stack := Stack new.
	elemento := 1.
	stack push: elemento.
	
	self assert: ((stack pop = elemento) and: (stack isEmpty))! !

!StackTest methodsFor: 'tests' stamp: 'sv 5/7/2023 22:56:32'!
test04StackShouldBeLIFO
	| stack primerElemento segundoElemento tercerElemento |
	
	stack := Stack new.
	primerElemento := 1.
	segundoElemento := 2.
	tercerElemento := 3.

	stack push: primerElemento.
	stack push: segundoElemento.
	stack push: tercerElemento.
	
	self assert: (stack pop = tercerElemento).
	self assert: (stack pop = segundoElemento).
	self assert: (stack pop = primerElemento).! !

!StackTest methodsFor: 'tests' stamp: 'sv 5/5/2023 00:51:06'!
test05CanSeeLastElementWithoutRemoveIt
	| stack primerElemento |
	
	stack := Stack new.
	primerElemento := 1.

	stack push: primerElemento.
	
	self assert: ((stack top = primerElemento) and: stack isEmpty not).! !

!StackTest methodsFor: 'tests' stamp: 'sv 5/7/2023 23:02:53'!
test06CanPopFromAnEmptyStack
	| stack |
	
	stack := Stack new..
	
	self
		should: [ stack pop ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = Stack StackIsEmptyError ]! !

!StackTest methodsFor: 'tests' stamp: 'sv 5/7/2023 23:03:19'!
test07CanTopFromAnEmptyStack
	| stack |
	
	stack := Stack new..
	
	self
		should: [ stack top ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = Stack StackIsEmptyError ]! !


!classDefinition: #Stack category: 'Stack-Preludio'!
Object subclass: #Stack
	instanceVariableNames: 'stack'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Stack-Preludio'!

!Stack methodsFor: 'initialization' stamp: 'sv 5/5/2023 00:27:08'!
initialize
	stack := OrderedCollection new.! !


!Stack methodsFor: 'operations' stamp: 'sv 5/7/2023 22:59:57'!
pop
	(stack isEmpty not) ifTrue: [ ^ stack removeLast ].
	^ self error: self class StackIsEmptyError.! !

!Stack methodsFor: 'operations' stamp: 'sv 5/5/2023 00:45:51'!
push: anItemToAdd
	stack add: anItemToAdd.! !

!Stack methodsFor: 'operations' stamp: 'sv 5/7/2023 23:04:00'!
top
	(stack isEmpty not) ifTrue: [ ^ stack last. ].
	^ self error: self class StackIsEmptyError.! !


!Stack methodsFor: 'test' stamp: 'sv 5/5/2023 00:45:58'!
isEmpty
	^ stack isEmpty.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Stack class' category: 'Stack-Preludio'!
Stack class
	instanceVariableNames: ''!

!Stack class methodsFor: 'as yet unclassified' stamp: 'sv 5/7/2023 23:00:20'!
StackIsEmptyError
	^ 'Stack is Empty'! !

!Stack class methodsFor: 'as yet unclassified' stamp: 'sv 5/5/2023 00:19:10'!
initialize! !

Stack initialize!
