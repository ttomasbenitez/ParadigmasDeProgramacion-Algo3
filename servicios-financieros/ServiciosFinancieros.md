!classDefinition: #PortfolioTest category: 'ServiciosFinancieros-Ejercicio'!
TestCase subclass: #PortfolioTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!PortfolioTest methodsFor: 'tests' stamp: 'n 6/4/2023 17:32:14'!
test01AnEmptyPortfolioBalanceIsZero
	| portfolio |
	
	portfolio := Portfolio new.
	
	self assert: (portfolio balance) equals: 0.! !

!PortfolioTest methodsFor: 'tests' stamp: 'n 6/4/2023 17:39:59'!
test02APortfolioWithOneAccountBalanceIsItsAccountBalance
	| portfolio account |
	
	account := ReceptiveAccount new.
	Deposit register: 100*peso on: account.
	portfolio := Portfolio new.
	portfolio add: account.
	
	self assert: (portfolio balance) equals: 100*peso.! !

!PortfolioTest methodsFor: 'tests' stamp: 'n 6/4/2023 17:39:26'!
test03APortfolioWithMoreThanOneAccountBalanceIsItsAccountsBalancesSum
	| portfolio account1 account2 |
	
	account1 := ReceptiveAccount new.
	account2 := ReceptiveAccount new.
	
	Deposit register: 100*peso on: account1.
	Deposit register: 50*peso on: account2.
	portfolio := Portfolio new.
	
	portfolio add: account1.
	portfolio add: account2.
	
	self assert: (portfolio balance) equals: 150*peso.! !

!PortfolioTest methodsFor: 'tests' stamp: 'n 6/4/2023 17:51:19'!
test04APortfolioWithOneEmptyPortfolioBalanceIsZero
	| mainPortfolio subPortfolio |
	
	mainPortfolio := Portfolio new.
	subPortfolio := Portfolio new.

	mainPortfolio add: subPortfolio.
	
	self assert: (mainPortfolio balance) equals: 0*peso.! !

!PortfolioTest methodsFor: 'tests' stamp: 'n 6/4/2023 18:00:28'!
test05KnowsItsAccountsTransactions
	| mainPortfolio subPortfolio cuenta transaction |
	
	cuenta := ReceptiveAccount new.
	mainPortfolio := Portfolio new.
	subPortfolio := Portfolio new.
	
	transaction := Deposit register: 100*peso on: cuenta. 
	
	subPortfolio add: cuenta.
	mainPortfolio add: subPortfolio.
	
	self assert: (mainPortfolio hasRegistered: transaction).! !

!PortfolioTest methodsFor: 'tests' stamp: 'S.V. 6/4/2023 20:52:46'!
test06APortfolioKnowsAllItsAccountsTransactions
	| mainPortfolio subPortfolio cuenta1 cuenta2 |
	
	cuenta1 := ReceptiveAccount new.
	cuenta2 := ReceptiveAccount new.
	mainPortfolio := Portfolio new.
	subPortfolio := Portfolio new.
	
	Deposit register: 100*peso on: cuenta1. 
	Deposit register: 200*peso on: cuenta2. 
	
	subPortfolio add: cuenta1.
	mainPortfolio add: cuenta2.
	mainPortfolio add: subPortfolio.
	
	self assert: (mainPortfolio transactions: cuenta1) equals: cuenta1 transactions.
	self assert: (mainPortfolio transactions: cuenta2) equals: cuenta2 transactions.! !


!classDefinition: #ReceptiveAccountTest category: 'ServiciosFinancieros-Ejercicio'!
TestCase subclass: #ReceptiveAccountTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!ReceptiveAccountTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:10:43'!
test01ReceptiveAccountHaveZeroAsBalanceWhenCreated 

	| account |
	
	account := ReceptiveAccount new.

	self assert: 0 equals: account balance
! !

!ReceptiveAccountTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:14:20'!
test02DepositIncreasesBalanceOnTransactionValue 

	| account |
	
	account := ReceptiveAccount  new.
	Deposit register: 100 * peso on: account.
		
	self assert: 100 * peso equals: account balance
! !

!ReceptiveAccountTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:14:24'!
test03WithdrawDecreasesBalanceOnTransactionValue 

	| account |
	
	account := ReceptiveAccount new.
	Deposit register: 100 * peso on: account.
	Withdraw register: 50 * peso on: account.
		
	self assert: 50 * peso equals: account balance
! !

!ReceptiveAccountTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:10:14'!
test04WithdrawValueMustBePositive 

	| account withdrawValue |
	
	account := ReceptiveAccount new.
	withdrawValue := 50 * peso.
	
	self assert: withdrawValue equals: (Withdraw register: withdrawValue on: account) value
! !

!ReceptiveAccountTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:10:22'!
test05ReceptiveAccountKnowsRegisteredTransactions 

	| account deposit withdraw |
	
	account := ReceptiveAccount new.
	deposit := Deposit register: 100 * peso on: account.
	withdraw := Withdraw register: 50 * peso on: account.
		
	self assert: (account hasRegistered: deposit).
	self assert: (account hasRegistered: withdraw).
! !

!ReceptiveAccountTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:12:14'!
test06ReceptiveAccountDoNotKnowNotRegisteredTransactions

	| account deposit withdraw |
	
	account := ReceptiveAccount new.
	deposit :=  Deposit for: 100 * peso.
	withdraw := Withdraw for: 50 * peso.
		
	self deny: (account hasRegistered: deposit).
	self deny: (account hasRegistered: withdraw).
! !

!ReceptiveAccountTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:12:23'!
test07AccountKnowsItsTransactions 

	| account deposit |
	
	account := ReceptiveAccount new.
	
	deposit := Deposit register: 50 * peso on: account.
		
	self assert: 1 equals: account transactions size.
	self assert: (account transactions includes: deposit).
! !


!classDefinition: #TransferenciasTest category: 'ServiciosFinancieros-Ejercicio'!
TestCase subclass: #TransferenciasTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!TransferenciasTest methodsFor: 'tests' stamp: 'n 6/4/2023 16:41:53'!
test01ATransactionTransfersMoneyFromAnOriginAccountToATargetAccountCorrectly
	
	| originAccount targetAccount |
	originAccount := ReceptiveAccount new.
	targetAccount := ReceptiveAccount new.
	
	Deposit register: 100*peso on: originAccount.
	originAccount transferTo: targetAccount forAmount: 100*peso.
	
	self assert: (originAccount balance) equals: 0*peso. 
	self assert: (targetAccount balance) equals: 100*peso. ! !

!TransferenciasTest methodsFor: 'tests' stamp: 'n 6/4/2023 16:42:02'!
test02ATransactionWithdrawsMoneyFromAnOriginAccount
	
	| originAccount targetAccount |
	originAccount := ReceptiveAccount new.
	targetAccount := ReceptiveAccount new.
	
	Deposit register: 100*peso on: originAccount.
	originAccount transferTo: targetAccount forAmount: 50*peso.
	
	self assert: (originAccount balance) equals: 50*peso.
	self assert: (targetAccount balance) equals: 50*peso. ! !

!TransferenciasTest methodsFor: 'tests' stamp: 'n 6/4/2023 16:39:20'!
test03AttemptingToTransferWithoutEnoughMoneyRaisesAnError
	
	| originAccount targetAccount |
	originAccount := ReceptiveAccount new.
	targetAccount := ReceptiveAccount new.
	
	self
		should: [ originAccount transferTo: targetAccount forAmount: 50*peso. ]
		raise: Error
		withExceptionDo: [ :anError |
			self assert: anError messageText = 'Insufficient balance error' ].! !

!TransferenciasTest methodsFor: 'tests' stamp: 'n 6/4/2023 16:54:02'!
test04ATransferenceKnowsItsDepositAndWithdrawValues
	
	| originAccount targetAccount transference |
	
	originAccount := ReceptiveAccount new.
	targetAccount := ReceptiveAccount new.
	
	Deposit register: 100*peso on: originAccount.
	transference := originAccount transferTo: targetAccount forAmount: 50*peso.
	
	self assert: (transference depositValue) equals: 50*peso.
	self assert: (transference withdrawValue) equals: 50*peso. ! !

!TransferenciasTest methodsFor: 'tests' stamp: 'n 6/4/2023 17:26:07'!
test05ATransferenceLegKnowsItsOppositeLeg
	
	| originAccount targetAccount transference |
	
	originAccount := ReceptiveAccount new.
	targetAccount := ReceptiveAccount new.
	
	Deposit register: 100*peso on: originAccount.
	transference := originAccount transferTo: targetAccount forAmount: 50*peso.
	
	self assert: ((transference depositLeg) withdrawLeg) equals: transference withdrawLeg.
	self assert: ((transference withdrawLeg) depositLeg) equals: transference depositLeg.! !


!classDefinition: #AccountTransaction category: 'ServiciosFinancieros-Ejercicio'!
Object subclass: #AccountTransaction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!AccountTransaction methodsFor: 'value' stamp: 'S.V. 6/4/2023 19:46:54'!
addMyBalanceTo: aBalance

	self subclassResponsibility ! !

!AccountTransaction methodsFor: 'value' stamp: 'HernanWilkinson 9/12/2011 12:25'!
value 

	self subclassResponsibility ! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'AccountTransaction class' category: 'ServiciosFinancieros-Ejercicio'!
AccountTransaction class
	instanceVariableNames: ''!

!AccountTransaction class methodsFor: 'instance creation' stamp: 'NR 10/17/2019 03:22:00'!
register: aValue on: account

	| transaction |
	
	transaction := self for: aValue.
	account register: transaction.
		
	^ transaction! !


!classDefinition: #Deposit category: 'ServiciosFinancieros-Ejercicio'!
AccountTransaction subclass: #Deposit
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!Deposit methodsFor: 'initialization' stamp: 'HernanWilkinson 7/13/2011 18:45'!
initializeFor: aValue

	value := aValue ! !


!Deposit methodsFor: 'value' stamp: 'HernanWilkinson 7/13/2011 18:38'!
value

	^ value! !


!Deposit methodsFor: 'operations' stamp: 'S.V. 6/4/2023 19:46:38'!
addMyBalanceTo: aBalance

	^ aBalance + value.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Deposit class' category: 'ServiciosFinancieros-Ejercicio'!
Deposit class
	instanceVariableNames: ''!

!Deposit class methodsFor: 'instance creation' stamp: 'HernanWilkinson 7/13/2011 18:38'!
for: aValue

	^ self new initializeFor: aValue ! !


!classDefinition: #Withdraw category: 'ServiciosFinancieros-Ejercicio'!
AccountTransaction subclass: #Withdraw
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!Withdraw methodsFor: 'initialization' stamp: 'HernanWilkinson 7/13/2011 18:46'!
initializeFor: aValue

	value := aValue ! !


!Withdraw methodsFor: 'value' stamp: 'HernanWilkinson 7/13/2011 18:33'!
value

	^ value! !


!Withdraw methodsFor: 'operations' stamp: 'S.V. 6/4/2023 19:46:43'!
addMyBalanceTo: aBalance

	^ aBalance - value.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Withdraw class' category: 'ServiciosFinancieros-Ejercicio'!
Withdraw class
	instanceVariableNames: ''!

!Withdraw class methodsFor: 'instance creation' stamp: 'HernanWilkinson 7/13/2011 18:33'!
for: aValue

	^ self new initializeFor: aValue ! !


!classDefinition: #DepositLeg category: 'ServiciosFinancieros-Ejercicio'!
Object subclass: #DepositLeg
	instanceVariableNames: 'withdrawLeg deposit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!DepositLeg methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:13:56'!
deposit

	^ deposit .! !

!DepositLeg methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:13:52'!
withdrawLeg

	^ withdrawLeg.! !


!DepositLeg methodsFor: 'initialization' stamp: 'S.V. 6/4/2023 21:13:58'!
withDeposit: aDeposit

	deposit := aDeposit.! !

!DepositLeg methodsFor: 'initialization' stamp: 'S.V. 6/4/2023 21:14:00'!
withWithdrawLeg: aWithdrawLeg

	withdrawLeg := aWithdrawLeg.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DepositLeg class' category: 'ServiciosFinancieros-Ejercicio'!
DepositLeg class
	instanceVariableNames: ''!

!DepositLeg class methodsFor: 'as yet unclassified' stamp: 'n 6/4/2023 17:18:34'!
withDeposit: deposit
	^self new withDeposit: deposit.! !


!classDefinition: #Portfolio category: 'ServiciosFinancieros-Ejercicio'!
Object subclass: #Portfolio
	instanceVariableNames: 'entities'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!Portfolio methodsFor: 'operations' stamp: 'n 6/4/2023 17:53:28'!
add: anAccount

	entities add: anAccount.! !

!Portfolio methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:01:57'!
addAccountsTo: aCollectionOfAccounts

	entities do: [ :anEntity | anEntity addAccountsTo: aCollectionOfAccounts ].
	! !

!Portfolio methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:01:38'!
addMyBalanceTo: aBalance

	^ aBalance + self balance.! !

!Portfolio methodsFor: 'operations' stamp: 'S.V. 6/4/2023 19:47:48'!
balance
	
	^ entities inject: 0*peso into: [ :totalBalance :anAccount | anAccount addMyBalanceTo: totalBalance ].! !

!Portfolio methodsFor: 'operations' stamp: 'n 6/4/2023 18:06:27'!
hasRegistered: aTransaction

	^ entities anySatisfy: [ :anEntity | anEntity hasRegistered: aTransaction]. 
	! !

!Portfolio methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:00:53'!
transactions: anAccount
	| allAccounts |
	
	allAccounts := OrderedCollection new.
	self addAccountsTo: allAccounts.
	
	^ (allAccounts detect: [ :theAccount | theAccount = anAccount ] ifNone: [self error: 'Account not found']) transactions.
	! !


!Portfolio methodsFor: 'initialization' stamp: 'S.V. 6/4/2023 21:02:02'!
initialize

	entities := Set new.! !


!classDefinition: #ReceptiveAccount category: 'ServiciosFinancieros-Ejercicio'!
Object subclass: #ReceptiveAccount
	instanceVariableNames: 'transactions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'S.V. 6/4/2023 21:02:08'!
addAccountsTo: aCollectionOfAccounts

	aCollectionOfAccounts add: self.! !

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'S.V. 6/4/2023 19:49:42'!
addMyBalanceTo: aBalance

	^ aBalance + self balance.! !

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'S.V. 6/4/2023 19:49:12'!
balance

	^ transactions inject: 0*peso into: [  :totalBalance :aTransaction | aTransaction addMyBalanceTo: totalBalance ].! !

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'NR 10/17/2019 03:28:43'!
hasRegistered: aTransaction

	^ transactions includes: aTransaction 
! !

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'NR 10/17/2019 15:06:56'!
initialize

	transactions := OrderedCollection new.! !

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'HernanWilkinson 7/13/2011 18:37'!
register: aTransaction

	transactions add: aTransaction 
! !

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'S.V. 6/4/2023 20:28:39'!
transactions

	^ transactions copy! !

!ReceptiveAccount methodsFor: 'as yet unclassified' stamp: 'S.V. 6/4/2023 19:50:03'!
transferTo: targetAccount forAmount: anAmount

	(self balance < anAmount) ifTrue: [self error: self class insufficientBalanceError].
	
	^ Transference from: self to: targetAccount forAmount: anAmount.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ReceptiveAccount class' category: 'ServiciosFinancieros-Ejercicio'!
ReceptiveAccount class
	instanceVariableNames: ''!

!ReceptiveAccount class methodsFor: 'as yet unclassified' stamp: 'n 6/4/2023 16:30:08'!
insufficientBalanceError
	^'Insufficient balance error'.! !


!classDefinition: #Transference category: 'ServiciosFinancieros-Ejercicio'!
Object subclass: #Transference
	instanceVariableNames: 'depositLeg withdrawLeg'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!Transference methodsFor: 'initialization' stamp: 'n 6/4/2023 17:15:11'!
from: originAccount to: targetAccount forAmount: anAmountToTransfer
	|deposit withdraw|
	deposit := Deposit register: anAmountToTransfer on: targetAccount.
	withdraw := Withdraw register: anAmountToTransfer on: originAccount.
	
	depositLeg := DepositLeg withDeposit: deposit. 
	withdrawLeg := WithdrawLeg withWithdraw: withdraw.
	
	depositLeg withWithdrawLeg: withdrawLeg.
	withdrawLeg withDepositLeg: depositLeg.! !


!Transference methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:14'!
deposit

	^depositLeg deposit.! !

!Transference methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:17'!
depositLeg

	^depositLeg.! !

!Transference methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:22'!
depositValue

	^depositLeg deposit value.! !

!Transference methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:25'!
withdraw

	^withdrawLeg withdraw.! !

!Transference methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:27'!
withdrawLeg

	^withdrawLeg.! !

!Transference methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:30'!
withdrawValue

	^withdrawLeg withdraw value.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Transference class' category: 'ServiciosFinancieros-Ejercicio'!
Transference class
	instanceVariableNames: ''!

!Transference class methodsFor: 'as yet unclassified' stamp: 'n 6/4/2023 16:51:13'!
from: originAccount to: targetAccount forAmount: anAmountToTransfer
	^self new from: originAccount to: targetAccount forAmount: anAmountToTransfer.! !


!classDefinition: #WithdrawLeg category: 'ServiciosFinancieros-Ejercicio'!
Object subclass: #WithdrawLeg
	instanceVariableNames: 'withdraw depositLeg'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Ejercicio'!

!WithdrawLeg methodsFor: 'initialization' stamp: 'S.V. 6/4/2023 21:14:54'!
withDepositLeg: aDepositLeg

	depositLeg := aDepositLeg.! !

!WithdrawLeg methodsFor: 'initialization' stamp: 'S.V. 6/4/2023 21:14:57'!
withWithdraw: aWithdraw

	withdraw := aWithdraw.! !


!WithdrawLeg methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:47'!
depositLeg

	^ depositLeg.! !

!WithdrawLeg methodsFor: 'operations' stamp: 'S.V. 6/4/2023 21:14:51'!
withdraw

	^ withdraw.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'WithdrawLeg class' category: 'ServiciosFinancieros-Ejercicio'!
WithdrawLeg class
	instanceVariableNames: ''!

!WithdrawLeg class methodsFor: 'as yet unclassified' stamp: 'n 6/4/2023 17:20:59'!
withWithdraw: withdraw
	^self new withWithdraw: withdraw.! !
