!classDefinition: #PortfolioTest category: 'ServiciosFinancieros-Solucion'!
TestCase subclass: #PortfolioTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 11:16:26'!
test01BalanceOfPortfolioWithoutAccountsIsZero

	self assert: 0 equals: Portfolio new balance! !

!PortfolioTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:27:00'!
test02BalanceOfPortfolioWithOneAccountIsAccountBalance

	| account portfolio |
	
	account := ReceptiveAccount new.
	Deposit register: 100 * peso on: account.
	portfolio := Portfolio with: account.
	
	self assert: account balance equals: portfolio balance! !

!PortfolioTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:27:06'!
test03BalanceOfPortfolioIsCalculatedRecursivelyOnPortfolios

	| simplePortfolioAccount simplePortfolio composedPortfolioAccount composedPortofolio |
	
	simplePortfolioAccount := ReceptiveAccount new.
	Deposit register: 100 * peso on: simplePortfolioAccount.
	simplePortfolio := Portfolio with: simplePortfolioAccount.
	
	composedPortfolioAccount := ReceptiveAccount new.
	Withdraw register: 50 * peso on: composedPortfolioAccount.
	composedPortofolio := Portfolio with: simplePortfolio with: composedPortfolioAccount.
	
	self assert: (composedPortfolioAccount balance + simplePortfolio balance) equals: composedPortofolio balance! !

!PortfolioTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:27:12'!
test04PortfolioWithoutAccountsHasNoRegisteredTransaction

	self deny: (Portfolio new hasRegistered: (Deposit for: 100 * peso))! !

!PortfolioTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:27:18'!
test05PortfolioHasRegisteredItsAccountsTransactions

	| account portfolio deposit |
	
	account := ReceptiveAccount new.
	deposit := Deposit register: 100 * peso on: account.
	portfolio := Portfolio with: account.
	
	self assert: (portfolio hasRegistered: deposit)! !

!PortfolioTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:27:26'!
test06PortfolioLooksForRegisteredTransactionsRecursively

	| simplePortfolioAccount simplePortfolio composedPortfolioAccount composedPortfolio composedPortfolioAccountWithdraw simplePortfolioAccountDeposit |
	
	simplePortfolioAccount := ReceptiveAccount new.
	simplePortfolioAccountDeposit := Deposit register: 100 * peso on: simplePortfolioAccount.
	simplePortfolio := Portfolio with: simplePortfolioAccount.
	
	composedPortfolioAccount := ReceptiveAccount new.
	composedPortfolioAccountWithdraw := Withdraw register: 50 * peso on: composedPortfolioAccount.
	composedPortfolio := Portfolio with: simplePortfolio with: composedPortfolioAccount.
	
	self assert: (composedPortfolio hasRegistered: simplePortfolioAccountDeposit).
	self assert: (composedPortfolio hasRegistered: composedPortfolioAccountWithdraw)! !

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 11:58:10'!
test07PortfolioHasNoTransactionWhenHasNoAccounts

	self assert: Portfolio new transactions isEmpty! !

!PortfolioTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:28:08'!
test08PortfolioTransactionsIncludesAllItsAccountsTransactions

	| account portfolio accountDeposit portfolioTransactions anotherAccountWithdraw |
	
	account := ReceptiveAccount new.
	accountDeposit := Deposit register: 100 * peso on: account.
	anotherAccountWithdraw := Withdraw register: 100 * peso on: account.
	portfolio := Portfolio with: account.
	
	portfolioTransactions := portfolio transactions.
	
	self assert: 2 equals: portfolioTransactions size.
	self assert: (portfolioTransactions includes: accountDeposit).
	self assert: (portfolioTransactions includes: anotherAccountWithdraw)! !

!PortfolioTest methodsFor: 'tests' stamp: 'LL 7/1/2021 03:28:20'!
test09PortfolioTransactionsAreCalculatedRecursively

	| simplePortfolioAccount simplePortfolio composedPortfolioAccount composedPortfolio composedPortfolioAccountWithdraw simplePortfolioAccountDeposit composedPortfolioTransactions |
	
	simplePortfolioAccount := ReceptiveAccount new.
	simplePortfolioAccountDeposit := Deposit register: 100 * peso on: simplePortfolioAccount.
	simplePortfolio := Portfolio with: simplePortfolioAccount.
	
	composedPortfolioAccount := ReceptiveAccount new.
	composedPortfolioAccountWithdraw := Withdraw register: 50 * peso on: composedPortfolioAccount.
	composedPortfolio := Portfolio with: simplePortfolio with: composedPortfolioAccount.
	
	composedPortfolioTransactions := composedPortfolio transactions.
	self assert: 2 equals: composedPortfolioTransactions size.
	self assert: (composedPortfolioTransactions includes: simplePortfolioAccountDeposit).
	self assert: (composedPortfolioTransactions includes: composedPortfolioAccountWithdraw)! !

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 11:58:24'!
test10PortfolioCanNotIncludeTheSameAccountMoreThanOnce

	| account portfolio |
	
	account := ReceptiveAccount new.
	portfolio := Portfolio with: account.
	
	self 
		should: [ portfolio add: account ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: Portfolio canNotAddAccountErrorMessage equals: anError messageText.
			self assert: 1 equals: portfolio accountsSize.
			self assert: (portfolio accountsIncludes: account) ]! !

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 11:58:28'!
test11PortfolioCanNotIncludeAccountOfItsPortfolios

	| account simplePortfolio composedPortfolio |
	
	account := ReceptiveAccount new.
	simplePortfolio := Portfolio with: account.
	composedPortfolio := Portfolio with: simplePortfolio.
	
	self 
		should: [ composedPortfolio add: account ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: Portfolio canNotAddAccountErrorMessage equals: anError messageText.
			self assert: 1 equals: composedPortfolio accountsSize.
			self assert: (composedPortfolio accountsIncludes: simplePortfolio) ]! !

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 11:58:32'!
test12PortfolioCanNotIncludeItself

	| account simplePortfolio |
	
	account := ReceptiveAccount new.
	simplePortfolio := Portfolio with: account.
	
	self 
		should: [ simplePortfolio add: simplePortfolio ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: Portfolio canNotAddAccountErrorMessage equals: anError messageText.
			self assert: 1 equals: simplePortfolio accountsSize.
			self assert: (simplePortfolio accountsIncludes: account) ]! !

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 12:01:51'!
test13ComposedPortfolioCanNotHaveParentPortfolioAccount

	| account simplePortfolio composedPortfolio |
	
	account := ReceptiveAccount new.
	simplePortfolio := Portfolio new.
	composedPortfolio := Portfolio with: simplePortfolio.
	composedPortfolio add: account.
	
	self 
		should: [ simplePortfolio add: account ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: Portfolio canNotAddAccountErrorMessage equals: anError messageText.
			self assert: simplePortfolio accountsIsEmpty ]! !

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/25/2019 12:12:16'!
test14ComposedPortfolioCanNotHaveAccountOfAnyRootParentRecursively

	| account leftParentPortfolio leftRootParentPortfolio portfolio rightParentPortfolio rightRootParentPortfolio |
	
	account := ReceptiveAccount new.
	portfolio := Portfolio new.
	leftParentPortfolio := Portfolio with: portfolio .
	leftRootParentPortfolio := Portfolio with: leftParentPortfolio.
	leftRootParentPortfolio add: account.
	
	rightParentPortfolio := Portfolio with: portfolio .
	rightRootParentPortfolio := Portfolio with: rightParentPortfolio.
	rightRootParentPortfolio add: account.

	self 
		should: [ portfolio add: account ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: Portfolio canNotAddAccountErrorMessage equals: anError messageText.
			self assert: portfolio accountsIsEmpty ]! !

!PortfolioTest methodsFor: 'tests' stamp: 'HAW 5/29/2019 16:31:18'!
test15PortfolioCanNotIncludeAnyOfTheComposedAccountOfPortfolioToAdd

	| portfolioToAdd portfolioToModify rootPortfolio sharedAccount |
	
	sharedAccount := ReceptiveAccount new.
	portfolioToModify := Portfolio new.
	rootPortfolio := Portfolio with: sharedAccount with: portfolioToModify.
	portfolioToAdd := Portfolio with: sharedAccount.
	
	self 
		should: [ portfolioToModify add: portfolioToAdd ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError | 
			self assert: Portfolio canNotAddAccountErrorMessage equals: anError messageText.
			self assert: portfolioToModify accountsIsEmpty ]! !


!classDefinition: #ReceptiveAccountTest category: 'ServiciosFinancieros-Solucion'!
TestCase subclass: #ReceptiveAccountTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

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


!classDefinition: #ReportesTest category: 'ServiciosFinancieros-Solucion'!
TestCase subclass: #ReportesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!


!classDefinition: #NetoDeTransferenciasTest category: 'ServiciosFinancieros-Solucion'!
ReportesTest subclass: #NetoDeTransferenciasTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!NetoDeTransferenciasTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:25:31'!
test01NetoDeTransferenciaEnCuentaVacia
	| cuentaVacia reporte |
	cuentaVacia := ReceptiveAccount new.
	reporte := NetoDeTransferencias para: cuentaVacia.
	
	self assert: 'Neto de transferencias = 0 pesos' equals: reporte resumen.! !

!NetoDeTransferenciasTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:25:55'!
test02NetoDeTransferenciaEnCuentaConTransferenciaDeposito
	| originAccount destinationAccount reporte |

	originAccount := ReceptiveAccount new.
	destinationAccount := ReceptiveAccount new.
	reporte := NetoDeTransferencias para: destinationAccount.

	Transfer amount: 100 * peso from: originAccount to: destinationAccount.

	self assert: 'Neto de transferencias = 100 pesos' equals: reporte resumen.! !

!NetoDeTransferenciasTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:26:25'!
test03NetoDeTransferenciaEnCuentaConTransferenciaExtraccion
	| originAccount destinationAccount reporte |

	originAccount := ReceptiveAccount new.
	destinationAccount := ReceptiveAccount new.
	reporte := NetoDeTransferencias para: originAccount.

	Transfer amount: 100 * peso from: originAccount to: destinationAccount.

	self assert: 'Neto de transferencias = -100 pesos' equals: reporte resumen.! !

!NetoDeTransferenciasTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:26:43'!
test04NetoDeTransferenciaEnCuentaConDeposito
	| cuenta reporte |

	cuenta := ReceptiveAccount new.
	reporte := NetoDeTransferencias para: cuenta.

	Deposit register: 100 * peso on: cuenta.

	self assert: 'Neto de transferencias = 0 pesos' equals: reporte resumen.! !

!NetoDeTransferenciasTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:26:57'!
test05NetoDeTransferenciaEnCuentaConExtraccion
	| cuenta reporte |

	cuenta := ReceptiveAccount new.
	reporte := NetoDeTransferencias para: cuenta.

	Withdraw register: 100 * peso on: cuenta.

	self assert: 'Neto de transferencias = 0 pesos' equals: reporte resumen.! !


!classDefinition: #ReporteDeArbolDetalladoTest category: 'ServiciosFinancieros-Solucion'!
ReportesTest subclass: #ReporteDeArbolDetalladoTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ReporteDeArbolDetalladoTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:31:03'!
test01ReporteDeArbolDetalladoEnPortfolioVacio
	| portfolioVacio reporte |

	portfolioVacio := Portfolio named: 'Portfolio vacio'.
	reporte := ReporteDeArbolDetallado para: portfolioVacio.
	
	self assert: 'Portfolio vacio' equals: reporte resumen.! !

!ReporteDeArbolDetalladoTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:31:39'!
test02ReporteDeArbolEnPortfolioConCuentaConDeposito
	| portfolio cuenta reporte |

	cuenta :=  ReceptiveAccount named: 'Cuenta mia'.
	Deposit register: 200 * peso on: cuenta.
	portfolio := Portfolio named: 'Portfolio de la familia' with: cuenta.
	reporte := ReporteDeArbolDetallado para: portfolio.

	self assert:
		'Portfolio de la familia',
		Character cr asString,
   		'	Cuenta mia',
		Character cr asString,
		'		Depósito por 200 pesos',
		Character cr asString,
		'		Balance = 200 pesos'
	equals: reporte resumen.! !

!ReporteDeArbolDetalladoTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:33:51'!
test03ReporteDeArbolEnPortfolioConPortfolioConCuentaVacia
	| portfolio cuenta reporte portfolio2 |

	portfolio := Portfolio named: 'Portfolio de la familia'.
	portfolio2 := Portfolio named: 'Portfolio de hijos'.
	cuenta :=  ReceptiveAccount named: 'Cuenta de Juan'.

	portfolio2 add: cuenta.
	portfolio add: portfolio2.
	reporte := ReporteDeArbolDetallado para: portfolio.

	self assert:
		'Portfolio de la familia',
		Character cr asString,
		'	Portfolio de hijos',
		Character cr asString,
		'		Cuenta de Juan',
		Character cr asString,
		'			Balance = 0 pesos'
	equals: reporte resumen.! !


!classDefinition: #ReporteDeArbolTest category: 'ServiciosFinancieros-Solucion'!
ReportesTest subclass: #ReporteDeArbolTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ReporteDeArbolTest methodsFor: 'tests' stamp: 'S.V. 6/11/2023 22:27:35'!
test01ReporteDeArbolEnPortfolioVacio
	| portfolioVacio reporte |

	portfolioVacio := Portfolio named: 'Portfolio vacio'.
	reporte := ReporteDeArbol para: portfolioVacio.
	
	self assert: 'Portfolio vacio' equals: reporte resumen.! !

!ReporteDeArbolTest methodsFor: 'tests' stamp: 'S.V. 6/11/2023 22:28:04'!
test02ReporteDeArbolEnPortfolioConCuenta
	| portfolio cuenta reporte |

	cuenta :=  ReceptiveAccount named: 'Cuenta mia'.
	portfolio := Portfolio named: 'Portfolio de la familia' with: cuenta.
	reporte := ReporteDeArbol para: portfolio.
	
	self assert:
		'Portfolio de la familia',
		Character cr asString,
   		'	Cuenta mia'
	equals: reporte resumen.! !

!ReporteDeArbolTest methodsFor: 'tests' stamp: 'S.V. 6/11/2023 22:30:10'!
test03ReporteDeArbolEnPortfolioConPortfolioConCuenta
	| portfolio cuenta reporte portfolio2 |

	portfolio := Portfolio named: 'Portfolio de la familia'.
	portfolio2 := Portfolio named: 'Portfolio de hijos'.
	cuenta :=  ReceptiveAccount named: 'Cuenta de Juan'.
	
	portfolio2 add: cuenta.
	portfolio add: portfolio2.
	reporte := ReporteDeArbol para: portfolio.
	
	self assert:
		'Portfolio de la familia',
		Character cr asString,
		'	Portfolio de hijos',
		Character cr asString,
		'		Cuenta de Juan'
	equals: reporte resumen.! !


!classDefinition: #ResumenDeCuentaTest category: 'ServiciosFinancieros-Solucion'!
ReportesTest subclass: #ResumenDeCuentaTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ResumenDeCuentaTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:23:34'!
test01ResumenDeCuentaEnCuentaVacia
	| cuentaVacia reporte |
	cuentaVacia := ReceptiveAccount new.
	reporte := ResumenDeCuenta para: cuentaVacia.
	
	self assert: 'Balance = 0 pesos' equals: reporte resumen.! !

!ResumenDeCuentaTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:23:21'!
test02ResumenDeCuentaEnCuentaConDeposito
	| cuenta reporte |
	cuenta := ReceptiveAccount new.
	reporte := ResumenDeCuenta para: cuenta.
	Deposit register: 100 * peso on: cuenta.

	self assert: 
		'Depósito por 100 pesos',
		Character cr asString,
		'Balance = 100 pesos'
	equals: reporte resumen.! !

!ResumenDeCuentaTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:23:51'!
test03ResumenDeCuentaEnCuentaConExtraccion
	| cuenta reporte |
	cuenta := ReceptiveAccount new.
	reporte := ResumenDeCuenta para: cuenta.
	Withdraw register: 100 * peso on: cuenta.

	self assert: 
		'Extracción por 100 pesos',
		Character cr asString,
		'Balance = -100 pesos'
	equals: reporte resumen.! !

!ResumenDeCuentaTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:24:13'!
test04ResumenDeCuentaEnCuentaConTransferenciaExtraccion
	| originAccount destinationAccount reporte |
	originAccount := ReceptiveAccount new.
	destinationAccount := ReceptiveAccount new.
	reporte := ResumenDeCuenta para: originAccount.
	Transfer amount: 100 * peso from: originAccount to: destinationAccount.

	self assert: 
		'Salida por transferencia de 100 pesos',
		Character cr asString,
		'Balance = -100 pesos'
	equals: reporte resumen.! !

!ResumenDeCuentaTest methodsFor: 'test' stamp: 'S.V. 6/11/2023 22:24:28'!
test05ResumenDeCuentaEnCuentaConTranferenciaDeposito
	| originAccount destinationAccount reporte |
	originAccount := ReceptiveAccount new.
	destinationAccount := ReceptiveAccount new.
	reporte := ResumenDeCuenta para: destinationAccount.
	Transfer amount: 100 * peso from: originAccount to: destinationAccount.

	self assert: 
		'Entrada por transferencia de 100 pesos',
		Character cr asString,
		'Balance = 100 pesos'
	equals: reporte resumen.! !


!classDefinition: #TransferTest category: 'ServiciosFinancieros-Solucion'!
TestCase subclass: #TransferTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!TransferTest methodsFor: 'tests' stamp: 'LL 9/20/2020 21:07:03'!
test01ATransferDecreasesBalanceFromOriginAccountAndIncreasesItForDestinationAccount

	| destinationAccount originAccount |
	
	originAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: originAccount.
	
	destinationAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: destinationAccount.
	
	Transfer amount: 2 * peso from: originAccount to: destinationAccount.
	
	self assert: 8 * peso equals: originAccount balance.
	self assert: 12 * peso equals: destinationAccount balance.! !

!TransferTest methodsFor: 'tests' stamp: 'LL 9/20/2020 21:12:28'!
test02ATransferKnowsItsValue

	| destinationAccount originAccount transfer |
	
	originAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: originAccount.
	
	destinationAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: destinationAccount.
	
	transfer := Transfer amount: 2 * peso from: originAccount to: destinationAccount.
	
	self assert: 2 * peso equals: transfer value.! !

!TransferTest methodsFor: 'tests' stamp: 'LL 9/20/2020 23:01:44'!
test03DepositLegKnowsItsWithdrawCounterpart

	| destinationAccount originAccount transfer |
	
	originAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: originAccount.
	
	destinationAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: destinationAccount.
	
	transfer := Transfer amount: 2 * peso from: originAccount to: destinationAccount.
	
	self assert: transfer withdrawLeg equals: transfer depositLeg withdrawLeg! !

!TransferTest methodsFor: 'tests' stamp: 'LL 9/20/2020 23:01:51'!
test04WithdrawLegKnowsItsDepositCounterpart

	| destinationAccount originAccount transfer |
	
	originAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: originAccount.
	
	destinationAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: destinationAccount.
	
	transfer := Transfer amount: 2 * peso from: originAccount to: destinationAccount.
	
	self assert: transfer depositLeg equals: transfer withdrawLeg depositLeg! !

!TransferTest methodsFor: 'tests' stamp: 'LL 9/20/2020 23:02:36'!
test05OriginAndDestinationAccountsCannotBeTheSame

	| originAccount |
	
	originAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: originAccount.
	
	self should: [ Transfer amount: 2 * peso from: originAccount to: originAccount ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: Transfer cannotTransferToOriginErrorDescription equals: anError messageText.
			self assert: 10 * peso equals: originAccount balance.
		]
! !

!TransferTest methodsFor: 'tests' stamp: 'LL 9/20/2020 23:02:44'!
test06CannotTransferAZeroAmount

	| originAccount destinationAccount |
	
	originAccount := ReceptiveAccount new.
	Deposit register: 10 * peso on: originAccount.
	
	destinationAccount := ReceptiveAccount new.
	Deposit register: 5 * peso on: destinationAccount.
	
	self should: [ Transfer amount: 0 from: originAccount to: destinationAccount ]
		raise: Error - MessageNotUnderstood 
		withExceptionDo: [ :anError |
			self assert: Transfer cannotTransferZeroErrorDescription equals: anError messageText.
			self assert: 10 * peso equals: originAccount balance.
		]
! !


!classDefinition: #Account category: 'ServiciosFinancieros-Solucion'!
Object subclass: #Account
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!Account methodsFor: 'main protocol' stamp: 'HAW 5/25/2019 12:23:40'!
balance

	self subclassResponsibility ! !

!Account methodsFor: 'main protocol' stamp: 'HAW 5/25/2019 12:23:47'!
hasRegistered: aTransaction

	self subclassResponsibility ! !

!Account methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 18:55:04'!
name
	^ name! !

!Account methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 21:42:18'!
reportePara: unReporte enProfundidad: unaProfundidad
	self subclassResponsibility! !

!Account methodsFor: 'main protocol' stamp: 'HAW 5/25/2019 12:23:15'!
transactions

	self subclassResponsibility ! !

!Account methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 15:40:33'!
transactionsDo: AClosureToDo
	self transactions do: AClosureToDo.! !


!Account methodsFor: 'composition' stamp: 'LL 7/8/2021 21:45:50'!
addedTo: aPortfolio

	self subclassResponsibility! !

!Account methodsFor: 'composition' stamp: 'LL 7/8/2021 21:46:48'!
isComposedBy: anAccount

	self subclassResponsibility! !


!Account methodsFor: 'private' stamp: 'HAW 5/25/2019 12:23:27'!
addTransactionsTo: aCollectionOfTransactions

	self subclassResponsibility ! !


!Account methodsFor: 'initialization' stamp: 'S.V. 6/11/2023 22:20:49'!
initialize
	name := 'undefined'! !

!Account methodsFor: 'initialization' stamp: 'S.V. 6/11/2023 22:21:28'!
setName: accountName
	name := accountName.
	! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Account class' category: 'ServiciosFinancieros-Solucion'!
Account class
	instanceVariableNames: ''!

!Account class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 18:26:21'!
named: aName
	^ self new setName: aName.! !


!classDefinition: #Portfolio category: 'ServiciosFinancieros-Solucion'!
Account subclass: #Portfolio
	instanceVariableNames: 'accounts parents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!Portfolio methodsFor: 'initialization' stamp: 'S.V. 6/11/2023 22:21:11'!
initialize
	super initialize.
	accounts := OrderedCollection new.
	parents := OrderedCollection new.! !


!Portfolio methodsFor: 'main protocol' stamp: 'HAW 5/25/2019 11:19:36'!
balance
	
	^accounts sum: [ :anAccount | anAccount balance ] ifEmpty: [ 0 ]! !

!Portfolio methodsFor: 'main protocol' stamp: 'HAW 5/25/2019 11:28:29'!
hasRegistered: aTransaction

	^accounts anySatisfy: [ :anAccount | anAccount hasRegistered: aTransaction ]! !

!Portfolio methodsFor: 'main protocol' stamp: 'HAW 5/25/2019 11:38:32'!
transactions
	
	| transactions |
	
	transactions := OrderedCollection new.
	accounts do: [ :anAccount | anAccount addTransactionsTo: transactions ].
	
	^transactions ! !


!Portfolio methodsFor: 'accounts management' stamp: 'S.V. 6/11/2023 18:39:10'!
accountsDo: AClosureToDo
	accounts do: AClosureToDo.! !

!Portfolio methodsFor: 'accounts management' stamp: 'HAW 5/25/2019 11:49:20'!
accountsIncludes: anAccount

	^accounts includes: anAccount ! !

!Portfolio methodsFor: 'accounts management' stamp: 'HAW 5/25/2019 12:05:04'!
accountsIsEmpty
	
	^accounts isEmpty ! !

!Portfolio methodsFor: 'accounts management' stamp: 'HAW 5/25/2019 11:49:06'!
accountsSize
	
	^accounts size! !

!Portfolio methodsFor: 'accounts management' stamp: 'HAW 5/25/2019 12:19:20'!
add: accountToAdd

	self assertCanAdd: accountToAdd.
		
	accounts add: accountToAdd.
	accountToAdd addedTo: self 
	! !


!Portfolio methodsFor: 'private' stamp: 'HAW 5/25/2019 11:42:55'!
addTransactionsTo: aCollectionOfTransactions

	accounts do: [ :anAccount | anAccount addTransactionsTo: aCollectionOfTransactions ]! !

!Portfolio methodsFor: 'private' stamp: 'S.V. 6/11/2023 21:42:18'!
reportePara: unReporte enProfundidad: unaProfundidad
	^ unReporte reporteDePortfolio: self enProfundidad: unaProfundidad.! !


!Portfolio methodsFor: 'composition' stamp: 'HAW 5/25/2019 12:17:31'!
addRootParentsTo: rootParents

	parents 
		ifEmpty: [ rootParents add: self ] 
		ifNotEmpty: [ parents do: [ :aParent | aParent addRootParentsTo: rootParents ]]! !

!Portfolio methodsFor: 'composition' stamp: 'HAW 5/25/2019 12:02:59'!
addedTo: aPortfolio 
	
	parents add: aPortfolio ! !

!Portfolio methodsFor: 'composition' stamp: 'HAW 5/25/2019 12:20:56'!
anyRootParentIsComposedBy: accountToAdd

	^self rootParents anySatisfy: [ :aParent | aParent isComposedBy: accountToAdd]! !

!Portfolio methodsFor: 'composition' stamp: 'HAW 5/25/2019 12:20:36'!
assertCanAdd: accountToAdd

	(self anyRootParentIsComposedBy: accountToAdd) ifTrue: [ self signalCanNotAddAccount ].
! !

!Portfolio methodsFor: 'composition' stamp: 'HAW 5/29/2019 16:24:54'!
isComposedBy: anAccount

	^ self = anAccount or: [ accounts anySatisfy: [ :composedAccount | (composedAccount isComposedBy: anAccount) or: [ anAccount isComposedBy: composedAccount ]]]! !

!Portfolio methodsFor: 'composition' stamp: 'HAW 5/25/2019 12:17:31'!
rootParents
	
	| rootParents |
	
	rootParents := Set new.
	self addRootParentsTo: rootParents.
	
	^ rootParents! !

!Portfolio methodsFor: 'composition' stamp: 'HAW 5/25/2019 11:48:34'!
signalCanNotAddAccount
	
	self error: self class canNotAddAccountErrorMessage! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Portfolio class' category: 'ServiciosFinancieros-Solucion'!
Portfolio class
	instanceVariableNames: ''!

!Portfolio class methodsFor: 'errors' stamp: 'HAW 5/25/2019 11:48:55'!
canNotAddAccountErrorMessage
	
	^'Can not add repeated account to a portfolio'! !


!Portfolio class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 18:10:29'!
named: aName with: anAccount

	^self new 
		setName: aName;
		add: anAccount;
		yourself! !

!Portfolio class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 18:10:51'!
named: aName with: anAccount with: anotherAccount

	^self new 
		setName: aName;
		add: anAccount;
		add: anotherAccount;
		yourself! !

!Portfolio class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 18:11:17'!
with: anAccount

	^self new 
		setName: 'undefined';
		add: anAccount;
		yourself! !

!Portfolio class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 18:11:21'!
with: anAccount with: anotherAccount

	^self new 
		setName: 'undefined';
		add: anAccount;
		add: anotherAccount;
		yourself! !


!classDefinition: #ReceptiveAccount category: 'ServiciosFinancieros-Solucion'!
Account subclass: #ReceptiveAccount
	instanceVariableNames: 'transactions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ReceptiveAccount methodsFor: 'initialization' stamp: 'S.V. 6/11/2023 22:21:17'!
initialize
	super initialize.
	transactions := OrderedCollection new.! !


!ReceptiveAccount methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 16:41:44'!
balance

	^transactions 
		inject: 0 * peso
		into: [ :currentBalance :transaction | transaction affectBalance: currentBalance ]! !

!ReceptiveAccount methodsFor: 'main protocol' stamp: 'NR 10/21/2019 18:55:56'!
hasRegistered: aTransaction

	^ transactions includes: aTransaction 
! !

!ReceptiveAccount methodsFor: 'main protocol' stamp: 'HernanWilkinson 7/13/2011 18:37'!
register: aTransaction

	transactions add: aTransaction 
! !

!ReceptiveAccount methodsFor: 'main protocol' stamp: 'HernanWilkinson 7/13/2011 18:37'!
transactions 

	^ transactions copy! !


!ReceptiveAccount methodsFor: 'private' stamp: 'HAW 5/25/2019 11:38:52'!
addTransactionsTo: aCollectionOfTransactions

	aCollectionOfTransactions addAll: transactions ! !

!ReceptiveAccount methodsFor: 'private' stamp: 'S.V. 6/11/2023 21:42:19'!
reportePara: unReporte enProfundidad: unaProfundidad
	^ unReporte reporteDeCuenta: self enProfundidad: unaProfundidad.! !


!ReceptiveAccount methodsFor: 'composition' stamp: 'HAW 5/25/2019 12:03:32'!
addedTo: aPortfolio 
	
	! !

!ReceptiveAccount methodsFor: 'composition' stamp: 'HAW 5/25/2019 11:54:51'!
isComposedBy: anAccount

	^self = anAccount ! !


!classDefinition: #AccountTransaction category: 'ServiciosFinancieros-Solucion'!
Object subclass: #AccountTransaction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!AccountTransaction methodsFor: 'value' stamp: 'HernanWilkinson 9/12/2011 12:25'!
value 

	self subclassResponsibility ! !


!AccountTransaction methodsFor: 'balance' stamp: 'HAW 5/25/2019 11:25:39'!
affectBalance: aBalance

	self subclassResponsibility ! !


!AccountTransaction methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:40:05'!
reportePara: aReporte
	self subclassResponsibility.! !


!classDefinition: #Deposit category: 'ServiciosFinancieros-Solucion'!
AccountTransaction subclass: #Deposit
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!Deposit methodsFor: 'initialization' stamp: 'HernanWilkinson 7/13/2011 18:45'!
initializeFor: aValue

	value := aValue ! !


!Deposit methodsFor: 'value' stamp: 'HernanWilkinson 7/13/2011 18:38'!
value

	^ value! !


!Deposit methodsFor: 'private' stamp: 'S.V. 6/11/2023 21:43:51'!
reportePara: aReporte
	^ aReporte reporteDeDeposito: self.! !


!Deposit methodsFor: 'balance' stamp: 'HAW 5/25/2019 11:25:02'!
affectBalance: aBalance

	^aBalance + value ! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Deposit class' category: 'ServiciosFinancieros-Solucion'!
Deposit class
	instanceVariableNames: ''!

!Deposit class methodsFor: 'instance creation' stamp: 'HernanWilkinson 7/13/2011 18:38'!
for: aValue

	^ self new initializeFor: aValue ! !

!Deposit class methodsFor: 'instance creation' stamp: 'LL 7/8/2021 21:45:00'!
register: aValue on: account

	| transaction |
	
	transaction := self for: aValue.
	account register: transaction.
		
	^ transaction! !


!classDefinition: #TransferLeg category: 'ServiciosFinancieros-Solucion'!
AccountTransaction subclass: #TransferLeg
	instanceVariableNames: 'transfer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!TransferLeg methodsFor: 'value' stamp: 'LL 9/20/2020 22:27:32'!
value

	^transfer value! !


!TransferLeg methodsFor: 'initialization' stamp: 'LL 9/20/2020 22:28:14'!
initializeWith: aTransfer

	transfer := aTransfer! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'TransferLeg class' category: 'ServiciosFinancieros-Solucion'!
TransferLeg class
	instanceVariableNames: ''!

!TransferLeg class methodsFor: 'instance creation' stamp: 'LL 9/20/2020 22:45:01'!
of: aTransfer on: anAccount

	| transaction |
	
	transaction := self new initializeWith: 	aTransfer.
	anAccount register: transaction.	
	
	^transaction! !


!classDefinition: #TransferDepositLeg category: 'ServiciosFinancieros-Solucion'!
TransferLeg subclass: #TransferDepositLeg
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!TransferDepositLeg methodsFor: 'accessing' stamp: 'LL 9/20/2020 22:26:10'!
withdrawLeg

	^transfer withdrawLeg! !


!TransferDepositLeg methodsFor: 'balance' stamp: 'LL 9/20/2020 22:27:49'!
affectBalance: aBalance

	^aBalance + self value! !


!TransferDepositLeg methodsFor: 'private' stamp: 'S.V. 6/11/2023 21:43:51'!
reportePara: aReporte
	^ aReporte reporteDeEntradaPorTransferencia: self.! !


!classDefinition: #TransferWithdrawLeg category: 'ServiciosFinancieros-Solucion'!
TransferLeg subclass: #TransferWithdrawLeg
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!TransferWithdrawLeg methodsFor: 'accessing' stamp: 'LL 9/20/2020 22:25:47'!
depositLeg

	^transfer depositLeg! !


!TransferWithdrawLeg methodsFor: 'balance' stamp: 'LL 9/20/2020 22:27:55'!
affectBalance: aBalance

	^aBalance - self value! !


!TransferWithdrawLeg methodsFor: 'private' stamp: 'S.V. 6/11/2023 21:43:52'!
reportePara: aReporte
	^ aReporte reporteDeSalidaPorTransferencia: self.! !


!classDefinition: #Withdraw category: 'ServiciosFinancieros-Solucion'!
AccountTransaction subclass: #Withdraw
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!Withdraw methodsFor: 'initialization' stamp: 'HernanWilkinson 7/13/2011 18:46'!
initializeFor: aValue

	value := aValue ! !


!Withdraw methodsFor: 'value' stamp: 'HernanWilkinson 7/13/2011 18:33'!
value

	^ value! !


!Withdraw methodsFor: 'balance' stamp: 'HAW 5/25/2019 11:25:15'!
affectBalance: aBalance

	^aBalance - value! !


!Withdraw methodsFor: 'private' stamp: 'S.V. 6/11/2023 21:43:52'!
reportePara: aReporte
	^ aReporte reporteDeExtraccion: self.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Withdraw class' category: 'ServiciosFinancieros-Solucion'!
Withdraw class
	instanceVariableNames: ''!

!Withdraw class methodsFor: 'instance creation' stamp: 'HernanWilkinson 7/13/2011 18:33'!
for: aValue

	^ self new initializeFor: aValue ! !

!Withdraw class methodsFor: 'instance creation' stamp: 'LL 7/8/2021 21:45:05'!
register: aValue on: account

	| transaction |
	
	transaction := self for: aValue.
	account register: transaction.
		
	^ transaction! !


!classDefinition: #Reporte category: 'ServiciosFinancieros-Solucion'!
Object subclass: #Reporte
	instanceVariableNames: 'cuenta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!Reporte methodsFor: 'initialization' stamp: 'S.V. 6/11/2023 20:25:13'!
para: cuentaAReportar
	cuenta := cuentaAReportar.! !


!Reporte methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 20:24:56'!
resumen
	self subclassResponsibility.! !


!Reporte methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:12:51'!
saltoDeLinea
	^ Character cr asString.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Reporte class' category: 'ServiciosFinancieros-Solucion'!
Reporte class
	instanceVariableNames: ''!

!Reporte class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 20:25:29'!
para: cuentaAReportar
	self subclassResponsibility.! !


!classDefinition: #ReporteDeCuentas category: 'ServiciosFinancieros-Solucion'!
Reporte subclass: #ReporteDeCuentas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ReporteDeCuentas methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:17:40'!
generarTabs: numeroDeTabs
	| indentacion |

	indentacion := String new.
	numeroDeTabs timesRepeat: [ indentacion := indentacion, (Character tab asString)].

	^ indentacion.! !

!ReporteDeCuentas methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:13:09'!
indentar: unBloqueDeTexto enProfundidad: unaProfundidad
	| lineas lineasIndentadas bloqueDeTextoIndentado |

	lineas := unBloqueDeTexto lines.
	lineasIndentadas := lineas collect: [ :unaLinea | (self generarTabs: unaProfundidad), unaLinea ].
	bloqueDeTextoIndentado := (self saltoDeLinea) join: lineasIndentadas.

	^ bloqueDeTextoIndentado.! !

!ReporteDeCuentas methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:54:53'!
reporteDeCuenta: unaCuenta enProfundidad: unaProfundidad
	self subclassResponsibility.! !

!ReporteDeCuentas methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:55:03'!
reporteDePortfolio: unPortfolio enProfundidad: unaProfundidad
	self subclassResponsibility.! !


!classDefinition: #ReporteDeArbol category: 'ServiciosFinancieros-Solucion'!
ReporteDeCuentas subclass: #ReporteDeArbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ReporteDeArbol methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:13:25'!
reporteDeCuenta: unaCuenta enProfundidad: unaProfundidad
	^ (self indentar: (unaCuenta name) enProfundidad: unaProfundidad), self saltoDeLinea.! !

!ReporteDeArbol methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:18:09'!
reporteDePortfolio: unPortfolio enProfundidad: unaProfundidad
	| reporteString |
	
	reporteString := String new.
	reporteString := reporteString, (self indentar: (unPortfolio name) enProfundidad: unaProfundidad), self saltoDeLinea.
	unPortfolio accountsDo: [ :unaCuenta | reporteString := reporteString, (unaCuenta reportePara: self enProfundidad: (unaProfundidad + 1)) ].

	^ reporteString! !


!ReporteDeArbol methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 21:41:26'!
resumen
	^ (cuenta reportePara: self enProfundidad: 0) allButLast.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ReporteDeArbol class' category: 'ServiciosFinancieros-Solucion'!
ReporteDeArbol class
	instanceVariableNames: ''!

!ReporteDeArbol class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 18:16:27'!
para: cuentaAReportar
	^ ReporteDeArbol new para: cuentaAReportar.! !


!classDefinition: #ReporteDeArbolDetallado category: 'ServiciosFinancieros-Solucion'!
ReporteDeCuentas subclass: #ReporteDeArbolDetallado
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ReporteDeArbolDetallado methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:18:15'!
reporteDeCuenta: unaCuenta enProfundidad: unaProfundidad
	| reporteString reporteResumenDeCuenta |
	
	reporteString := String new.
	reporteString := reporteString, (self indentar: (unaCuenta name) enProfundidad: unaProfundidad), self saltoDeLinea.
	reporteResumenDeCuenta := ResumenDeCuenta para: unaCuenta.
	reporteString := reporteString, (self indentar: (reporteResumenDeCuenta resumen) enProfundidad: (unaProfundidad + 1)), self saltoDeLinea.
	
	^ reporteString.! !

!ReporteDeArbolDetallado methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:18:19'!
reporteDePortfolio: unPortfolio enProfundidad: unaProfundidad
	| reporteString |
	
	reporteString := String new.
	reporteString := reporteString, (self indentar: (unPortfolio name) enProfundidad: unaProfundidad), self saltoDeLinea.
	unPortfolio accountsDo: [ :unaCuenta | reporteString := reporteString, (unaCuenta reportePara: self enProfundidad: (unaProfundidad + 1)). ].

	^ reporteString! !


!ReporteDeArbolDetallado methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 21:42:47'!
resumen
	^ (cuenta reportePara: self enProfundidad: 0) allButLast.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ReporteDeArbolDetallado class' category: 'ServiciosFinancieros-Solucion'!
ReporteDeArbolDetallado class
	instanceVariableNames: ''!

!ReporteDeArbolDetallado class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 20:30:43'!
para: cuentaAReportar
	^ ReporteDeArbolDetallado new para: cuentaAReportar.! !


!classDefinition: #ReporteDeTransferencias category: 'ServiciosFinancieros-Solucion'!
Reporte subclass: #ReporteDeTransferencias
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ReporteDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:53:32'!
reporteDeDeposito: aDeposit
	self subclassResponsibility.! !

!ReporteDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:53:46'!
reporteDeEntradaPorTransferencia: aTransferDepositLeg
	self subclassResponsibility.! !

!ReporteDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:54:08'!
reporteDeExtraccion: aWithdraw
	self subclassResponsibility.! !

!ReporteDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:54:28'!
reporteDeSalidaPorTransferencia: aTransferWithdrawLeg
	self subclassResponsibility.! !


!classDefinition: #NetoDeTransferencias category: 'ServiciosFinancieros-Solucion'!
ReporteDeTransferencias subclass: #NetoDeTransferencias
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!NetoDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:53:32'!
reporteDeDeposito: aDeposit
	^ 0 * peso.! !

!NetoDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:53:46'!
reporteDeEntradaPorTransferencia: aTransferDepositLeg
	^ aTransferDepositLeg value.! !

!NetoDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:54:08'!
reporteDeExtraccion: aWithdraw
	^ 0 * peso.! !

!NetoDeTransferencias methodsFor: 'private' stamp: 'S.V. 6/11/2023 20:54:28'!
reporteDeSalidaPorTransferencia: aTransferWithdrawLeg
	^ aTransferWithdrawLeg value negated.! !


!NetoDeTransferencias methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 22:11:19'!
resumen
	| valorDeLasTransacciones total |

	valorDeLasTransacciones := OrderedCollection new.
	cuenta transactionsDo: [ :aTransaction | valorDeLasTransacciones add: (aTransaction reportePara: self) ].
	total :=  valorDeLasTransacciones inject: 0 * peso into: [ :acumulador :valorDeLaTransaccion | acumulador + valorDeLaTransaccion ].

	^ 'Neto de transferencias = ', (total amount asString), ' pesos'.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'NetoDeTransferencias class' category: 'ServiciosFinancieros-Solucion'!
NetoDeTransferencias class
	instanceVariableNames: ''!

!NetoDeTransferencias class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 17:14:02'!
para: cuentaAReportar
	^ NetoDeTransferencias new para: cuentaAReportar.! !


!classDefinition: #ResumenDeCuenta category: 'ServiciosFinancieros-Solucion'!
ReporteDeTransferencias subclass: #ResumenDeCuenta
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!ResumenDeCuenta methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:14:46'!
reporteDeDeposito: aDeposit
	^ 'Depósito por ', aDeposit value amount asString, ' pesos', self saltoDeLinea.! !

!ResumenDeCuenta methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:14:42'!
reporteDeEntradaPorTransferencia: aTransferDepositLeg
	^ 'Entrada por transferencia de ', aTransferDepositLeg value amount asString, ' pesos', self saltoDeLinea.! !

!ResumenDeCuenta methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:14:49'!
reporteDeExtraccion: aWithdraw
	^ 'Extracción por ', aWithdraw value amount asString, ' pesos', self saltoDeLinea.! !

!ResumenDeCuenta methodsFor: 'private' stamp: 'S.V. 6/11/2023 22:14:53'!
reporteDeSalidaPorTransferencia: aTransferWithdrawLeg
	^ 'Salida por transferencia de ', aTransferWithdrawLeg value amount asString, ' pesos', self saltoDeLinea.! !


!ResumenDeCuenta methodsFor: 'main protocol' stamp: 'S.V. 6/11/2023 22:18:42'!
resumen
	| resumenString |

	resumenString := String new.
	cuenta transactionsDo: [ :aTransaction | resumenString := resumenString, (aTransaction reportePara: self) ].
	resumenString := resumenString, 'Balance = ', (cuenta balance amount asString), ' pesos'.

	^ resumenString.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ResumenDeCuenta class' category: 'ServiciosFinancieros-Solucion'!
ResumenDeCuenta class
	instanceVariableNames: ''!

!ResumenDeCuenta class methodsFor: 'instance creation' stamp: 'S.V. 6/11/2023 17:04:47'!
para: cuentaAReportar
	^ ResumenDeCuenta new para: cuentaAReportar.! !


!classDefinition: #Transfer category: 'ServiciosFinancieros-Solucion'!
Object subclass: #Transfer
	instanceVariableNames: 'value withdrawLeg depositLeg'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ServiciosFinancieros-Solucion'!

!Transfer methodsFor: 'value' stamp: 'LL 9/20/2020 21:13:00'!
value

	^value! !


!Transfer methodsFor: 'accessing' stamp: 'LL 9/20/2020 22:39:46'!
depositLeg

	^depositLeg! !

!Transfer methodsFor: 'accessing' stamp: 'LL 9/20/2020 21:25:56'!
withdrawLeg
	
	^withdrawLeg! !


!Transfer methodsFor: 'initialization' stamp: 'LL 9/20/2020 21:25:21'!
initializeWith: aValue withdrawLeg: aWithdraw depositLeg: aDeposit

	value := aValue.
	withdrawLeg := aWithdraw.
	depositLeg := aDeposit! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Transfer class' category: 'ServiciosFinancieros-Solucion'!
Transfer class
	instanceVariableNames: ''!

!Transfer class methodsFor: 'errors' stamp: 'LL 9/20/2020 22:57:30'!
cannotTransferToOriginErrorDescription
	
	^'Transfer destionation cannot be the origin account'! !

!Transfer class methodsFor: 'errors' stamp: 'LL 9/20/2020 23:00:53'!
cannotTransferZeroErrorDescription

	^'Transfer amount should be positive'! !

!Transfer class methodsFor: 'errors' stamp: 'LL 9/20/2020 22:56:35'!
signalCannotTransferToOrigin
	
	self error: self cannotTransferToOriginErrorDescription! !

!Transfer class methodsFor: 'errors' stamp: 'LL 9/20/2020 23:06:17'!
signalCannotTransferZero

	self error: self cannotTransferZeroErrorDescription! !


!Transfer class methodsFor: 'assertions' stamp: 'LL 9/20/2020 23:05:22'!
assert: anAmountOfMoney canBeTransferedTo: destinationAccount from: originAccount

	self assertCanTransferTo: destinationAccount from: originAccount.
	self assertCanTransfer: anAmountOfMoney! !

!Transfer class methodsFor: 'assertions' stamp: 'LL 9/20/2020 23:11:50'!
assertCanTransfer: anAmountOfMoney

	^ anAmountOfMoney isZero ifTrue: [ self signalCannotTransferZero ]! !

!Transfer class methodsFor: 'assertions' stamp: 'LL 9/20/2020 23:04:58'!
assertCanTransferTo: destinationAccount from: originAccount

	^ originAccount = destinationAccount ifTrue: [ self signalCannotTransferToOrigin ]! !


!Transfer class methodsFor: 'instance creation' stamp: 'LL 9/20/2020 23:11:34'!
amount: anAmountOfMoney from: originAccount to: destinationAccount 
	
	| transfer depositLeg withdrawLeg |
	
	self assert: anAmountOfMoney canBeTransferedTo: destinationAccount from: originAccount.
	
	transfer := self new.
	
	withdrawLeg := TransferWithdrawLeg of: transfer on: originAccount.
	depositLeg := TransferDepositLeg of: transfer on: destinationAccount.
	
	^transfer initializeWith: anAmountOfMoney withdrawLeg: withdrawLeg depositLeg: depositLeg
	
	
	! !
