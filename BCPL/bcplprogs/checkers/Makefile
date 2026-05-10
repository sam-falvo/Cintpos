run:
	cintsys64 -c c b checkers
	cintsys64 -- checkers


xcheckers:	checkers.b
	cintsys64 -c c b checkers xref >rawxref
	cintsys64 -c sortxref rawxref to xcheckers
	rm rawxref
