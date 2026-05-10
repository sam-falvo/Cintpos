GET "libhdr"

LET f(x) BE {
}

AND start() = VALOF {
	LET a = 2

	SWITCHON a INTO {
	DEFAULT:	f('D'); ENDCASE
	CASE 10:		f('0'); ENDCASE
	CASE 20:		f('1'); ENDCASE
	CASE 30:		f('2'); ENDCASE
	CASE 40:		f('0'); ENDCASE
	CASE 50:		f('4'); ENDCASE
	CASE 60:		f('5'); ENDCASE
	CASE 70:		f('6'); ENDCASE
	CASE 80:		f('7'); ENDCASE
	CASE 90:		f('8'); ENDCASE
	}

	RESULTIS 0
}

