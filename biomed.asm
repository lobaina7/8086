;***************************************************
;		Referencias externas si son necesarias
;***************************************************

;EXTRN		_main 	   : NEAR

;***************************************************
;		Code segment - Segmento de código
;***************************************************

_TEXT SEGMENT BYTE PUBLIC 'CODE'

	ASSUME cs:_TEXT, ds:_DATA

Start label	near

;*************************************************
; 	Relocalización del segmento de datos para
;	archivos .COM, .BIN o .EXE
;*************************************************
		mov		ax, 0000h
		mov		ds, ax

;*************************************************
; 	Preparar  todo para inicializar tabla de
;	vectores de Interrupción
;*************************************************
		xor		ax, ax
		push	ds
		mov		ds, ax


;*************************************************
; 	Inicializar tabla de vectores de Interrupción
;	si es necesario.
;*************************************************

	  MOV SI,08H
      MOV DX,OFFSET STOP
      MOV [SI],DX
      MOV[SI + 2],AX

;	MOV bx,0180H
;	MOV DX,OFFSET STOPMED
;	MOV [bx],DX
;	MOV[bx + 2],AX

;*************************************************
; 	Recuperar segmento de datos
;*************************************************

		pop		ax
		mov		ds, ax

     MOV DI,05000H;
     MOV ES,DI
     xor di,di
     MOV MEM,DI
	JMP COMENZAR
org 400h

COMENZAR:
XOR DX,DX
XOR AX,AX
XOR BX,BX
XOR DI,DI
XOR CX,CX
XOR SI,SI
;********************************************************
;  PROGRAMACION DEL 8255A CONEXION CON EL ADC0808
;********************************************************
;PA ENTRADA(DATOS DE LA CONVERSION), PB ENTRADA(EOC), PC SALIDA (STAR Y ALE)
;COMANDO DE INICIALIZACION
MOV AL,10011010B
MOV DX,CPPIA
OUT DX,AL

;********************************************************
;  PROGRAMACION DEL 8255B CONEXION CON LA VISUALIZACION MULTIPLEXADA
;********************************************************
;PA SALIDA, PB SALIDA
;COMANDO DE INICIALIZACION
MOV AL,10000000B
MOV DX,CPPIB
OUT DX,AL


CALL SEGMENT7


;*************************************************
;  PROGRAMACION DEL 8259
;*************************************************
;MOV AL,ICW1
;OUT P59,AL
;MOV AL,ICW2
;OUT P59W,AL

;MOV AL,ICW4
;OUT P59W,AL
;MOV AL,OCW1
;OUT P59W,AL
;MOV AL,OCW
;OUT P59W,AL
;********************************************************
;  CODIGO PARA MOSTRAR LA IMPEDANCIA A LA FRECUENCIA DE 50KHZ
;********************************************************

;********************************************************
;ENCUESTA AL ACCIONAMIENTO DE LOS BOTONES DE SELECCION PARA 50KHZ O ESPECTROSCOPIA
;********************************************************


MOV DX,CS374
MOV AL,04H
OUT DX,AL

MOV DX,BUTTON

TES: IN AL,DX ; QUE BOTON ESTA ACTIVADO
     TEST AL,01H ;SI ESPECTROSCOPIA SOLAMENTE
	 JZ ESPECTRO;
	 TEST AL,02H ;SI 50KHZ SOLAMENTE
	 JZ FREQ
	 JMP TES
;A SELECCIONADO LA FRECUENCIA DE 50KHZ SOLAMENTE

FREQ:


; MOV DX,BUTTON

TESTAR: IN AL,DX ; QUE BOTON ESTA ACTIVADO
     TEST AL,04H ;SI APRIETA STAR
	 JZ ESPP;
	JMP TESTAR
ESPP:
;SELECCIONA LA SEÑAL NO 2 DEL MULTIPLEXOR ANALOGICO

MOV DX,CS374
MOV AL,01H
OUT DX,AL

MOV CX,30
ES11:
CALL ESPERA
LOOP ES11


ADD MEM,80H
MOV DI,MEM
CALL FREQ50KHZ

MOV DI,4
CALL DECHEX

MOV DX,CS374
MOV AL,024H
OUT DX,AL

MOV DI,OFFSET IMP
ADD  DI,4

ENDF:
CALL DISPLEY
JMP ENDF


ESPECTRO:
;********************************************************
;SE LE DA SALIDA AL MENSAJE DE QUE SE HA APRETADOE STE BOTON
;********************************************************
MOV DX,BUTTON

TESTAR1: IN AL,DX ; QUE BOTON ESTA ACTIVADO
     TEST AL,04H ;SI APRIETA STAR
	 JZ ESPP2;
	JMP TESTAR1
ESPP2:

MOV DX,CS374
MOV AL,00H
OUT DX,AL

MOV CX,030
ESPP1:
CALL ESPERA
LOOP ESPP1

ADD MEM,80H
MOV DI,MEM
;DIRECCIONAR LAS LECTURA DEL ADC EN LA MEMORIA RAM
CALL FREQ50KHZ


;GUARDAR LA IMPEDANCIA PARA LA FRECUENCIA DE 5KHZ
XOR DI,DI
CALL DECHEX

MOV DX,CS374
MOV AL,11H
OUT DX,AL
MOV CX,30
PUSH DI
ESPP12:
CALL ESPERA
LOOP ESPP12

ADD MEM,80H
MOV DI,MEM
;DIRECCIONAR LAS LECTURA DEL ADC EN LA MEMORIA RAM
CALL FREQ50KHZ
;GUARDAR LA IMPEDANCIA PARA LA FRECUENCIA DE 50KHZ
POP DI
CALL DECHEX
PUSH DI

;SELECCIONA LA SEÑAL NO 3 DEL MULTIPLEXOR ANALOGICO
MOV DX,CS374
MOV AL,022H
OUT DX,AL

MOV CX,30
ESPP13:
CALL ESPERA
LOOP ESPP13

ADD MEM,80H
MOV DI,MEM
;DIRECCIONAR LAS LECTURA DEL ADC EN LA MEMORIA RAM
CALL FREQ50KHZ

;GUARDAR LA IMPEDANCIA PARA LA FRECUENCIA DE 200KHZ
POP DI
CALL DECHEX

MOV DX,CS374
MOV AL,044H
OUT DX,AL

CALL VISUALIZAR


 J1	 label	near

		jmp 	J1


 ;********************************************************
;  PARAR LA MEDICION O FINALIZAR
;********************************************************
 STOP PROC NEAR
    XOR AX,AX
    MOV AH,08H
	MOV CX,4
     DISE: JCXZ SGT
       MOV DX,PBB
       MOV AL,AH
	   OUT DX,AL

	   MOV DX,PAB
	   PUSH AX
	   MOV AL,0FFH
       OUT DX,AL
	   POP AX
	   ROR AH,1
	   DEC CX
	   JCXZ SGT
	   JMP DISE

 SGT:

JMP COMENZAR
STOP endp

FULL PROC NEAR
   XOR AX,AX
    MOV AH,08H
	MOV CX,4
     DISS: JCXZ SGTS
       MOV DX,PBB
       MOV AL,AH
	   OUT DX,AL

	   MOV DX,PAB
	   PUSH AX
	   MOV AL,080H
       OUT DX,AL
	   POP AX
	   ROR AH,1
	   DEC CX
	   JCXZ SGTS
	   JMP DISS

 SGTS:

JMP COMENZAR
FULL ENDP
;********************************************************
;  SUBRUTINAS
;********************************************************
VISUALIZAR PROC NEAR

MOV DX,BUTTON
MOV DI,OFFSET IMP
ADD DI,0Ch



TESS: IN AL,DX ; QUE BOTON ESTA ACTIVADO
     TEST AL,08H ;SI 5KHZ SOLAMENTE
	 JZ V1;
	 TEST AL,010H ;SI 50KHZ SOLAMENTE
	 JZ V2
	 TEST AL,020H ;SI 50KHZ SOLAMENTE
	 JZ V3
	 JMP TESS

V1:
PUSH DX
MOV DX,CS374
MOV AL,014H
OUT DX,AL
POP DX

PUSH DI
SUB DI,0CH
MOV CX,30H
END43:
CALL DISPLEY
LOOP  END43
CALL ESPERA
POP DI
JMP TESS

V2:
PUSH DX
MOV DX,CS374
MOV AL,024H
OUT DX,AL
POP DX



PUSH DI
SUB DI,08H
MOV CX,30H
ENDF41:
CALL DISPLEY
LOOP  ENDF41
CALL ESPERA
POP DI
JMP TESS

V3:

PUSH DX
MOV DX,CS374
MOV AL,044H
OUT DX,AL
POP DX

PUSH DI
SUB DI,04H
MOV CX,30H
ENDF42:
CALL DISPLEY
LOOP  ENDF42
CALL ESPERA
POP DI
JMP TESS

RET
VISUALIZAR ENDP



;********************************************************
;OBTENCION DE LA IMPEDANCIA CON 50KHZ
;********************************************************
FREQ50KHZ  PROC  NEAR

MOV CX,080H


CICLO:
nop
nop
nop
nop
nop

PUSH CX
CALL ADCX
MOV ES:[DI],AL
INC DI
POP CX
LOOP CICLO
SUB DI,80H
CALL PROMED


RET

FREQ50KHZ ENDP
;*********************************************************************************
; LECTURA DE LOS DATOS QUE  RESULTARON DE LA CONVERSION CON EL ADC0808
;***********************************************************************************

ADCX PROC NEAR
;MANDAR A CONVERTIR
;PC EN O
XOR AX,AX
MOV DX,PCA
OUT DX,AL

;INICIO DE CONVERSION(START)
XOR AX,AX
MOV AL,02H
OUT DX,AL
nop
nop
nop
nop

MOV AL,06H
OUT DX,AL
 MOV CX,10H
dotime: loop dotime

MOV AL,04H
OUT DX,AL
nop
nop
nop

XOR AL,AL
OUT DX,AL
;ESPERA FIN DE CONVERSION(EOC)
  MOV DX,PBA
  WAITREAD:
   IN AL,DX
   TEST AL,01H
   JZ WAITREAD
 ;LEER CONVERSION

MOV DX,PAA
XOR AX,AX
IN AL,DX

XOR DX,DX
RET
ADCX ENDP
;*********************************************************************************
; INCREMENTAR LOS BIT DE CONTROL DEL MULTIPLEXOR PARA LAS DEMAS FRECUENCIAS
;***********************************************************************************

;********************************************************
;PROMEDIO DE LAS MEDICIONES DE CADA FRECUENCIA
;********************************************************

PROMED PROC NEAR
PUSH BX
XOR BX,BX
MOV CX,080H
XOR AX,AX

PROM: JCXZ DIVI
       PUSH CX
       MOV AL,ES:[DI]

      ADD BX,AX
	   POP CX
       DEC CX
	   INC DI
       JMP PROM

DIVI:
       XOR DX,DX
       MOV CX, 080H
       MOV AX,BX
       DIV CX
	  POP BX
    ; TRANSFORMAR EL VALOR EN IMPEDANCIA
	   XOR DX,DX
	  CALL IMPEDANCIA
       RET
        PROMED ENDP


;********************************************************
;OBTENCION DE Z=V/I (RMS)
;********************************************************

IMPEDANCIA  PROC NEAR
XOR BX,BX

MOV CX, 086A0H
MOV BX,AX
MUL CX
ADD DX,BX

MOV  BX, 04243H
DIV BX

RET
IMPEDANCIA ENDP

;********************************************************
;CONVERSION DE HEXADECIMAL A DECIMAL
;********************************************************
DECHEX PROC NEAR
push si
xor cx,cx
MOV BX,10

DIV1: XOR DX,DX
      DIV BX
      PUSH DX
	  inc cx
	  OR AX,AX
	  JNZ DIV1

 mov si,4
 sub si,cx

 PUSH DI
 PUSH CX
  MOV CX,SI
 LLE: JCXZ NEW
 MOV BX,0AH
 MOV IMP[DI],BL
 INC DI
 DEC CX
 JMP LLE


NEW:
 POP CX
  POP DI
 add di,si
   ETW:
   POP DX
   MOV IMP[DI],DL
   INC DI
   LOOP ETW
  pop si
FINAL: RET
     DECHEX ENDP

;********************************************************
;VISUALIZACION EN LOS 7SEGMENTOS
;********************************************************
ESPERA PROC NEAR
     PUSH AX
     PUSH BX
     PUSH DX
	 PUSH CX

MOV CX,4
XOR AX,AX
MOV AH,08H

 DISW: JCXZ SGTEW
       MOV DX,PBB
       MOV AL,AH
	   OUT DX,AL

	   MOV DX,PAB
	   MOV BX,AX
	   MOV AL,07FH
       OUT DX,AL
	   MOV AX,BX
	   ROR AH,1
	   PUSH CX
	   MOV CX,2000h
	   TIMEW:LOOP TIMEW

       POP CX
	   DEC CX
	   JCXZ SGTEW

	   JMP DISW

 SGTEW:POP CX
	   POP DX
	   POP BX
	   POP AX
	   RET
 ESPERA ENDP


DISPLEY PROC NEAR

     PUSH AX
     PUSH BX
     PUSH DX
	 PUSH SI
	 PUSH CX
;INICIALIZAR EL REGISTRO DE EXHIBICION
	MOV CX,4
	ADD DI,0003H
	MOV SI,OFFSET SEGM

	XOR AX,AX
	XOR BX,BX
    MOV AH,08H
	;MOV BX,OFFSET T7SEG
; EXHIBICION

  DIS: JCXZ SGTEI
       MOV DX,PBB
       MOV AL,AH
	   OUT DX,AL

	   MOV DX,PAB
	   PUSH AX
	   MOV AL,[DI]
	   MOV BL,AL
	   MOV AL,[BX + SI]
       OUT DX,AL
	   POP AX
	   ROR AH,1
	   PUSH CX
	   MOV CX,1000
	   TIME:LOOP TIME

       POP CX
	   DEC CX
	   JCXZ SGTEI
	   DEC DI
	   JMP DIS

 SGTEI:POP CX
	   POP SI
	   POP DX
	   POP BX
	   POP AX
	   RET
  DISPLEY ENDP



SEGMENT7 PROC NEAR

MOV DI,OFFSET SEGM

MOV AL,0C0H
MOV [DI],AL
INC DI
MOV AL,0F9H
MOV [DI],AL
INC DI
MOV AL,0A4H
MOV [DI],AL
INC DI
MOV AL,0B0H
MOV [DI],AL
INC DI
MOV AL,099H
MOV [DI],AL
INC DI
MOV AL,092H
MOV [DI],AL
INC DI
MOV AL,082H
MOV [DI],AL
INC DI
MOV AL,0F8H
MOV [DI],AL
INC DI
MOV AL,080H
MOV [DI],AL
INC DI
MOV AL,090H
MOV [DI],AL
INC DI
MOV AL,0FFH
MOV [DI],AL
RET
SEGMENT7 ENDP


;*************************************************
; 		Código usuario termina aquí
;*************************************************

_TEXT ENDS

;***************************************************
;		Data segment - Segmento de datos
;***************************************************

_DATA SEGMENT WORD PUBLIC 'DATA'
	public	__acrtused		; trick to force in startup
	__acrtused = 9876h

	; Aquí dentro declaramos las variables necesarç

CPPIA  EQU 86H
PBA      EQU 82H
PCA      EQU 84H
PAA      EQU 80H
CPPIB  EQU 0A6H
PBB      EQU 0A2H
PCB     EQU 0A4H
PAB      EQU 0A0H
BUTTON     EQU 0D0H
Z   DW 0
CS374 EQU 090H

ICW1      EQU 013H
ICW2     EQU 60H
ICW4      EQU 03H
OCW1       EQU 0FFH
OCW       EQU 0FEH
P59W    EQU  0E2H
P59   EQU  0E0H
MEM  DW 0000H
IMP DB 12 DUP(0)
SEGM DB 11 DUP(0)



;*************************************************
; 		Segmento de pila (no declarar tamaño)
;*************************************************

;*************************************************
; 		Segmento de pila (no declarar tamaño)
;*************************************************
_DATA ENDS

END
