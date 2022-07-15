;====================================================================
; Main.asm file generated by New Project wizard
;
.MODEL	SMALL
.8086
.stack 100h
.code
org 400h
mov ax,0042h
mov ds,ax


; Configuracion del 8255 PP1
     mov dx, DIR_PP1
     mov al, WC1
     out dx, al

; Configuracion del 8255 PP2
    mov dx, DIR_PP2
    mov al, WC2
    out dx, al

    mov dx, PB1
    mov al, 0
    out dx, al
; inicializar variables
start:    CALL TECLADO1
          JMP start

 TECLADO1 PROC NEAR
          MOV CX,5
	  MOV BL,0FEH ;calcula la máscara de fila
 TECLA:   MOV DX, PB1
          MOV AL,BL
          OUT DX,AL
          MOV DX,PC1
          IN  AL,DX
          TEST AL,1
          JZ TEC_DECODIFICADOR1
          TEST AL,2
          JZ TEC_DECODIFICADOR2
CONTINUE: ROL BL,1
	  DEC CX
	  JCXZ FIN
	  JMP TECLA


 TEC_DECODIFICADOR1:
    CALL DECODIFICADOR1_TECLADO1
    JMP CONTINUE
 TEC_DECODIFICADOR2:
    CALL DECODIFICADOR2_TECLADO1
    JMP CONTINUE

 FIN:
     RET
 TECLADO1 ENDP

 DECODIFICADOR1_TECLADO1 PROC NEAR USES BX AX

   XOR BH,BH
   NOT BL
   MOV DX,PA1
   MOV AL,BL
   OUT DX,AL
   MOV DX,PC1
   MOV AL,BH
   OUT DX,AL

 RET
 DECODIFICADOR1_TECLADO1 ENDP


 DECODIFICADOR2_TECLADO1 PROC NEAR USES BX AX

   OR BH,0FFh
   ROL BX,1
   ROL BX,1
   ROL BX,1
   ROL BX,1
   ROL BX,1
   ROL BH,1
   ROL BH,1
   ROL BH,1
   ROL BH,1
   NOT BX
   MOV DX,PA1
   MOV AL,BL
   OUT DX,AL
   MOV DX,PC1
   MOV AL,BH
   OUT DX,AL

 RET
 DECODIFICADOR2_TECLADO1 ENDP
end1:
	jmp   end1



.data ;Data segment - Segmento de datos
;***************************************************
;		Configuración del 8255A
;***************************************************
DIR_PP1   EQU 0FE06H
WC1       EQU 81H
PC1       EQU 0FE04H
PB1       EQU 0FE02H
PA1       EQU 0FE00H
DIR_PP2   EQU 0FF06H
WC2       EQU 89H
PC2       EQU 0FF04H
PB2       EQU 0FF02H
PA2       EQU 0FF00H
LEDS      dW   1,2,4,6,8,16,32,64,128,256,400h,800h
END
