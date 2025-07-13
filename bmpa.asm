Assume CS:codigo, DS:datos


;Declaracion de variables 
datos Segment
	Char_image db 'Chas.txt',0			;Reserva un byte en memoria 
	char 		db 0       					;Reserva un byte en memoria 
	bit_offset	dw (?)	  		 			;Reserva una palabara (word) en memoria o 2bytes
			dw 0							;
	color 	db 0							;Reserva un byte en memoria
	fila 		dw (?)						;Reserva una palabara (word) en memoria o 2bytes
			dw 0							;
	columna 	dw (?)						;Reserva una palabara (word) en memoria o 2bytes
			dw 0							;
	new_columna dw (?)						;Reserva una palabara (word) en memoria o 2bytes
		     dw 0							;
	contador  dw 0							;Reserva una palabara (word) en memoria o 2bytes
	archivo   db 'Peri.bmp'					;Reserva un byte en memoria 
	Handle_pixels dw ?						;Reserva una palabara (word) en memoria o 2bytes
	Handle_image_chars dw ?					;Reserva una palabara (word) en memoria o 2bytes
	msg_intro 	db 	'<<<Bienvenido>>>',10,13
			db	'Este programa imprime imagenes a color',10,13 
			db	'de formato bmp de 16 colores en consola y ',10,13
			db 	'convertir los pixeles de la imagen a codigo ascii',10,13
			db 	'Para ver su imagen solo debe de introducir',10,13
			db	'nombre de este programa + nombre del archivo',10,13
			db 	'   	Por ejemplo:',10,13
			db 	'	bmp2asc imagen.bmp$',10,13
			
	no_archivo db 'No se encontro el archivo .bmp$'
	puede_ver db 'Presione cualquier tecla para continuar$'
	ver_pixel_char db 'Ya puede ver la imagen en modo caracteres ver: chas.txt$'
	incorrect_colors db 'La imagen no es de 16 colores$'	
	pixel db 01H dup (?) 					;Reserva un byte en memoria

datos EndS									

codigo Segment 								

;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""PROC"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

write_file proc near			;Procedimiento
	
	push cx						;Guardar cx (contador de bucle)
	push dx						;guarda el valor de dx  -guarda los registros de la pila
	push ax						;guarda el valor de ax  -guarda los registros de la pila
	push bx						;guarda el valor de bx  -guarda los registros de la pila
	
	xor ax, ax					;Limpia el ax
	mov ah, 040h 				;Escribe sobre el archivo 
	mov bx, handle_image_chars	;
	xor cx, cx					;Limpia el cx
	inc cx						;incrementa el cx
	mov dx, offset char			;offset lugar de memoria donde esta la variable char
	int 21h						;Interrupcion al DOS para devolver el control SO
	
	pop bx						;restaura el valor original del bx -saca los registros de la pila
	pop ax						;restaura el valor original del ax -saca los registros de la pila
	pop dx						;restaura el valor original del dx -saca los registros de la pila
	pop cx						;restaura el valor original del cx -saca los registros de la pila
	ret							;para volver del procedimiento al programa principal
endp							;aquí termina el procedimiento

;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""PROC"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

;Guarda los datos que estarian debajo del puntero 
save_data proc near				;

	mov ah, 03fh				;lectura del archivo 
	mov cx, 04h 				;lee 4 bytes del archivo 
	int 21h					;Interrupcion al DOS para devolver el control SO
	ret						;para volver del procedimiento al programa principal
endp							;aquí termina el procedimiento

;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""MACRO"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

;Avanzar_puntero macro se crea una variable "contador" del pixel actual en la cual se le suma 1 para que este avance
;Avanza el puntero en una unidad

avanzar_puntero macro  
	push cx			    	;guarda el valor de cx  -guarda los registros de la pila
	push dx				;guarda el valor de dx  -guarda los registros de la pila
	push ax				;guarda el valor de ax  -guarda los registros de la pila
	push bx				;guarda el valor de bx  -guarda los registros de la pila

	add contador, 1   		;Se le suma a la variable contador un 1 para que avance en el pixel  
	xor ax, ax				;Limpia el ax 
	mov bx, handle_pixels   
	xor cx, cx				;Limpia el cx
	xor al, al				;Mueve el puntero un byte hacia adelante
	mov dx, contador    		;Contador del pixel actual 
	mov ah, 42h         		;Establece un puntero del archivo
	int 21h				;Interrupcion al DOS para devolver el control SO
	
	pop bx				;restaura el valor original del bx -saca los registros de la pila
	pop ax				;restaura el valor original del bx -saca los registros de la pila
	pop dx				;restaura el valor original del bx -saca los registros de la pila
	pop cx				;restaura el valor original del bx -saca los registros de la pila
endm						;Finaliza la definición de un macro

;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""MACRO"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

leer_pixel macro			

	push cx					;guarda el valor de cx  -guarda los registros de la pila
	push dx					;guarda el valor de dx  -guarda los registros de la pila
	push ax					;guarda el valor de ax  -guarda los registros de la pila
	push bx					;guarda el valor de bx  -guarda los registros de la pila
	
	xor ax, ax					;Limpia el ax
	mov ah, 03fh				;
	mov bx, handle_pixels			;
	mov cx, 01h 				;
	mov dx, offset color			;offset lugar de memoria donde esta la variable char
	int 21h					;Interrupcion al DOS para devolver el control SO
	
	pop bx					;restaura el valor original del bx -saca los registros de la pila
	pop ax					;restaura el valor original del bx -saca los registros de la pila
	pop dx					;restaura el valor original del bx -saca los registros de la pila
	pop cx					;restaura el valor original del bx -saca los registros de la pila
endm							;Finaliza la definición de un macro


;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""PROC"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

;Procedimiento que hace es mover el puntero en donde se indique
;Como el al queda 0 el codigo del desplazamiento sera desde el inicio del fichero
;cx es la mitad mas significativa por lo que va a estar en 0's porque el archivo es pequeno

mov_pointer proc near

	xor ax, ax					;Limpia el ax 
	xor cx, cx					;0000 0000 Mitad mas significativa del desplazamiento. 
							;     ffff
	mov ah, 42h				;Posiciona el puntero sobre cuantas columnas de pixels tiene la imagen 
	int 21h					;Interrupcion al DOS para devolver el control SO
	ret						;para volver del procedimiento al programa principal
endp							;aquí termina el procedimiento


;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""PROC"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

GetCommanderLine Proc near		
	
    LongLC    EQU   080h		
	Mov   Bp,Sp				
	Mov   Ax,Es				
	Mov   Ds,Ax					
	Mov   Di,2[Bp]				
	Mov   Ax,4[Bp]			   
	Mov   Es,Ax					
	Xor   Cx,Cx				
	Mov   Cl,Byte Ptr Ds:[LongLC]
	dec		Cl 					 
	
	Mov   Si,2[LongLC]  
	cld               			
	Rep   Movsb 
	Ret   2*2     				

GetCommanderLine EndP

;""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""PROC"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

						;Toma el numero, el codigo del pixel que se agregue al color y lo convierte en un caracter 
						;Toma el color y va preguntando uno por uno los posibles colores que existen en el formato de la imagen que en total serian 16 

print_char_pixel proc near
	
	push cx				;guarda el valor de cx  -guarda los registros de la pila
	push dx				;guarda el valor de dx  -guarda los registros de la pila
	push ax				;guarda el valor de ax  -guarda los registros de la pila
	push bx				;guarda el valor de bx  -guarda los registros de la pila
	
	mov al, char        ;En la variable char se encuentra el color
	cmp ax, 0h 			;Se pregunta si el ax es igual a 0, y salte a "prueba"
						;Pregunta si el color que esta en el ax es 0 "negro"
	mov char, 02eh		;Mueve el char al 02eh que equivale a un caracter en ascii 
	je prueba			;Finalmente salta a la etiqueta prueba si es igual a 0
	
	cmp ax, 01h    			;Cantidad de posibles comparaciones de los colores,  compara ax con 01h
	mov char, 025h 			;Mueve a la variable char un punto
	je prueba				;Salte si esa comparacion era real, en ese ejemplo no va a saltar
		
	cmp ax, 02h         		;Cantidad de posibles comparaciones de los colores,  compara ax con 02h
	mov char, 026h			;Mueve a la variable char un punto
	je prueba	        		;Salte si esa comparacion era real, en ese ejemplo no va a saltar
	
	cmp ax, 03h			;Cantidad de posibles comparaciones de los colores,  compara ax con 03h
	mov char, 025h			;Mueve a la variable char un punto
	je prueba				;Salte si esa comparacion era real, en ese ejemplo no va a saltar
		
	cmp ax, 04h			;Cantidad de posibles comparaciones de los colores,  compara ax con 04h
	mov char, 066h			;Mueve a la variable char un punto
	je prueba				;Salte si esa comparacion era real, en ese ejemplo no va a saltar
	
	cmp ax, 05h 	   	 	;Finalmente, el color que esta en el ax es 5
	mov char, 03ch	  	 	;Mueve a la variable char un punto
	je prueba		   	 	;Salte si esa comparacion era real, en ese ejemplo no va a saltar
	
	cmp ax, 06h			;Cantidad de posibles comparaciones de los colores,  compara ax con 01h	
	mov char, 030H			;Mueve a la variable char un punto
	je prueba				;Salte si esa comparacion era real, en ese ejemplo no va a saltar
	
	cmp ax, 07h			;Cantidad de posibles comparaciones de los colores,  compara ax con 01h
	mov char, 07eh			;Mueve a la variable char un punto
	je prueba				;Salte si esa comparacion era real, en ese ejemplo no va a saltar
	jmp sig					

;Etiqueta con un jump, se utiliza porque la cantidad de comparaciones que realiza son tantas 
;Escribe todo el archivo en caracter 

prueba:
	jmp paint_char		;salta a la etiqueta paint_char	
	
sig:
	cmp ax, 08h			
	mov char, 025h		
	je paint_char		
	
	cmp ax, 09h
	mov char, 023h
	je paint_char
	
	cmp ax, 0ah
	mov char, 057h
	je paint_char
	
	cmp ax, 0bh
	mov char,024h
	je paint_char
	
	cmp ax, 0ch
	mov char, 06ch			
	je paint_char
	
	cmp ax, 0dh
	mov char,05eh
	je paint_char
	
	cmp ax, 0eh
	mov char,038h
	je paint_char
	
	cmp ax, 0fh
	mov char, 040h			
	je paint_char				;salto a la etiqueta paint_char si es igual a 0			
								;Etiqueta que ejecuta la interrupcion 40h (escritura del archivo)
								;Lo que hace es escribir en el archivo los colores con su valor en caracteres 	
paint_char:

	xor ax, ax					;Limpia el ax 
	mov ah, 040h 				;escribe sobre el archivo
	mov bx, handle_image_chars
	xor cx, cx 					;Numero de bytes a escribir /Solo escribo un solo byte
	inc cx		    			;Lo incrementa en una unidad 
	mov dx, offset char  		;Desplazamiento del buffer desde donde se toman los caracteres a escribir
								;En la variable char va a estar el codigo en caracter 
	int 21h						;Interrupcion al DOS para devolver el control SO

	pop bx						;establecer bx en el valor original de ax
	pop ax						;establecer ax en el valor original de bx
	pop dx
	pop cx
	ret							;para volver del procedimiento al programa principal
endp							;aquí termina el procedimiento

notfound:
	mov dx, offset no_archivo		;Mensaje de ayuda para el usuario	
	mov ah, 09h				;
	int 21h					;Interrupcion para imprimir este mensaje
	mov ah, 04ch				;
	int 21h 					;Interrupcion para terminar el programa
	
imprime_help:					;Imprime la ayuda del programa 
							;Solo se llega aqui con en caso que no se introduzcan datos en la linea de comandos 
	mov dx, offset msg_intro		;Mensaje de ayuda para el usuario	
	mov ah, 09h				;
	int 21h					;Interrupcion para imprimir este mensaje
	mov ah, 04ch				;
	int 21h 					;Interrupcion para terminar el programa

error_no_archivo:				;Imprime el error para el usuario 
							;Solo se llega aqui cuando no se encontro la direccion de la image que el usuario digito
	mov dx, offset no_archivo		;Mensaje de error para el usuario	
	mov ah, 09h				;
	int 21h					;Interrupcion para imprimir este mensaje
	mov ah, 04ch				;
	int 21h 					;Interrupcion para terminar el programa

imprime_Icolors:
	mov dx, offset incorrect_colors	;Mensaje de error para el usuario	
	mov ah, 09h				;
	int 21h					;Interrupcion para imprimir este mensaje
	JMP IMPRIME_HELP
;_______________________________________________INICIO DE PROGRAMA_______________________________________________________________________________

inicio:

	Mov Cl,Byte Ptr Ds:[LongLC]	;Mueve al cx el numero de caracteres de la linea de comandos 
	push ds
	mov ax,datos
	mov ds,ax					;mueve la direccion al registro de segmento por medio de AX
	cmp cx, 0
	je open_image
	push ds 
	push ds	
	lea ax, archivo				;direccion del archivo a imprimir
	push ax
	call 	GetCommanderline			;Llamada a la rutina GetCommanderLine
	pop ds
	xor si, si
	xor ax, ax 
	cmp archivo[si],'/'
	jne open_image 			;Sino es la pedida de ayuda quiere decir que la entrada es un numero
	inc si		
	cmp archivo[si],'h'
	je imprime_help	

;-----------------------------------------ABRE ARCHIVO BMP---------------------------------------------------------------------------------
open_image:	
	mov ah, 3dh 			 	;Abrir el archivo existente
	mov dx, offset archivo 	 	;Abre el archivo bmp
	mov al, 0				 	;modo de acceso para abrir archivo, modo de lectura/archivo
	int 21h 				 	;interrupcion al DOS para devolver el control SO
	jc  error_no_archivo
	mov handle_pixels, ax			;

;-------------------------------------LEEE EL NUMERO DE COLORES BMP-----------------------------------------------------------------------------------------

							;Guarda cuantas columnas tiene el archivo 
	mov bx, handle_pixels			;En el bx siempre estaran dos handles de los archivos que se generan
	mov dx, 01Ch				;Desplazamiento del ancho del BMP definido en pixeles  (columnas)
	call mov_pointer				;Call a los procedimientos 


							;leer lo que esta en esa posicion del puntero
	mov bx, handle_pixels			;mueve el puntero
	mov dx, offset columna		;Guarda cuantas columnas tiene el archivo 
	call save_data				;Llamada a la rutina save_data
	mov AX,columna
	mov dx, 04h 
	cmp ax,dx
	jne imprime_Icolors
;------------------------------------ARREGLA EL NUMERO DE COLUMNAS DEL BMP--------------------------------------------------------------------------------

;-------------------------------------LEEE EL NUMERO DE COLUMNAS DEL BMP-----------------------------------------------------------------------------------------

							;Guarda cuantas columnas tiene el archivo 
	mov bx, handle_pixels			;En el bx siempre estaran dos handles de los archivos que se generan
	mov dx, 012h				;Desplazamiento del ancho del BMP definido en pixeles  (columnas)
	call mov_pointer				;Call a los procedimientos 


							;leer lo que esta en esa posicion del puntero
	mov bx, handle_pixels			;mueve el puntero
	mov dx, offset columna		;Guarda cuantas columnas tiene el archivo 
	call save_data				;Llamada a la rutina save_data
	
;------------------------------------ARREGLA EL NUMERO DE COLUMNAS DEL BMP--------------------------------------------------------------------------------

							;Idea principal: Rellenar las columnas, porque las columnas tienen que ser multiplo de 4
							;Se tienen el numero de bits que tiene la columna 
	mov ax, columna 				;mueve al ax lo que es columna
	mov bx, 4					;mueve al bx un 4
	mul bx					;multiplica por el ax
							;se tiene el n�mero de bits que tienen las columnas
							;Encontrar el numero de bytes
	mov bx, 8					;mueve al bx un 8 
	div bx   					;dividir el numero de bits entre 8 (1 byte) - lo divido entre el ax 
	push ax					;Guardar el numero de bytes original del archivo -Hay que fijarse y si ese numero de bytes es multiplo de 4
	mov bx, 4					;mueve al bx un 4
	div bx					;si confirma si es multiplo de 4
	cmp dx, 0					;si el dx es (lo que almacena es el residuo)  0
	je siga					;si es multiplo de 4 salta a la etiqueta siga 	
							;Si en dado caso el dx no es de residuo 0 entra al ciclo 
ciclo:

	pop ax	    				;saca el numero de columnas en bytes original 
	add ax,1					;Se le suma al ax un 1- Avanza columna por columna y se avanza en 1 a 1
	push ax					;Guarda el ax - Ciclo para rellenar (numero de columnas + 1)
	div bx					;Divide nuevamente el ax entre 4 -Ir rellenando si ya es multiplo 
	cmp dx, 0					;Si dx es 0, no salta a ciclo
							;Si dx no es igual a 0, entonces salta a ciclo las veces que sean necesarias hasta que de 0
	jne ciclo					;Salta a la etiqueta ciclo
							;El ciclo vuelve a realizar el mismo procedimiento 
							;Cuando el residuo da 0 quiere decir que el numero de columnas es 4 y ese seria el original;*
	pop ax					;Se guarda el numero real
	mov new_columna, ax  			;Se guarda en la variable new_columna
	
;-------------------------------------LEE EL NUMERO DE FILAS DEL BMP-----------------------------------------------------------------------------

							;Seguir leyendo los pixeles
							;Guardar cuantas filas tiene  
siga:
	
	mov bx, handle_pixels			;En el bx siempre estaran dos handles de los archivos que se generan
	mov dx, 016h          		;Desplazamiento del largo del BMP definido en pixeles  (filas)
	call mov_pointer				;Call a los procedimientos	
	mov bx, handle_pixels			;mueve el puntero
	mov dx, offset fila	  		;Guarda cuantas filas tiene el archivo 
	call save_data				;Llamada a la rutina save_data

;------------------------------------LEE DONDE ES QUE EMPIEZAN LOS PIXELES ---------------------------------------------------------------------------------

							;Guardar el offset de los pixeles 	
	mov bx, handle_pixels			;En el bx siempre estaran dos handles de los archivos que se generan
	mov dx, 0ah				;Muevo el puntero sobre la posición data offset(que es partiendo donde inician mis pixeles)
	call mov_pointer				;Call a los procedimientos
		
	mov bx, handle_pixels           ;
	mov dx, offset bit_offset		;Guarda el offset de los pixeles (donde inician los datos)
	call save_data				;Llamada a la rutina save_data
		
;------------------------------------MUEVE EL PUNTERO DEL ARCHIVO A DONDE EMPIEZAN LOS PIXELES------------------------------------------------------------------------------
	
	mov ax, bit_offset 			;Mueve el offset a un contador 
	mov contador, ax				;Sirve para movernos a traves del archivo en el prcedimiento avanzar_puntero
							;Inicia en los pixeles 
									
;------------------------------------------------------------------------------------------------------------------------------------------------------

	xor ah, ah 				;
	mov al,12h          			;Deja la consola en modo grafico
	int 10h					;Limpia la pantalla.

;-------------------------------------CREA EL ARCHIVO PARA METER LA IMAGEN EN MODO CHAR----------------------------------------------------------------------
	
							;Crea el archivo de caracteres 
	xor ax, ax					;Limpia el ax 
	mov ah, 03ch 				;Crea el archivo de caracteres 
	xor cx,cx                       ;Declaracion de archivo de chars
	mov dx, offset Char_image  	;movemos al registro dx el contenido de char_imagen
	int 21h			  		;Interrupcion al DOS para devolver el control SO
	mov handle_image_chars, ax		;Almacena el handle para manejar el archivo en la avriable "handle_image_chars"
	
;-------------------------------------EMPIEZA A PINTAR LOS PIXELES A CONSOLA--------------------------------------------------------------

	mov dx, fila				;Mueve la fila al dx
	xor ax, ax					;Limpia el ax

otrocolor:

	xor ax, ax					;Limpia el ax
	xor cx, cx					;Limpia el cx
							;Guarda los datos del archivo y los traduce a pixeles a imagenes
linea:

	push ax					;guarda el ax que empiezaen 0
	leer_pixel					;Lee pixel
	avanzar_puntero				;avanza el puntero
							;Ciclo y termina cuando pinto todas las filas y columnas 
							;Imprime la parte alta del valor del pixel y se imprime
	mov ah, 0ch				;
	mov al, color				;mueve el al a la variable color (valor del pixel)
	and al, 0f0h				;
	shr al, 4   				;	
	xor bh, bh					;
	int 10h					;Limpia la pantalla.
	inc cx					;Se incrementa el cx
							;Imprime la parte baja del valor del pixel y se imprime
	mov ah, 0ch				;
	mov al, color				;
	and al, 0fh   				;
	xor bh, bh					;
	int 10h					;Limpia la pantalla.
	inc cx					;Se incrementa el cx
	
	pop ax					;saco el ax
	inc ax 					;Se incrementa el cx y lo comparo con el new_columna
	cmp ax, new_columna			;compara el ax con new_columna (numero de las columnas arregladas, termina siendo la mitad de pixeles)
	jne linea					;salta a la etiqueta linea
	
	dec dx 					;se decrementa el dx
	cmp dx, 0					;se compara el dx con 0
	jne bn_jmp					;salta a la etiqueta bn_jmp
	
	
;-------------------------------Aqui empieza a leer los pixeles impresos en consola------------------------------------------------------------------															;	
;------------------------------para escribirlos en forma de caracteres en un archivo txt-------------------------------------------------------------

	xor dx, dx					;Limpia el registro dx
	xor ax, ax					;Limpia los registros
	xor cx, cx					;Limpia el registro cx(cx me lleva el contador de columnas)
	mov bx, handle_image_chars		;Mueve al bx el handle del archivo txt (nunca cambia)

loop_read_pixel:					;Loop que lee los pixeles de la consola 

	mov ah, 0dh 				;
	int 10h					;obtener el color de un pixel grafico
	xor ah, ah					;
	mov char, al				;Guardo el color en una variable
	call print_char_pixel			;LLamada a la rutina print_char_pixel
	inc cx					;se incrementa el cx en una unidad(cx=columna)
	mov char, 0				;Limpio la variable char 
	cmp cx, columna 				;contador de columnas  /Comparo con columna por el numero original del archivo 
							;Cuando cx igual a columna
	jne loop_read_pixel			;no salta a la etiqueta 
	
;-------------------------------------CUANDO TERMINO UNA FILA DE LA IMAGEN--------------------------------------------------------------
;-------------------------------------HACE UN ENTER DENTRO DEL ARCHIVO TXT------------------------------------------------------------

	mov char, 0Ah				;Hacer un enter en el archivo txt 
	call write_file			 	;LLamada a la rutina write_file
						 	;Para hacer un enter dentro del archivo de texto()
	
;-------------------------------------DEBE EMPEZAR A LEER LA SIGUIENTE FILA DESDE 0-----------------------------------------------------

	inc dx					;numero fila a leer en la imagen - incrementa el dx
	xor cx, cx					;Leer el inicio de las columnas para la sigueiente columna 
	xor ax,ax					;Se limpia el ax, porque quedan los colores
	
	cmp dx, fila				;Compara el dx con fila
	jne BN_Read_pixel			;Si no se ha llegado al final del archivo se devuelve a leer e imprimir la otra linea 

;-------------------------------------AQUI YA SE IMPRIMIO TODA LA IMAGEN EN LA CONSOLA-----------------------------------------------------------------
;-------------------------------------POR LO QUE EL USUARIO YA LA PUEDE VER EN FORMA DE CARACTERES-----------------------------------------------------------------
	
	mov dx, offset puede_ver		;Imprime mensaje cuando la imagen esta lista
	mov ah, 09h				;Indica al usuario que puede continuar presionando cualquier tecla
	int 21h 			
	xor ax, ax					
	mov ah, 01h
	int 21h 					;Genera la entrada de un caracter para que el usuario pueda ver la imagen 				
	mov ax, 3
	int 10h					;Limpia pantalla
	xor dx, dx 
	mov dx, offset ver_pixel_char	;Genera el mensaje para que el usuario ya puede ver la imagen en modo caracteres en un archivo de texto
	mov ah, 09h
	int 21h

;------------------------------------------AQUI TERMINA EL PROGRAMA--------------------------------
salir:
	mov ax,4c00h 				;terminar el programa
	int 21h 	 				;interrupcion al DOS para devolver el control SO

bn_jmp: 
	jmp otrocolor				;salta a la etiqueta otrocolor

bn_read_pixel:
	jmp loop_read_pixel			;salta a la etiqueta loop_read_pixel	
	
codigo EndS					;								 
End inicio						;termina el segmento definido como inicio 							 