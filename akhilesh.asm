%OUT-------------------------------------------------------------------------
%OUT TITLE: DrawTurtle function for LOGO implementation assignment
%OUT Author : Akhilesh Godi (CS10B037) - Project - CS2610
%OUT NOTES/TODO: 
%OUT 1. Use masm/tasm to compile
%OUT 2. Checks for the pixel row/column going out of range
%OUT 3. Commands - 
%OUT			   fd or FD or Fd or fD: Forward
%OUT 			   bk or BK or Bk or bK: Back
%OUT 			   rt or RT or Rt or rT: Right
%OUT 			   lt or LT or Lt or lT: Left
%OUT			   rp followed by the number of times to repeat followed by a series of instructions on the same line
%OUT			   bye to quit
%OUT-------------------------------------------------------------------------

	.model small 
	.stack 256h
	
	
CR equ 13d 		;Carriage Return ASCII value
LF equ 10d		;Next Line ASCII Value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;											DATA SEGMENT														   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.data 
	
	turtle_row	 dw 	100             ; Rows: 0-199 so one byte suffices
	turtle_col   dw 	160     		; Cols: 0-319 so need a word
	pixel_row    dw	    100         	; Position of Present Pixel Row
	pixel_col    dw     160 			; Poosition of Present Pixel Column
	bkflag       db     0				; Flag to check if BK instruction is called
	length1		 dw		2 dup (0)		; Length to be moved by the cursor
	angle 		 dw     0				; Stores the angle at which the cursor is tilted w.r.t the horizontal
	sign 		 db		0				; Checks for magnitude
	num 		 dw		2 dup (0)		; Number read by Evaluate Number Procedure
	msg1 		 db 	"Enter the command. Type bye to terminate$"
	string  	 db     80 dup('$')		; Stores the string here
	repeatstring db     80 dup('$')		; In case the repeat flag is set, the string is stored here
	errormsg     db     "Out of range. Bye. $"
	repeattime   dw     1				; No. of times to repeat the instruction. You cannot have nested repeats
	repeatflag   db     0				; Checks if repeat is called and sets it to 1 if repeat is called. After done set back to zero.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;											CODE SEGMENT														   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
.code 

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    DrawPixel macro color
	       
	        push ax
	        push cx
	        push dx
	        
	        mov al,color				;al has the pixel color
	        mov dx,pixel_row			;dx - Row Number
	        mov cx,pixel_col			;cx - Column number
	        
	        mov ah,0ch				
	        int 10h     				;Set the Pixel
	        
	        pop dx
	        pop cx
	        pop ax
    ENDM
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 		
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ClearScreen macro 
	
	 	LOCAL CLRSCR
	     
	       push ax
   	       push bx
 	       push cx
 	       push dx
 	      
 	      mov ax,0000b				   	
 	      ;Setting the co-ordinates of the bottom of the screen		 
 	      mov dx,200					;dx : Row Nuber
 	      mov cx,320					;cx : Column Number
	
		clrscr: 
    	   mov al, 0000b
    	   mov ah, 0ch
		   int 10h     					;Set pixel. 
    	   dec cx
    	   jnz clrscr
    	   dec dx
    	   mov cx,320
    	   cmp dx,190	
    	   jge clrscr
    	   
    	   
        	mov dx, 198					;Row Number 
			mov cx, 0					;Column Number 
			mov bh, 0					;Page number 0
			mov ah, 2					;Position my cursor
			int 10h						;Gooo!!!
		
		mov dl, '?'
		mov ah, 02h
		int 21h
		
		mov dl, ':'
		mov ah, 02h
		int 21h
		   
		 pop dx
    	 pop cx
    	 pop bx
    	 pop ax
    	
    ENDM
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 		DrawTurtle macro colour
    		
    		LOCAL ROW1, ROW2
   	      push ax
          push bx
          push cx
          push dx
          
          ; Head pixel 
          mov al,colour         
          mov dx,turtle_row
          mov cx,turtle_col
          mov ah,0ch
          int 10h        
        
	      ; Row 1 : 3 pixels
          inc dx
          dec cx
          mov bx,3

	  ROW1:
 	      int 10h    
          inc cx
          dec bx
          jnz ROW1
         
          mov bx,5
   	      inc dx
	
	  ROW2: 
         int 10h
         dec cx
         dec bx
         jnz ROW2
        
        pop dx
        pop cx
        pop bx
        pop ax
        
        ENDM
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
START:
	
		
		mov ax, @data					;Stores the address of data segment in the accumulator register sx
		mov ds, ax						;Copies the value in the accumulator to the ds register
		
        mov al, 13h
        mov ah, 0
        int 10h                         ;set graphics video mode. 
        
        mov turtle_row,100
        mov turtle_col,160
        ;DrawTurtle 1110b               ;This shows
       
		lea dx, msg1					;loads effective address of msg1 into dx
		mov ah, 09h						;In C: printf. Prints msg1
		int 21h							;Interrupt call
		
		
        mov dx, 197						;Row Number 
		mov cx, 0						;Column Number 
		mov bh, 0						;Page number 0
		mov ah, 2						;Position my cursor
		int 10h							;Gooo!!!
      
        ;Prints ?: on the screen to prompt user for input 	
		mov dl, '?'
		mov ah, 02h
		int 21h
		
		mov dl, ':'
		mov ah, 02h
		int 21h
		
		call gets		
		call StringRecognizer
		
		;Go back to the normal mode!
		mov al,03h
		mov ah, 0
		int 10h
		
        mov ax,4c00h                    ; Return control back to the OS
        int 21h
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
;;;;																												 ;;;;
;;;;						 				PROCEDURES ARE LISTED BELOW											     ;;;;
;;;;																												 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
 		StringRecognizer PROC
 		
 			;DrawTurtle 0000b
 			push ax
 			push bx
 			push cx
 			push dx
 	
 		    cmp repeatflag,0			;Check if repeat flag is off
 		    jne rpstring				;If not do what repeat command is supposed to do
 		    
 			mov ax, offset string		;moves the effective address of the string that was taken into ax
			mov bx, ax					;bx now has the address
			mov al, byte ptr [bx]		;moves the address of the first byte of string to al
			jmp below
	
	rpstring:
			mov ax, offset repeatstring
			mov bx,ax
			mov al, byte ptr[bx]
			
	below:	
			;Check for FD. If F is there check next byte for D
 			cmp al,'F'				;computes al-'F'
			je F					;if al=='$' then goto done
			cmp al,'f'				;computes al-'f'
			je F					;if al=='$' then goto done
			
			;Check for BK. Checks for a B here
			cmp al,'B'				;computes al - 'B'
			je B					;
			cmp al,'b'				;computes al - 'b'
			je B					;
			
			;Checks for RT or RP. Checks for R here
			cmp al,'R'				;computes al - 'R'
			je R					;
			cmp al,'r'				;computes al - 'r'
			je R					;
			
			;Checkd
			cmp al,'L'				;computes al - 'L'
			je L					;
			cmp al,'l'				;computes al - 'l'
			je L					;
			
			jmp done
			
		F:
			inc bx					;next byte in memory so increment
			mov al, byte ptr[bx]	;move the value of the memory location of bx to al
			
			;If found F. Check next byte for a 'd' or 'D'
			cmp al,'D'
			je FD
			cmp al,'d'
			je FD
			jmp done
			
			FD: 
			
				inc bx					;next byte in memory so increment
				mov al, byte ptr[bx]	;move the value of the memory location of bx to al
			
				cmp al,'$' 
				je don
				
				call FORWARD			;Calls FORWARD instruction and reads the length by which to move
				
				cmp al,'$' 
				je don
				
				inc bx					;next byte in memory so increment
				mov al, byte ptr[bx]	;move the value of the memory location of bx to al
				jmp below
		
		B:
			inc bx					;next byte in memory so increment
			mov al, byte ptr[bx]	;move the value of the memory location of bx to al
			cmp al,'K'
			je BK
			cmp al,'k'
			je BK
			cmp al, 'Y'
			je BY
			cmp al, 'y'
			je BY
			jmp done
			
			BK:	
				inc bx					;next byte in memory so increment
				mov al, byte ptr[bx]	;move the value of the memory location of bx to al
				
				cmp al,'$' 
				je don
				
				call BACK
				
				cmp al,'$' 
				je don
				inc bx
				mov al, byte ptr[bx]
				jmp below
				
			BY:
				inc bx					;next byte in memory so increment
				mov al, byte ptr[bx]	;move value of the memory location of bx to al
				cmp al, 'E'
				je exit
				cmp al, 'e'
				je exit
				
	don: jmp done						;don label for jumping down to prevent going out of bound
		
			exit : 
				pop dx
				pop cx
				pop bx
				pop ax
				
				ret
				
	R: jmp R1							; Flag used as it is going out of bound when we call R1 directly from top			
		
		L:
			inc bx					;next byte in memory so increment
			mov al, byte ptr[bx]	;move the value of the memory location of bx to al
			cmp al,'T'
			je LFT
			cmp al,'t'
			je LFT
			jmp done
			
			LFT: 
				inc bx					;next byte in memory so increment
				mov al, byte ptr[bx]	;move the value of the memory location of bx to al
				cmp al,'$' 
				je done
				call LEFT
				cmp al,'$' 
				je done
				inc bx
				mov al, byte ptr[bx]
				jmp below
				
		R1:
			inc bx					;next byte in memory so increment
			mov al, byte ptr[bx]	;move the value of the memory location of bx to al
			cmp al,'T'
			je RGT
			cmp al,'t'
			je RGT
			cmp al,'p'
			je RP
			cmp al,'P'
			je RP
			jmp done
			
			RGT: 
				inc bx					;next byte in memory so increment
				mov al, byte ptr[bx]
				cmp al,'$' 
				je done
				call RIGHT
				cmp al,'$' 
				je done
				inc bx
				mov al, byte ptr[bx]
				jmp below
				
		   RP:
		        inc bx
		        mov al, byte ptr[bx]
		        call EvaluateNumber
		        mov cx,num
		    	mov repeattime,cx
		     		
		        call CopyString
		       	mov repeatflag,1
		       	
		   loopback:
		       	cmp repeattime, 0
		        je beforedone    
		        Call StringRecognizer
		        dec repeattime
		        jmp loopback
		        
		    beforedone: mov repeatflag,0
				
			done :
			    pop dx
				pop cx
				pop bx
				pop ax
			
			ClearScreen
			jmp eval									;Checks for Out of range
			
	exit1:	cmp repeatflag,1
			je exits 
			call gets
			call StringRecognizer
	exits:	ret
			
		eval:
			cmp turtle_row,0
            jl pack
            cmp turtle_row,199
            jg pack
            cmp turtle_col,0
            jl pack
            cmp turtle_col,319
            jg pack
            jmp exit1
    
       pack:
            mov dx,turtle_row
		    lea dx, errormsg
		    mov ah,09h
		    int 21h
		    
		    mov ah,01h
			int 21h
	        ret 

			
		StringRecognizer ENDP
		
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    CopyString PROC
	    	
	    	push cx
	    	push dx
	    	mov si, offset repeatstring		;moves the effective address of the string to si
			
	      loopcpy:	
	    	inc bx
	    	mov al, byte ptr [bx]			;moves the address of the first byte of string to al
			cmp al, '$'
			je copyover
	    	mov byte ptr [si], al			
			inc si
			jmp loopcpy
			
		copyover:
		 	mov byte ptr [si], '$'
		 	
		 	pop dx
		 	pop cx
			ret
			
	    CopyString ENDP
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	;Sub routine for getc used in gets equivalent	
	
         getc PROC
	
			push bx
			push cx
			push dx
			
			mov ah, 1h
			int 21h
			
			pop dx
			pop cx
			pop bx
			
			ret
	    getc ENDP
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
		
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Start of gets Procedure
		gets PROC	
			
			mov ax, offset string			;Moves the effective address of the string to be taken into the accumulator		
			
			
			push ax							;Pushes the present values into the stack
			push bx							;Every push is accompanied by a pop at the end
			push cx					
			push dx
		
			mov bx,ax						;bx now holds the address to the first location of the string
			call getc						;Reads the first character in the input stream 
											;getc subroutine is below our main code and copies the value from input into "al" register
			mov byte ptr [bx], al			;copies the value of accumulator al to the byte pointer bx which stores the location in 										;the string
		
			get_loop:
				cmp al,CR					;compare al, CR (Computes al - CR)
				je get_fin					;if zero then jump to get_fin
				
				inc bx						;moves bx pointer to next position so that number is stored
				call getc					;reads a character from input
				mov byte ptr [bx], al		;moves al register into bx
				jmp get_loop				;loops it until carriage return
			
			get_fin:
				mov byte ptr [bx], '$'		;stores sentinel character at the end of the string
						
			pop dx							;Pops the values that were pushed into the stack
			pop cx
			pop bx
			pop ax
			ret
			
		gets ENDP
		;End of gets equivalent subroutine
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 
	 EvaluateNumber PROC
	      
	     push dx
	     push ax
	     push cx
	     mov sign, 0
	     dec bx
	     spaces:
	     	inc bx					;next byte in memory so increment
			mov al, byte ptr[bx]	;move the value of the memory location of bx to al
   	      	cmp al,' '				
   	      	je spaces				
			cmp al, '-'
			je changesign
			mov num,0
		 	jmp sumloop
		 	
		 changesign:
			cmp sign,0
			je eql
			mov sign,0
			jmp neq
	   eql: mov sign,1	
	   neq:	mov num, 0
		  	inc bx					;next byte in memory so increment
			mov al, byte ptr[bx]	;move the value of the memory location of bx to al
			jmp sumloop
	
 	      sumloop:
			cmp al,'$'				;computes al-'$'
			je done2					;if al=='$' then goto done
			cmp al,' '
			je done2
			cmp al, 57				;computes al - '9'
			jg done2					;if it is greater than zero jump to invalid
			cmp al, 48				;computes al - '0'
			jl done2					;if al is less than zero then jump to invalid
			mov ah,00
			sub al,48
			mov cl,al
			mov ax, 10
			mul num
			mov num,ax
			mov al,cl
			mov ah, 0
			add num,ax				;the loop continues if al<='9' and al>= '0'
			inc bx					;next byte in memory so increment
			mov al, byte ptr[bx]	;move the value of the memory location of bx to al
			jmp sumloop				;keeps looping until $ is seen
		    
        done2:
        pop cx
        pop ax
        pop dx
        ret
        EvaluateNumber ENDP
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
            
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	;Comes here on seeing an FD
 	FORWARD PROC
 		  
 		  push cx
   	      push dx
   	      
   	      call EvaluateNumber					; Evaluates the number after FD. Result in num.
   	      cmp num,0
   	      je finisflag
		  cmp bkflag,1							; CHeck if BK was called. If yes, if angle is zero. move by 180
   	      je doone
   	      jmp dotwo
   	      
   	      doone : mov sign,1
   	      dotwo:
   	      mov dx, num
   		  mov length1, dx
 
   		  mov cx,length1
 		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  		  
 		  
 		  	cmp angle,0
 		  	je Zero
 		  
 		  	cmp angle,45
    	  	je FortyFive
    	  
 		  	cmp angle,90
 		  	je Ninety
 		  
		  	jmp dwn
 	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	   
 	   Zero:
 	   		;DrawTurtle 0000b
 			call MoveAngZ
   	   		jmp finis
   	   
   	   FortyFive:
 			;DrawTurtle 0000b
 			call MoveAngFF
 			jmp finis
 	
 		Ninety:	
 			;DrawTurtle 0000b
 			call MoveAngNin
 finisflag:
 					jmp finis
 		
 		
 		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 		
 			dwn:
 				cmp angle,135
				je OTF
 		
 				cmp angle,180
 				je OE
 		
 				cmp angle,225
 				je TTF
 				jmp dwn2
 		
 		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 		OTF:
 			;DrawTurtle 0000b
 			call MoveAngOTF
 			jmp finis
 		OE:
 			;DrawTurtle 0000b
 			cmp sign,1
 			je downd
 			mov sign,1
 			jmp downd2
 			downd:
 			mov sign,0
 			
			downd2:
				call MoveAngZ
 				jmp finis
 		
 		
 		TTF:
 			cmp sign,1
 			je downd3
 			mov sign,1
 			jmp downd4
 			downd3:
 			mov sign,0	
			downd4:
				call MoveAngFF
 				jmp finis
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			
 			
 			dwn2:
 				 cmp angle,270
 				 je TS
 				 
 				 cmp angle,315
 				 je TOF
 		 
 				 cmp angle,360
 				 call MoveAngZ
 				 jmp finis
 				 
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;	 
 		
 		 TS:
 		 	cmp sign,1
 			je downd5
 			mov sign,1
 			jmp downd6
 			downd5:
 			mov sign,0
 			
			downd6:
				call MoveAngNin
 				jmp finis

 		 	
 		 TOF:
 		 	cmp sign,1
 			je downd7
 			mov sign,1
 			jmp downd8
 			downd7:
 			mov sign,0
 			
			downd8:
				call MoveAngOTF
 				jmp finis
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 		
 		finis:	
 			pop dx
 			pop cx
 			mov bkflag,0
 			mov sign,0	
 			ret
 			
 			
 	FORWARD ENDP
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	BACK PROC
 		
 		mov bkflag,1
 		call FORWARD
 		mov bkflag,0
 		ret
 			
 	BACK ENDP
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	RIGHT PROC
 	
 		push cx
 		push dx
 		
 		call EvaluateNumber
 		mov cx,0
 		mov cx,num
 		cmp sign,1
 		je another1
 		
 		add angle,cx
 	
 	;If angle is greater than 360. Find remainder on dividing by 360	
 	subl:
 		cmp angle,0
 		jl another1
 		cmp angle,360
 		jle onel
 		sub angle,360
 		jmp subl	
 	
 	onel:
 		mov cx, angle 
flagged:pop dx
 		pop cx	
 		
 		ret

 		
 	another1:
 		sub angle,cx 
 		another:
 			cmp angle,360
 			jg subl
 			cmp angle,0
 			jge onel
 			add angle,360
 			jmp another	
 	RIGHT ENDP
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 	LEFT PROC
 	
 		push cx
 		push dx
 		
 		call EvaluateNumber
 		mov cx,0
 		mov cx,num
 		cmp sign,1
 		je another2
 		
 		sub angle,cx
 		
 	subl2:
 		
 		cmp angle,0
 		jl anotherl
 		cmp angle,360
 		jle onel2 
 		sub angle,360
 		jmp subl2	
 	
 	onel2:
 		mov cx, angle 
 		
 		pop dx
 		pop cx	
 		
 		ret

 		
 	another2:
 		add angle,cx 
 		anotherl:
 			cmp angle,360
 			jg subl2
 			cmp angle,0
 			jge onel2
 			add angle,360
 			jmp anotherl	

 	LEFT ENDP
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;END OF INSTRUCTIONS FOR FORWARD,BACK,LEFT AND RIGHT
 
 ;BELOW ARE PROCEDURES FOR MOVING THE TURTLE PIXEL BY PIXEL GIVEN THAT THE TURTLE IS ORIENTED IN A PARICULAR WAY
  	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	;WHEN ANGLE IS ZERO
 	MoveAngZ PROC
 	 		
 	 		push bx
 	 		mov bx,turtle_row
   		  	cmp sign,0
   		  	je ok1
   		  	jmp ok2
   	   ok1 :
   	   		mov pixel_row,bx
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
   		  	mov bx,cx
   		  	;DrawTurtle 1111b
   	   		jmp cont2
   	   ok2:
   	   		mov pixel_row,bx
   		  	;sub pixel_row,3
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
  	 		mov bx, cx 
   	   		;DrawTurtle 1111b
   	   		;sub turtle_row,3
   	   		jmp cont2
   	   
   	   cont2:
   	   		DrawPixel 1111b	
   		  	mov dx,pixel_row
 			mov cx,pixel_col	
 			cmp dx,0
 			je exceed1
 			cmp dx,199
 			je exceed2
 	fixed:	dec bx
 			cmp bx,0
 			je fin
 			cmp sign,1
 			je nega
 			dec dx
 			mov pixel_row,dx
 			jmp cont2
   	    nega:
   	    	inc dx
 			mov pixel_row,dx
 			jmp cont2
 	 	
 	 	exceed1: 
 	 		mov dx, 199
 	 		jmp fixed
 	 		
 	 	exceed2:
 	 		mov dx, 0
 	 		jmp fixed
 	 			 		 
 	 	fin:
 	 		mov turtle_row,dx
 	 		mov turtle_col,cx
 	 		mov sign,0
 	 		pop bx
 	 		ret 
 	 MoveAngZ ENDP
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	;WHEN ANGLE IS FORTY FIVE
 	MoveAngFF PROC
 	 
 	 		push bx
 	 		push ax
 	 		
			mov ax,7
   			mul cx
   			mov bx,10
   			div bx
   		  	mov cx,ax
   		  	pop ax
   		  
 		  	
   		  	mov bx,turtle_row
   		  	cmp sign,0
   		  	je ok3
   		  	jmp ok4
   	   ok3 :
 			mov pixel_row,bx
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
   		  	;DrawTurtle 1111b
   	   		mov bx,cx
   	   		jmp cont3
   	   ok4:
   	   		;sub bl,3
   	   		mov pixel_row,bx
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
   		  	mov bx,cx
   		  	;DrawTurtle 1110b
   	   		jmp cont3
   	   
   	   cont3:
   	   		DrawPixel 1111b	
   		  	mov dx,pixel_row
 			mov cx,pixel_col	
 			cmp dx,0
 			je exceeds1
 			cmp dx,199
 			je exceeds2
fixed1:		cmp cx,320
 			jge exceedR1
			cmp cx,0
 			jle exceedL1
fixedR1:	dec bx
 			cmp bx,0
 			jle fin2
 			cmp sign,1
 			je nega2
 			dec dx
 			mov pixel_row,dx
 			inc cx
 			mov pixel_col,cx
 			jmp cont3
 			 			
   	    nega2:
   	    	inc dx
 			mov pixel_row,dx
 			dec cx
 			mov pixel_col,cx
 			jmp cont3
 	 		
 	 	exceeds1: 
 	 		mov dx, 199
 	 		jmp fixed1
 	 		
 	 	exceeds2:
 	 		mov dx, 0
 	 		jmp fixed1
 	 		
 	 	exceedR1:
 	 		mov cx,0
 	 		jmp fixedR1
 	 		
 	 	exceedL1:
 	 		mov cx,319
 	 		jmp fixedR1
 	 	
 	
 	 	fin2:
 	 		mov turtle_row,dx
 	 		mov turtle_col,cx 
 	 		mov sign,0
 	 		pop bx
 		  	ret
 	 		 
 	 MoveAngFF ENDP
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	;WHEN ANGLE IS NINETY
 	MoveAngNin PROC
 	 
 	 		push ax
 	 		push bx
 	 		mov bx,turtle_row
   		  	cmp sign,0
   		  	je ok5
   		  	jmp ok6
   	   ok5 :
   	   		;add bl,2
 			mov pixel_row,bx
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
   		  	mov bx,cx
   		  	;DrawTurtle 1111b
   	   		jmp cont4
   	   ok6:
   	   		;sub bl,3
   	   		mov pixel_row,bx
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
   		  	mov bx,cx
   		  	;DrawTurtle 1110b
   	   		jmp cont4
   	   
   	   cont4:
   	   		DrawPixel 1111b	
   		  	mov dx,pixel_row
 			mov cx,pixel_col	
 			cmp cx,320
 			jge exceedR
			cmp cx,0
 			jle exceedL
 	fixedR:	dec bx
 			cmp bx,0 
 			je fini
 			cmp sign,1
 			je negat
 			inc cx
 			mov pixel_col,cx
 			jmp cont4
   	    negat:
   	    	dec cx
 			mov pixel_col,cx
 			jmp cont4
 	 	
 	 	exceedR:
 	 		mov cx,0
 	 		jmp fixedR
 	 		
 	 	exceedL:
 	 		mov cx,319
 	 		jmp fixedR
 	 	
 	 	fini:
 	 		mov turtle_row,dx
 	 		mov turtle_col,cx
 	 		mov sign,0
 	 		pop bx
 	 		pop ax
 	 		ret 
 	 MoveAngNin ENDP
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 	
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	;ANGLE is 135
 	MoveAngOTF PROC
 	 
 	 		push ax
 	 		push bx
 	 		
			mov ax,5
   			mul cx
   			mov bx,7
   			div bx
   		  	mov cx,ax
   	
   	      		  	
   		  	mov bx,turtle_row
   		  	cmp sign,0
   		  	je ok7
   		  	jmp ok8
   	   ok7 :
 			mov pixel_row,bx
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
   		  	mov bx,cx
   		  	;DrawTurtle 1111b
   	   		jmp cont5
   	   ok8:
   	   		;sub bl,3
   	   		mov pixel_row,bx
   		  	mov dx,turtle_col
   		  	mov pixel_col,dx
   		  	mov bx,cx
   		  	;mov turtle_col,bx
   	   		;DrawTurtle 1110b
   	   		jmp cont5
   	   
   	      	   
   	   cont5:
   	   		DrawPixel 1111b	
   		  	mov dx,pixel_row
 			mov cx,pixel_col	
 			
 			cmp dx,0
 			je exceeds11
 			cmp dx,199
 			je exceeds21
fixed11:	cmp cx,320
 			jge exceedR11
			cmp cx,0
 			jle exceedL11
fixedR11:	dec bx
 			cmp bx,0
 			jle fin3
 			cmp sign,1
 			je nega3
 			inc dx
 			mov pixel_row,dx
 			inc cx
 			mov pixel_col,cx
 			jmp cont5
 			
 	 nega3:
   	    	dec dx
 			mov pixel_row,dx
 			dec cx
 			mov pixel_col,cx
 			jmp cont5
 			
 	exceeds11: 
 	 		mov dx, 199
 	 		jmp fixed11
 	 		
 	exceeds21:
 	 		mov dx, 0
 	 		jmp fixed11
 	 		
 	exceedR11:
 	 		mov cx,0
 	 		jmp fixedR11
 	 		
 	exceedL11:
 	 		mov cx,319
 	 		jmp fixedR11
 	 	
 	fin3:
 	 		mov sign,0
 		  	mov turtle_row,dx
 	 		mov turtle_col,cx
 		  	pop bx
 		  	pop ax
 		  	ret
 	 		 
 	 MoveAngOTF ENDP
 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
 	
END START



