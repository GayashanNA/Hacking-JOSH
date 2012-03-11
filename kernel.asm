;Modified by Gayashan Amarasinghe
;090018T
;Department of Computer Science and Engineering
;Universoty of Moratuwa

;*****************start of the kernel code***************
[org 0x000]
[bits 16]

[SEGMENT .text]

;START #####################################################
    mov ax, 0x0100			;location where kernel is loaded
    mov ds, ax
    mov es, ax
    
    cli
    mov ss, ax				;stack segment
    mov sp, 0xFFFF			;stack pointer at 64k limit
    sti

    push dx
    push es
    xor ax, ax
    mov es, ax
    cli
    mov word [es:0x21*4], _int0x21	; setup interrupt service
    mov [es:0x21*4+2], cs
    sti
    pop es
    pop dx

    mov si, strWelcomeMsg   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21

	call _shell				; call the shell
    
    int 0x19                ; reboot
;END #######################################################

_int0x21:
    _int0x21_ser0x01:       ;service 0x01
    cmp al, 0x01            ;see if service 0x01 wanted
    jne _int0x21_end        ;goto next check (now it is end)
    
	_int0x21_ser0x01_start:
    lodsb                   ; load next character
    or  al, al              ; test for NUL character
    jz  _int0x21_ser0x01_end
    mov ah, 0x0E            ; BIOS teletype
    mov bh, 0x00            ; display page 0
    mov bl, 0x07            ; text attribute
    int 0x10                ; invoke BIOS
    jmp _int0x21_ser0x01_start
    _int0x21_ser0x01_end:
    jmp _int0x21_end

    _int0x21_end:
    iret

_shell:
	_shell_begin:
	;move to next line
	call _display_endl

	;display prompt
	call _display_prompt

	;get user command
	call _get_command
	
	;split command into components
	call _split_cmd

	;check command & perform action

	; empty command
	_cmd_none:		
	mov si, strCmd0
	cmp BYTE [si], 0x00
	jne	_cmd_ver		;next command
	jmp _cmd_done
	
	; display version
	_cmd_ver:		
	mov si, strCmd0
	mov di, cmdVer
	mov cx, 4
	repe	cmpsb
	;jne	_cmd_exit		;next command
	jne	_cmd_test		;test command
	
	call _display_endl
	mov si, strOsName		;display version
	mov al, 0x01
    int 0x21
	call _display_space
	mov si, txtVersion		;display version
	mov al, 0x01
    int 0x21
	call _display_space

	mov si, strMajorVer		
	mov al, 0x01
    int 0x21
	mov si, strMinorVer
	mov al, 0x01
    int 0x21
	jmp _cmd_done
	
    _cmd_test:				;view who editted the OS
    	mov si, strCmd0
    	mov di, cmdWho
    	mov cx, 4
    	repe	cmpsb
    	jne	_cmd_hinfo
					;custom command
	call _display_endl
	call _display_custom_msg
	jmp _cmd_done
					
    _cmd_hinfo:				;view hardware information					
	mov si, strCmd0
	mov di, cmdHInfo
	mov cx, 6
	repe	cmpsb
	jne	_cmd_help
	
	call _display_endl
	call _display_hardware_info				
	jmp _cmd_done
	
    _cmd_help:				;view help
    	mov si, strCmd0
	mov di, cmdHelp
	mov cx, 5
	repe	cmpsb
	jne	_cmd_exit		;next command
	
	call _display_endl
	call _display_help				
	jmp _cmd_done	
					
	; exit shell
	_cmd_exit:		
	mov si, strCmd0
	mov di, cmdExit
	mov cx, 5
	repe	cmpsb
	jne	_cmd_unknown		;next command

	je _shell_end			;exit from shell

	_cmd_unknown:
	call _display_endl
	mov si, msgUnknownCmd		;unknown command
	mov al, 0x01
    int 0x21

	_cmd_done:

	;call _display_endl
	jmp _shell_begin
	
	_shell_end:
	ret

_get_command:
	;initiate count
	mov BYTE [cmdChrCnt], 0x00
	mov di, strUserCmd

	_get_cmd_start:
	mov ah, 0x10		;get character
	int 0x16

	cmp al, 0x00		;check if extended key
	je _extended_key
	cmp al, 0xE0		;check if new extended key
	je _extended_key

	cmp al, 0x08		;check if backspace pressed
	je _backspace_key

	cmp al, 0x0D		;check if Enter pressed
	je _enter_key

	mov bh, [cmdMaxLen]		;check if maxlen reached
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start

	;add char to buffer, display it and start again
	mov [di], al			;add char to buffer
	inc di					;increment buffer pointer
	inc BYTE [cmdChrCnt]	;inc count

	mov ah, 0x0E			;display character
	mov bl, 0x07
	int 0x10
	jmp	_get_cmd_start

	_extended_key:			;extended key - do nothing now
	jmp _get_cmd_start

	_backspace_key:
	mov bh, 0x00			;check if count = 0
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start		;yes, do nothing
	
	dec BYTE [cmdChrCnt]	;dec count
	dec di

	;check if beginning of line
	mov	ah, 0x03		;read cursor position
	mov bh, 0x00
	int 0x10

	cmp dl, 0x00
	jne	_move_back
	dec dh
	mov dl, 79
	mov ah, 0x02
	int 0x10

	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_move_back:
	mov ah, 0x0E		; BIOS teletype acts on backspace!
    mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_enter_key:
	mov BYTE [di], 0x00
	ret

_split_cmd:
	;adjust si/di
	mov si, strUserCmd
	;mov di, strCmd0

	;move blanks
	_split_mb0_start:
	cmp BYTE [si], 0x20
	je _split_mb0_nb
	jmp _split_mb0_end

	_split_mb0_nb:
	inc si
	jmp _split_mb0_start

	_split_mb0_end:
	mov di, strCmd0

	_split_1_start:			;get first string
	cmp BYTE [si], 0x20
	je _split_1_end
	cmp BYTE [si], 0x00
	je _split_1_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_1_start

	_split_1_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb1_start:
	cmp BYTE [si], 0x20
	je _split_mb1_nb
	jmp _split_mb1_end

	_split_mb1_nb:
	inc si
	jmp _split_mb1_start

	_split_mb1_end:
	mov di, strCmd1

	_split_2_start:			;get second string
	cmp BYTE [si], 0x20
	je _split_2_end
	cmp BYTE [si], 0x00
	je _split_2_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_2_start

	_split_2_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb2_start:
	cmp BYTE [si], 0x20
	je _split_mb2_nb
	jmp _split_mb2_end

	_split_mb2_nb:
	inc si
	jmp _split_mb2_start

	_split_mb2_end:
	mov di, strCmd2

	_split_3_start:			;get third string
	cmp BYTE [si], 0x20
	je _split_3_end
	cmp BYTE [si], 0x00
	je _split_3_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_3_start

	_split_3_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb3_start:
	cmp BYTE [si], 0x20
	je _split_mb3_nb
	jmp _split_mb3_end

	_split_mb3_nb:
	inc si
	jmp _split_mb3_start

	_split_mb3_end:
	mov di, strCmd3

	_split_4_start:			;get fourth string
	cmp BYTE [si], 0x20
	je _split_4_end
	cmp BYTE [si], 0x00
	je _split_4_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_4_start

	_split_4_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb4_start:
	cmp BYTE [si], 0x20
	je _split_mb4_nb
	jmp _split_mb4_end

	_split_mb4_nb:
	inc si
	jmp _split_mb4_start

	_split_mb4_end:
	mov di, strCmd4

	_split_5_start:			;get last string
	cmp BYTE [si], 0x20
	je _split_5_end
	cmp BYTE [si], 0x00
	je _split_5_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_5_start

	_split_5_end:
	mov BYTE [di], 0x00

	ret

_display_space:
	mov ah, 0x0E                            ; BIOS teletype
	mov al, 0x20
    mov bh, 0x00                            ; display page 0
    mov bl, 0x07                            ; text attribute
    int 0x10                                ; invoke BIOS
	ret

_display_endl:
	mov ah, 0x0E		; BIOS teletype acts on newline!
    mov al, 0x0D
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x0E		; BIOS teletype acts on linefeed!
    mov al, 0x0A
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	ret

_display_prompt:
	mov si, strPrompt
	mov al, 0x01
	int 0x21
	ret

;***************************************************************************************
;display the hardaware information - invoked by hinfo command

_display_hardware_info:
	call _display_endl
	mov si, strHInfo
	mov al, 0x01
	int 0x21
	
	;read low memory
	call _show_CPU_VendorID			;show CPU vendor ID
	call _show_Processor_Brand		;show CPU Brand
	call _show_floppy_info			;show floppy drive related info
	call _show_serial_info			;show info of serial port(s)
	call _show_parallel_info		;show info of parallel port(s)
	call _show_memory_info			;show memory related info
	call _show_HDD_info			;show HDD related info
	call _show_Mouse_status			;show PS/2 mouse status
	ret
;end
	
_show_CPU_VendorID:
	;******************read CPU vendor ID
	mov eax,0
	cpuid
	mov [vendorID], ebx
	mov [vendorID+4],edx
	mov [vendorID+8],ecx		
	
	call _display_endl
	mov si, strVendorID
	mov al, 0x01
     int 0x21
     	
     	call _display_space
	mov si, vendorID		;print CPU vender ID
	mov al, 0x01
    int 0x21
	ret
;end

_show_Processor_Brand:
	;******************read processor brand
	mov eax,0x80000002
	cpuid				;get the processor brand string
	mov [CPUbrand],eax
	mov [CPUbrand+4],ebx
	mov [CPUbrand+8],ecx
	mov [CPUbrand+12],edx

	mov eax,0x80000003
	cpuid
	mov [CPUbrand+16],eax
	mov [CPUbrand+20],ebx
	mov [CPUbrand+24],ecx
	mov [CPUbrand+28],edx

	mov eax,0x80000004
	cpuid
	mov [CPUbrand+32],eax
	mov [CPUbrand+36],ebx
	mov [CPUbrand+40],ecx
	mov [CPUbrand+44],edx

	call _display_endl
	mov si, strProcessor
	mov al,0x01
    int 0x21
	call _display_space
	mov si, CPUbrand
	mov al,0x01
    int 0x21
	ret
;end
	
_show_floppy_info:
	
	;*****************display floppy driver information
	;read whether the floppy drives are availabe and if so show the number of available drives
	
	call _display_endl
	mov si, strFloppyD
	mov al, 0x01
    int 0x21
    	call _display_space
    	xor ax, ax
    int 0x11
    	and ax, 0x01			;get first bit
    	cmp ax, 0x01			;if the first bit is 1
    	je _available_floppyD		;show the available number of floppy drives
    	jmp _no_floppyD			;if no floppy drives are available
    	ret
    	
_no_floppyD:
	;if no floppy drives are available print 0
    	mov ah, 0x0e
    	mov al, '0'
    int 0x10
    	ret
    		
_available_floppyD:
	;send interrupt 11h and read the 6-7 bits from the ax register to read the available number of floppy drives 
	
    	xor ax, ax
    int 0x11			;send the interrupt, it sets the ax register
    	and ax, 0xc0		;get the 6-7 bits
    	shr ax, 6		;shift right 6 bits
    	add al, 0x31		;add 49 because the bit indicates [No of floppy drives - 1]
    	mov ah, 0x0e
    int 0x10 
    	ret
    	
;end
   	
_show_serial_info:    	    	  		
	;*****************display available number of serial ports
	;send interrupt 11h to get the equipment list to the ax register and read bits from 9 - 11
	;to read the number of available serial ports
	
	call _display_endl
	mov si, strSerialPorts
	mov al,0x01
    int 0x21
    	call _display_space
	xor ax, ax		;clear the ax register
    int 0x11			;send the interrupt, it sets the ax register
	and ax, 0x0e00             ;keep the bits 9-11
	shr ax, 9	  	   ;shift 9 bits right                  
	add al, 0x30               ;add 48 and convert to decimal
	mov ah, 0x0e
    int 0x10
   	ret
   
;end
   	
_show_parallel_info:
   	;*****************display available number of parallel ports
   	;send interrupt 11h to get the equipment list to the ax register and read the bits from 14 - 15
   	;to read the number of available parallel ports 
   	
    	call _display_endl
    	mov si, strParallelPorts
    	mov al,0x01
    int 0x21
    	call _display_space
	xor ax, ax
    int 0x11			;send the interrupt, it sets the ax register
	and ax, 0x0c000            ;keep the bits 14-15
    	shr ax, 14		   ;shift 14 bits right 
    	add al, 0x30
    	mov ah, 0x0e
    int 0x10
    	ret
    	
;end
    	
_show_memory_info:
    	;*******************display lower memory size
    	;read the offset 13h of the 0040:0000h to get the Memory size in KB
    	
    	call _display_endl
    	mov si, strLowerMemory
    	mov al, 0x01
    int 0x21
    	
    	call _display_space
	
	push es
	mov ax, 0x40
	mov es, ax
	mov ax, [es:13h]	;13h offset from 0040:0000h includes the lower memory size
	mov dx, ax
	call _display_space
	call _hexToDec
	mov si, strKB
	mov al, 0x01
    int 0x21
	
	pop es
    	
    	call _display_endl
    	
    	xor cx, cx			;clear cx
	xor dx, dx			;clear dx
	mov ax, 0xe801
    int 0x15
	jc _MemErr		; CF set on error
	cmp ah, 0x86		; unsupported function
	je _MemErr
	cmp ah, 0x80		; invalid command
	je _MemErr

	mov si, strUpperMemory
	mov al, 0x01
    int 0x21
	
	call _display_space
	
	cmp cx, 0x0000		;if CX=0
	je _remove_cx_conflict
	jmp _memCalculate

_remove_cx_conflict:		
	;some bioses return CX=BX=0 if so copy ax to cx and bx to dx
	mov cx,ax		
	mov dx,bx

_memCalculate:
	;Now CX = configured memory 1M to 16M, in K
	;Now DX = configured memory above 16M, in 64K blocks
	;configured memory above 16M in MBs = DX*64/1024 = (DX/2^4)
	shr dx, 4		;divide dx by 2^4 or shift 4 bits right
	shr cx, 10		;divide cx by 2^10 or shift 10 bits right
	add cx,dx		;total memory

	mov dx, cx		;move total memory size to dx
	call _hexToDec		;convert hex to decimal	
	mov si, strMB
	mov al, 0x01
    int 0x21

	ret
		
_MemErr:
	mov si, strMemErr	;in case an error occured while reading memory
	mov al, 0x01
    int 0x21
		
;end

_show_HDD_info:
	;***********************display no of available HDD
	;read the offset 75h of the 0040:0000h to get the number of available HDDs
	
	call _display_endl
	mov si, strNoOfHDD	;print string stored in the strNoOfHDD
	mov al, 0x01
    int 0x21
    	call _display_space
    	
    	push es			;save the current values of the es register
    	mov ax, 0x40		;get the address no 0x40 to the ax register
    	mov es, ax		;move ax to the es
    	mov al, [es:75h]	;get the required offset to display no of HDD
    	add al, 0x30		;convert the number to ASCII by adding 48(decimal) = 0x30
    	mov ah, 0x0e		;print the number
    int 0x10
    	
    	pop es			;restore the value stored in the es register
    	
    	ret
;end

_show_Mouse_status:
	;display if the PS/2 mouse is installed
	;send int 11h and read the 2nd bit to get the PS/2 info
	
	call _display_endl
	mov si, strMouseStatus		;print string stored in the strMouseStatus
	mov al, 0x01
    int 0x21
    	call _display_space
    	
    	xor ax, ax
    int 0x11
    	and ax, 0x04		;get 2nd bit
    	shr ax, 2		;shift 2 bits right
    	cmp ax, 0x01
    	je _print_true		;if the bit is 1 print 'true'
    	jmp _print_false	;else print 'false'
    	ret

_print_true:			;print 'true'
    	mov si, strTrue
    	mov al, 0x01
    int 0x21
    	ret
    	
_print_false:			;print 'false'
	mov si, strFalse
	mov al, 0x01
    int 0x21
    	ret
    		
;end

_display_custom_msg:		;print the string stored in the strTest
	mov si, strTest
	mov al, 0x01
    int 0x21
	ret
;end

_display_help:			;display the help
	mov si, strHelp0
	mov al, 0x01
    int 0x21
	call _display_endl
	mov si, strHelp1
	mov al, 0x01
    int 0x21
    	call _display_endl
	mov si, strHelp2
	mov al, 0x01
    int 0x21
	call _display_endl
	mov si, strHelp3
	mov al, 0x01
    int 0x21
    	call _display_endl
	mov si, strHelp4
	mov al, 0x01
    int 0x21
            
    ret

;end

;*****************************************************************************
_hexToDec:			;convert hex to decimal
	push ax
	push bx
	push cx
	push si
	mov ax,dx                ;copy number into AX
	mov si,10                ;SI will be our divisor
	xor cx,cx                ;clean up the CX

_non_zero:

	xor dx,dx                ;clean up the DX
	div si                   ;divide by 10
	push dx                  ;push number onto the stack
	inc cx                   ;increment CX to do it more times
	or ax,ax                 ;end of the number?
	jne _non_zero		 ;if not go to _non_zero

_write_digits:

	pop dx                    ;get the digit off DX
	add dl,0x30               ;add 48 to get the ASCII value
	call _print_char          ;print 
	loop _write_digits        ;keep going till cx == 0

	pop si                   ;restore SI
	pop cx                   ;restore DX
	pop bx                   ;restore CX
	pop ax                   ;restore AX
	ret                      

_print_char:
	push ax                 ;save the current AX register
	mov al, dl
        mov ah, 0x0E            ;BIOS teletype acts on newline
        mov bh, 0x00
        mov bl, 0x07
    int 0x10

	pop ax                  ;restore the AX register
	ret
;end

;************************************************************************************************
[SEGMENT .data]
    strWelcomeMsg   db  "Welcome to JOSH Ver 0.04", 0x00
	strPrompt		db	"JOSH>>", 0x00
	cmdMaxLen		db	255			;maximum length of commands

	strOsName		db	"JOSH", 0x00	;OS details
	strMajorVer		db	"0", 0x00
	strMinorVer		db	".04", 0x00
	strTest			db	"Modified by Gayashan Amarasinghe.", 0x00
	strHInfo		db	"Hardware information:", 0x00
	
	strVendorID		db	" CPU Vendor ID:", 0x00
	strProcessor		db	" Processor:", 0x00
	strSerialPorts		db	" Serial ports:", 0x00
	strParallelPorts	db	" Parallel ports:", 0x00
	strFloppyD		db	" Floppy drives:", 0x00
	strLowerMemory		db	" Lower memory size:", 0x00
	strUpperMemory		db	" Upper memory size:", 0x00
	strKB			db	"KB", 0x00
	strMB			db	"MB", 0x00
	strNoOfHDD		db	" No of Hard Disk Drives:", 0x00
	strMouseStatus		db	" PS/2 Mouse installed:", 0x00
	strTrue			db	"true", 0x00
	strFalse		db	"false", 0x00
	
	strMemErr		db	"Error reading the memory size.", 0x00
	
	strHelp0		db	"Command | Action", 0x00
	strHelp1		db	"ver     - view the JOSH version", 0x00
	strHelp2		db	"exit    - reboot", 0x00
	strHelp3		db	"hinfo   - view hardware information", 0x00
	strHelp4		db	"who     - who modified the JOSH to current version", 0x00

	cmdVer			db	"ver", 0x00		; internal commands
	cmdExit			db	"exit", 0x00
	cmdWho			db	"who", 0x00		;test command
	cmdHInfo		db	"hinfo", 0x00		;view hardware information
	cmdHelp			db	"help", 0x00		;view help

	txtVersion		db	"version", 0x00	;messages and other strings
	msgUnknownCmd	db	"Unknown command or bad file name!", 0x00

[SEGMENT .bss]
	vendorID	resb	12		;CPU vendor ID
	CPUbrand	resb	48		;CPU brand
	strUserCmd	resb	256		;buffer for user commands
	cmdChrCnt	resb	1		;count of characters
	strCmd0		resb	256		;buffers for the command components
	strCmd1		resb	256
	strCmd2		resb	256
	strCmd3		resb	256
	strCmd4		resb	256

;********************end of the kernel code********************
