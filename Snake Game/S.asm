[org 0x0100]
jmp start

game: db 'Welcome', 0 ; null terminated string
dev_by: db 'GAME Developed by', 0 ; null terminated string
Name1:db 'Muhammad Ibraheem Noor', 0 ; null terminated string
R_num1: db '21F-9068', 0 ; null terminated string
Name2:db 'Sadeed Amir', 0 ; null terminated string
R_num2: db '21F-9198', 0 ; null terminated string
key: db 'Press any key to continue', 0 ; null terminated string
delay: dd 0
S_len : dw 5 ; length of the snake
x : dw 10 ; x position of snake
y : dw 10 ; y position of snake
check_snake: dw 0 ; bool to check death
G_over: db ' GAME OVER ', 0 ; null terminated string
head_loc: dw 0 ; snake head location
score_display: db 'SCORE : ', 0 ; null terminated string
score: dw 0x2f ; total score
food_loc: dw 0 ; location of food
congrats: db 'Congrats! You WON the game', 0 ; null terminated string
win_check: dw 0 ; bool to check death
control: db 'CONTROLS', 0 ; null terminated string
instructions : db 'USE ARROW KEYS FOR MOVEMENT', 0 ; null terminated string

;;;;;;;;;;;;;; subroutine to clear the screen
clrscr: 
    push es
    push ax
    push cx
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    xor di, di ; point di to top left column
    mov ax, 0x0720 ; space char in normal attribute
    mov ah,225 ; bg colour
    mov cx, 2000 ; number of screen locations
    cld ; auto increment mode
    rep stosw ; clear the whole screen
    pop di
    pop cx
    pop ax
    pop es
    ret

;;;;;;;;;;;;;; subroutine to print a string
; takes the x position, y position, attribute, and address of a null
; terminated string as parameters
printstr: 
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di

    push ds
    pop es ; load ds in es
    mov di, [bp+4] ; point di to string
    mov cx, 0xffff ; load maximum number in cx
    xor al, al ; load a zero in al
    repne scasb ; find zero in the string
    mov ax, 0xffff ; load maximum number in ax
    sub ax, cx ; find change in cx
    dec ax ; exclude null from length
    jz exit ; no printing if string is empty

    mov cx, ax ; load string length in cx
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov al, 80 ; load al with columns per row
    mul byte [bp+8] ; multiply with y position
    add ax, [bp+10] ; add x position
    shl ax, 1 ; turn into byte offset
    mov di,ax ; point di to required location
    mov si, [bp+4] ; point si to string
    mov ah, [bp+6] ; load attribute in ah

    cld ; auto increment mode

nextchar: 
    lodsb ; load next char in al
    stosw ; print char/attribute pair
    loop nextchar ; repeat for the whole string

exit: 
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 8

;;;;;;;;;;;;;; subroutine for welcome screen
wel_scr:
     ; Printing welcome message
    mov ax, 36
    push ax ; push x position
    mov ax, 5
    push ax ; push y position
    mov ax, 0x6F ; white on brown attribute
    push ax ; push attribute
    mov ax, game
    push ax ; push address of message
    call printstr ; call the printstr subroutine

    ; Printing Name and Roll Number
        mov ax, 32
        push ax ; push x position
        mov ax, 7
        push ax ; push y position
        mov ax, 0x6D ; red on brown attribute
        push ax ; push attribute
        mov ax, dev_by
        push ax ; push address of message
        call printstr ; call the printstr subroutine
        
        mov ax, 29
        push ax ; push x position
        mov ax, 9
        push ax ; push y position
        mov ax, 0x6A ; green on brown attribute
        push ax ; push attribute
        mov ax, Name1
        push ax ; push address of message
        call printstr ; call the printstr subroutine

        mov ax, 36
        push ax ; push x position
        mov ax, 10
        push ax ; push y position
        mov ax, 0x6A ; green on brown attribute
        push ax ; push attribute
        mov ax, R_num1
        push ax ; push address of message
        call printstr ; call the printstr subroutine

        mov ax, 35
        push ax ; push x position
        mov ax, 12
        push ax ; push y position
        mov ax, 0x6A ; green on brown attribute
        push ax ; push attribute
        mov ax, Name2
        push ax ; push address of message
        call printstr ; call the printstr subroutine

        mov ax, 36
        push ax ; push x position
        mov ax, 13
        push ax ; push y position
        mov ax, 0x6A ; green on brown attribute
        push ax ; push attribute
        mov ax, R_num2
        push ax ; push address of message
        call printstr ; call the printstr subroutine

        mov ax, 28
        push ax ; push x position
        mov ax, 20
        push ax ; push y position
        ; binary of EF=11101111
        mov ax, 0xEF ; white on brown with white blinking
        push ax ; push attribute
        mov ax, key
        push ax ; push address of message
        call printstr ; call the printstr subroutine
    
    ; for press any key to continue
    mov ah, 0 ; service 0 - get keystroke
    int 0x16 ; call BIOS keyboard service
    ret

;;;;;;;;;;;;;; subroutine for controls screen    
controls_scr:
     ; Printing controls message
    mov ax, 36
    push ax ; push x position
    mov ax, 5
    push ax ; push y position
    mov ax, 0x6F ; white on brown attribute
    push ax ; push attribute
    mov ax, control
    push ax ; push address of message
    call printstr ; call the printstr subroutine

    mov ax, 26
    push ax ; push x position
    mov ax, 9
    push ax ; push y position
    mov ax, 0x6A ; white on brown attribute
    push ax ; push attribute
    mov ax, instructions
    push ax ; push address of message
    call printstr ; call the printstr subroutine
    
    mov ax, 28
    push ax ; push x position
    mov ax, 20
    push ax ; push y position
    mov ax, 0xEF ; white on brown with white blinking
    push ax ; push attribute
    mov ax, key
    push ax ; push address of message
    call printstr ; call the printstr subroutine
    
    ; for press any key to continue
    mov ah, 0 ; service 0 - get keystroke
    int 0x16 ; call BIOS keyboard service
    ret 4

;;;;;;;;;;;;;; print snake
print_snake:
    push bp
    mov bp, sp
    push es
    push ax
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov al, 80
    mul byte [bp+4] ; multiplying y with 80
    add ax, [bp+6] ; adding (y*80) and x
    shl ax, 1 ; multiplying with 2
    mov di, ax ; moving ax to di
    
        mov ah, 2h ; color attribute
        mov cx, [bp+8] ; moving length to cx
        mov al,0x2a ; moving the ascii of "*" to al
        call clrscr
        
        next_snake: 
            mov word[es:di], ax
            add di, 2 ; printing in forward direction
            dec cx
            cmp cx,1
            jne ok 
            mov al,02h ; smiley
            ok:
                cmp cx,0
                jne next_snake
            
            
        pop di
        pop ax
        pop es
        pop bp
        ret 8


;;;;;;;;;;;;;; snake right movement
printsnake_right:
    push bp
    mov bp, sp
    push es
    push ax
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov al, 80
    mul byte [bp+4] ; multiplying y with 80
    add ax, [bp+6] ; adding (y*80) and x
    shl ax, 1 ; multiplying with 2
    mov di, ax ; moving ax to di

        mov ah, 2h ; color attribute
        mov cx, [bp+8] ; moving length to cx
        mov al,0x2a ; moving the ascii of "*" to al
        call clrscr
        
        next_snake_right: 
            mov word[es:di], ax
            add di, 2 ; printing in forward direction
            dec cx
            cmp cx,1
            jne ok_right 
            mov al,02h ; smiley
            ok_right:
                cmp cx,0
                jne next_snake_right
                        
            call print_border
            call printFood
            mov ax,di ; moving snake head in ax
            mov word[head_loc],ax ; storing snake haed in variable
            
            
        end_right:
        pop di
        pop ax
        pop es
        pop bp
        ret 8

;;;;;;;;;;;;;; snake left movement
printsnake_left:
    push bp
    mov bp, sp
    push es
    push ax
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov al, 80
    mul byte [bp+4] ; multiplying y with 80
    add ax, [bp+6] ; adding (y*80) and x
    shl ax, 1 ; multiplying with 2
    mov di, ax ; moving ax to di

        mov ah, 2h ; color attribute
        mov cx, [bp+8] ; moving length to cx
        mov al,0x2a ; moving the ascii of "*" to al
        call clrscr
        
        next_snake_left: 
            mov word[es:di], ax
            sub di, 2 ; printing in forward direction
            dec cx
            cmp cx,1
            jne ok_left 
            mov al,02h ; smiley
            ok_left:
                cmp cx,0
                jne next_snake_left
            
            call print_border
            call printFood
            mov ax,di ; moving snake head in ax
            mov word[head_loc],ax ; storing snake haed in variable

            
        end_left:
        pop di
        pop ax
        pop es
        pop bp
        ret 8

;;;;;;;;;;;;;; snake up movement
printsnake_up:
    push bp
    mov bp, sp
    push es
    push ax
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov al, 80
    mul byte [bp+4] ; multiplying y with 80
    add ax, [bp+6] ; adding (y*80) and x
    shl ax, 1 ; multiplying with 2
    mov di, ax ; moving ax to di

        mov ah, 2h ; color attribute
        mov cx, [bp+8] ; moving length to cx
        mov al,0x2a ; moving the ascii of "*" to al
        call clrscr
        
        next_snake_up: 
            mov word[es:di], ax
            sub di, 160 ; printing in upward direction
            dec cx
            cmp cx,1
            jne ok_up 
            mov al,02h ; smiley
            ok_up:
                cmp cx,0
                jne next_snake_up
            
            call print_border
            call printFood
            mov ax,di ; moving snake head in ax
            mov word[head_loc],ax ; storing snake haed in variable
            
            
        end_up:
        pop di
        pop ax
        pop es
        pop bp
        ret 8

;;;;;;;;;;;;;; snake down movement
printsnake_down:
    push bp
    mov bp, sp
    push es
    push ax
    push di
    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov al, 80
    mul byte [bp+4] ; multiplying y with 80
    add ax, [bp+6] ; adding (y*80) and x
    shl ax, 1 ; multiplying with 2
    mov di, ax ; moving ax to di

        mov ah, 2h ; color attribute
        mov cx, [bp+8] ; moving length to cx
        mov al,0x2a ; moving the ascii of "*" to al
        call clrscr
        
        next_snake_down: 
            mov word[es:di], ax
            add di, 160 ; printing in upward direction
            dec cx
            cmp cx,1
            jne ok_down 
            mov al,02h ; smiley
            ok_down:
                cmp cx,0
                jne next_snake_down
            
            call print_border
            call printFood
            mov ax,di ; moving snake head in ax
            mov word[head_loc],ax ; storing snake haed in variable
            
            
        end_down:
        pop di
        pop ax
        pop es
        pop bp
        ret 8

;;;;;;;;;;;;;;subroutine to get random numbers
randomNumber:
	push ax
	push dx
	
	mov ah, 00h  ; interrupts to get system time        
	int 1AH      ; CX:DX now hold number of clock ticks since midnight      

	mov  ax, dx
	mov  dx, 1
	mov  cx, [bp-2]  ;upper number
	div  cx       ; here dx contains the remainder of the division - from 0 to 9

	mov bx,dx
   
	pop dx
	pop ax
	ret

;;;;;;;;;;;;;; subroutine to print food
printFood:
	push ax
	push bx
	push di
	push es

    mov ax, word[food_loc]
    cmp word[head_loc],ax
    jne old_food

    inc word[score]; incrementing score
    inc word[S_len]; incrementing snake length
	mov bx,80; x pos
	mov [bp-2],bx
	call randomNumber
	mov [bp-4],bx ; storing X pos in bp-4
	
	mov bx,25 ;y pos
	mov [bp-2],bx
	call randomNumber
	mov [bp-2],bx ; storing Y pos in bp-2
	
	call calculatePosition
	mov ax,0x0F40
	mov word [es:di],ax
    mov ax,di ; moving food location in ax
    mov word[food_loc],ax ; storing food location in variable
    	
    old_food:
        mov di,word[food_loc]
        mov ax,0x0F40
	    mov word [es:di],ax
	
	pop es
	pop di
	pop bx
	pop ax
	ret 

;;;;;;;;;;;;;; subroutine to calculate position
calculatePosition:
	push cx
	push dx
	mov cx,[bp-2] ;y pos 
    mov ax,80
    mul cx
    add ax,[bp-4] ;x pos 
    shl ax,1

	mov di,ax
    mov ax,0xb800
    mov es,ax
	
	pop dx
	pop cx
	ret

;;;;;;;;;;;;;;subroutine for delay
delay_sub:
    mov dword[delay],400000
    ;a large loop for delaying next move
    delay_loop:
        dec dword[delay]
        cmp dword[delay],0
        jne delay_loop
    ret

;;;;;;;;;;;;;; subroutine to print border
border:
    push bp
    mov bp, sp
    push es
    push ax
    push di
    push cx
    mov ax, 0xb800
    mov es, ax ; point es to video base 
    mov di, [bp+4]
    mov cx, [bp+8]
    border_loop:
        mov word[es:di], 0x0Eb2
        add di, [bp+6]
        loop border_loop

    pop cx
    pop di
    pop ax
    pop es
    pop bp
    ret 6

;;;;;;;;;;;;;; Subroutine to check death
check_loc:
    push bp
    mov bp,sp
    push ax
    push cx
    push dx
    push si
    mov cx, 0
    mov ax, [bp+6]
check_left:                        ;LEFT SIDE BORDER CHECK
    cmp cx, ax
    je found
    add cx, 160
    cmp cx, 3840
    jne check_left

    mov cx, 158
check_right:                       ;RIGHT SIDE BORDER CHECK
    cmp cx, ax
    je found
    add cx, 160
    cmp cx, 3998
    jne check_right

    mov cx, 0
check_up:                          ;TOP SIDE BORDER CHECK
    cmp cx, ax
    je found
    add cx, 2
    cmp cx, 158
    jne check_up

    mov cx, 3840
check_down:                        ;BOTTOM SIDE BORDER CHECK
    cmp cx, ax
    je found
    add cx, 2
    cmp cx, 4000
    jne check_down

    jmp n_leave
found:                             ;IF SNAKE TOUCHES BORDER, COME TO "found" AND EXIT
    mov dx, 1
    mov si, [bp+4]
    mov [si], dx
    
n_leave:                           ;IF NOT TOUCH BORDER, LEAVE THE LOOP AS IT IS
    pop si
    pop dx
    pop cx
    pop ax
    pop bp
    ret 4

;;;;;;;;;;;;; printing all the borders
print_border:
    ; printing the left border
    mov ax, 25
    push ax
    mov ax, 160
    push ax
    mov ax, 0
    push ax
    call border
    ; printing the right border
    mov ax, 25
    push ax
    mov ax, 160
    push ax
    mov ax, 158
    push ax
    call border
    ; printing the upper border
    mov ax, 80
    push ax
    mov ax, 2
    push ax
    mov ax, 0
    push ax
    call border
    ; printing the lower border
    mov ax, 80
    push ax
    mov ax, 2
    push ax
    mov ax, 3840
    push ax
    call border

     ; Printing score
    mov ax, 5
    push ax ; push x position
    mov ax, 0
    push ax ; push y position
    mov ax, 0x6F ; white on brown attribute
    push ax ; push attribute
    mov ax, score_display
    push ax ; push address of message
    call printstr ; call the printstr subroutine

    mov ax, 13
    push ax ; push x position
    mov ax, 0
    push ax ; push y position
    mov ax, 0x6F ; white on brown attribute
    push ax ; push attribute
    mov ax, score
    push ax ; push address of message
    call printstr ; call the printstr subroutine

    ret

;;;;;;;;;;;;; checking death in gamer boi
gamer_death:
    mov word[check_snake], 0 ; Checking death condition
    push word[head_loc]
    push check_snake
    call check_loc
    ret

win_win:
    cmp word[score],0x39
    jne exit_win

    ;displaying win_win message
    mov ax, 28
    push ax ; push x position
    mov ax, 13
    push ax ; push y position
    ; binary of EF=11101111
    mov ax, 0xEF ; white on brown with white blinking
    push ax ; push attribute
    mov ax, congrats
    push ax ; push address of message
    call printstr ; call the printstr subroutine
    mov word[win_check],1

    exit_win:
ret
;;;;;;;;;;;;; main game subroutine
Gamer_boi:
    game_loop:
        call win_win 
         
        ; keyboard input
        push ax
        mov ah, 0 ; service 0 - get keystroke
        int 0x16 ; call BIOS keyboard service

        ; AH = BIOS scan code
        cmp ah,0x48
        je up
        cmp ah,0x4B
        je left
        cmp ah,0x4D
        je right
        cmp ah,0x50
        je down

        

        up:
            dec word[y] ; decrementing y to move up
            push word [S_len] ; push initial snake length
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printsnake_right

            call gamer_death
            cmp word[check_snake], 1
            jne game_loop ; if not dead
            cmp word[check_snake], 1
            je snake_dead ; if dead

        left:
            dec word[x] ; decrementing x to move left
            push word [S_len] ; push initial snake length
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printsnake_right

            call gamer_death
            cmp word[check_snake], 1
            jne game_loop ; if not dead
            cmp word[check_snake], 1
            je snake_dead ; if dead

        right:
            inc word[x] ; incrementing x to move right
            push word [S_len] ; push initial snake length
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printsnake_right

            call gamer_death
            cmp word[check_snake], 1
            jne game_loop ; if not dead
            cmp word[check_snake], 1
            je snake_dead ; if dead

        down:
            inc word[y] ; incrementing y to move up
            push word [S_len] ; push initial snake length
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printsnake_right

            call gamer_death
            cmp word[check_snake], 1
            jne game_loop ; if not dead
            cmp word[check_snake], 1
            je snake_dead ; if dead
            
        cmp word[win_check],1
        je exit_gamer
       
        snake_dead: ; if dead
            call clrscr
            mov ax, 33
            push ax ; push x position
            mov ax, 12
            push ax ; push y position
            mov ax, 15 ; white on black attribute
            push ax ; push attribute
            mov ax, G_over
            push ax ; push address of message
            call printstr ; call the printstr subroutine
    
    exit_gamer:       
    ret 2

;;;;;;;;;;;;; START
start: 
    call clrscr ; call the clrscr subroutine

    call wel_scr ; call the welcome subroutine

    call clrscr ; call the clrscr subroutine     

    call clrscr ; call the clrscr subroutine

    call controls_scr ; call the controls subroutine

    call clrscr ; call the clrscr subroutine     


    push word [S_len] ; push initial snake length
    push word[x] ; push x on stack
    push word[y] ; push y on stack
    call print_snake

    call print_border

    call printFood ; calling the food subroutine

    call Gamer_boi

mov ax, 0x4c00 ; terminate program
int 0x21