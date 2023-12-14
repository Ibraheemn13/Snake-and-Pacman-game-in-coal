[org 0x0100]
jmp start

game: db 'Welcome', 0 ; null terminated string
dev_by: db 'GAME Developed by', 0 ; null terminated string
Name1:db 'Muhammad Saad Zubair', 0 ; null terminated string
R_num1: db '22F-3288', 0 ; null terminated string
Name2:db 'Urwa Sajid', 0 ; null terminated string
R_num2: db '22F-3244', 0 ; null terminated string
key: db 'Press any key to continue', 0 ; null terminated string
delay: dd 0
x : dw 10 ; x position of pacman
y : dw 10 ; y position of pacman
check_pacman: dw 0 ; bool to check death
G_over: db ' GAME OVER ', 0 ; null terminated string
head_loc: dw 0 ; pacman head location
score_display: db 'SCORE : ', 0 ; null terminated string
score: dw 0x2f ; total score
food_loc: dw 0 ; location of food
congrats: db 'Congrats! You WON the game', 0 ; null terminated string
win_check: dw 0 ; bool to check death
control: db 'CONTROLS', 0 ; null terminated string
instructions : db 'USE ARROW KEYS FOR MOVEMENT', 0 ; null terminated string
enemy_y: dw 2 ; initial x position of the enemy
enemy_x: dw 6 ; initial y position of the enemy
enemy_head: dw 0

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
    mov ah,0x3F; bg colour
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

;;;;;;;;;;;;;; print pacman
print_pacman:
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
    
        mov ah, 1Fh ; color attribute
        mov al,02h ; moving the ascii of "smiley" to al
        call clrscr
        
            mov word[es:di], ax
            
            
        pop di
        pop ax
        pop es
        pop bp
        ret 8


;;;;;;;;;;;;;; pacman  movement
printpacman_right:

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

        mov ah, 0xF1 ; color attribute
        mov al,02h ; moving the ascii of "smiley" to al
        call clrscr
         
            mov word[es:di], ax
            
            call print_border
            call printFood
            
           ;call print_enemy_loop
            ;print enemy
            mov ax,di ; moving pacman head in ax
            mov word[head_loc],ax ; storing pacman haed in variable
            
            
        end_right:
        pop di
        pop ax
        pop es
        pop bp
        ret 8

print_enemy:
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

        mov ah, 04h ; color attribute
       
        mov al,02h ; moving the ascii of "smiley" to al
        call clrscr
         
            mov word[es:di], ax
            
            mov ax,di ; moving enemy head in ax
            mov word[enemy_head],ax ; storing enemy haed in variable

        pop di
        pop ax
        pop es
        pop bp

        ret 4   
;;;;;;;;;;;;;upper loop for enemy
enemy_loop:

mov cx, 34 
loopEn:
        mov ax, [enemy_x]
	push ax
	mov ax, [enemy_y]
	push ax
	
	;call Gamer_boi
	call print_enemy
	call delay_sub
        add word[enemy_x], 2
loop loopEn



;;;;;;;;;;;;;;subroutine to get random numbers;;;from chatgpt
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

    ;;;;;;;;;;;;;;subroutine for enemy in a loop

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
    je found1
    add cx, 160
    cmp cx, 3840
    jne check_left

    mov cx, 158
check_right:                       ;RIGHT SIDE BORDER CHECK
    cmp cx, ax
    je found1
    add cx, 160
    cmp cx, 3998
    jne check_right

    mov cx, 0
check_up:                          ;TOP SIDE BORDER CHECK
    cmp cx, ax
    je found1
    add cx, 2
    cmp cx, 158
    jne check_up

    mov cx, 3840
check_down:                        ;BOTTOM SIDE BORDER CHECK
    cmp cx, ax
    je found1
    add cx, 2
    cmp cx, 4000
    jne check_down
    
    
    mov cx, 664
;;;;;;;; inner borders
;;;;; top-left
check_top_left1:                        ;top-left BORDER CHECK
    cmp cx, ax
    je found1
    add cx, 2
    cmp cx, 694
    jne check_top_left1

jmp cont                       ;;due to error of out of range of found
found1:
 jmp found
cont:

   mov cx, 664
check_top_left2:         
    cmp cx, ax
    je found
    add cx, 160
    cmp cx, 1144
    jne check_top_left2
 
mov cx, 3224
;;;;botton left
check_bottom_left1:                        ;bottom-left BORDER CHECK
    cmp cx, ax
    je found
    add cx, 2
    cmp cx, 3254
    jne check_bottom_left1

   mov cx, 2744
check_bottom_left2:         
    cmp cx, ax
    je found
    add cx, 160
    cmp cx, 3224
    jne check_bottom_left2

 mov cx, 746
;;;;top-right border
check_top_right1:                        ;top-right BORDER CHECK
    cmp cx, ax
    je found
    add cx, 2
    cmp cx, 778
    jne check_top_right1

   mov cx, 776
check_top_right2:         
    cmp cx, ax
    je found
    add cx, 160
    cmp cx, 1416
    jne check_top_right2

mov cx, 3306
;;;;bottom-right border
check_bottom_right1:                        ;bottom-right BORDER CHECK
    cmp cx, ax
    je found
    add cx, 2
    cmp cx, 3336
    jne check_bottom_right1

mov cx, 2856
check_bottom_right2:         
    cmp cx, ax
    je found
    add cx, 160
    cmp cx, 3336
    jne check_bottom_right2


    jmp n_leave
found:                             ;IF pacman TOUCHES BORDER, COME TO "found" AND EXIT
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
    mov ax, 25 ;cx
    push ax
    mov ax, 160 ;add this
    push ax
    mov ax, 0 ;index
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
    
    ;;;;;;;;;;; printing the inner border

    ;;;;; inner left-top border
    mov ax, 15 ;cx
    push ax
    mov ax, 2 ;add this
    push ax
    mov ax, 664 ;dx index value
    push ax
    call border	

    mov ax, 4 ;cx
    push ax
    mov ax, 160 ;add this
    push ax
    mov ax, 664 ;dx index value
    push ax
    call border	
    
    ;;;; inner right-top border
     mov ax, 15 ;cx
    push ax
    mov ax, 2 ;add this
    push ax
    mov ax, 746 ;dx index value
    push ax
    call border	

    mov ax, 4 ;cx
    push ax
    mov ax, 160 ;add this
    push ax
    mov ax, 776 ;dx index value
    push ax
    call border
    
    ;;;;left-left bottom border
    mov ax, 3 ;cx
    push ax
    mov ax, 160 ;add this
    push ax
    mov ax, 2744 ;dx index value
    push ax
    call border
     
    mov ax, 15 ;cx
    push ax
    mov ax, 2 ;add this
    push ax
    mov ax, 3224 ;dx index value
    push ax
    call border
      
    ;;;;left-right bottom bordar
    mov ax, 4 ;cx
    push ax
    mov ax, 160 ;add this
    push ax
    mov ax, 2856 ;dx index value
    push ax
    call border	
    
    mov ax, 15 ;cx
    push ax
    mov ax, 2 ;add this
    push ax
    mov ax, 3306 ;dx index value
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
    mov word[check_pacman], 0 ; Checking death condition
    push word[head_loc]
    push check_pacman
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
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printpacman_right
	    
            ;call enemy_loop
            call gamer_death
            cmp word[check_pacman], 1
            jne game_loop ; if not dead
            cmp word[check_pacman], 1
            je pacman_dead ; if dead

        left:
            dec word[x] ; decrementing x to move left
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printpacman_right
	    ;call enemy_loop
            
	    call gamer_death
            cmp word[check_pacman], 1
            jne game_loop ; if not dead
            cmp word[check_pacman], 1
            je pacman_dead ; if dead

        right:
            inc word[x] ; incrementing x to move right
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printpacman_right
            ;call enemy_loop

            call gamer_death
            cmp word[check_pacman], 1
            jne game_loop ; if not dead
            cmp word[check_pacman], 1
            je pacman_dead ; if dead

        down:
            inc word[y] ; incrementing y to move up
            ;push word [S_len] ; push initial pacman length
            push word[x] ; push x on stack
            push word[y] ; push y on stack
            call printpacman_right
            ;call enemy_loop

            call gamer_death
            cmp word[check_pacman], 1
            jne game_loop ; if not dead
            cmp word[check_pacman], 1
            je pacman_dead ; if dead
            
        cmp word[win_check],1
        je exit_gamer
       
        pacman_dead: ; if dead
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


    
    push word[x] ; push x on stack
    push word[y] ; push y on stack
    call print_pacman

    call print_border
    
    call printFood ; calling the food subroutine
    ;call enemy_loop
    call Gamer_boi

mov ax, 0x4c00 ; terminate program
int 0x21