        ;
        ; Invaders in 512 bytes
        ;
        ; by Oscar Toledo G.
        ;
        ; (c) Copyright 2015-2019 Oscar Toledo G.
        ;
        ; Creation: Oct/27/2015.
        ; Revision: Nov/06/2015. Adjusted bullet collision. Invaders
        ;                        accelerate.
        ; Revision: Apr/03/2019. Invaders now can shoot. Spaceship does
        ;                        explosion.
        ; Revision: May/28/2019. Invaders goes down at 11px instead 12px.
        ;                        Now starts another invaders wave more
        ;                        difficult.
        ; Revision: Jun/01/2019. Redesigned for 320x200x256 mode.
        ; Revision: Jun/02/2019. Now in color. Color carries information
        ;                        about thing being hit.
        ; Revision: Jun/03/2019. Optimized, 601 bytes as COM!!!
        ; Revision: Jun/04/2019. At last 512 bytes!
        ; Revision: Jun/05/2019. By popular demand added pure8088 option.
        ;

        ;
        ; The usage of PUSHA and POPA means it requires 80286 or higher
        ;

    %ifndef pure8088            ; If not defined create a 80286 binary
pure8088:       equ 0           ; Enable this for pure 8088, caveat! it
                                ; will exceed 512 bytes.
    %endif

    %ifndef com_file            ; If not defined create a boot sector
com_file:       equ 0
    %endif

base:           equ 0xfc80      ; Memory base (same segment as video)

shots:          equ base+0x00   ; Space to contain 4 shots (2 bytes each one)
                                ; Plus space for a ignored shot (full table)
                                ; Notice (sprites + SPRITE_SIZE) - (shots + 2)
                                ; must be divisible by SPRITE_SIZE.
xdir:           equ base+0x0a   ; X direction of player
old_time:       equ base+0x0c   ; Old time
level:          equ base+0x10   ; Current level number
lives:          equ base+0x11   ; Current lives
sprites:        equ base+0x12   ; Space to contain sprite table

SHIP_ROW:       equ 0x5c*OFFSET_X       ; Row of spaceship
X_WIDTH:        equ 0x0140      ; X-width of video
OFFSET_X:       equ X_WIDTH*2   ; X-offset between screen rows (2 pixels)
SPRITE_SIZE:    equ 4           ; Size of each sprite in bytes

        ;
        ; All colors different (important to distinguish things)
        ;
SPACESHIP_COLOR:        equ 0x1c        ; Must be below 0x20
SHIP_EXPLOSION_COLOR:   equ 0x0a
INVADER_EXPLOSION_COLOR:        equ 0x0e
BULLET_COLOR:           equ 0x0c
START_COLOR:    equ ((sprites+SPRITE_SIZE-(shots+2))/SPRITE_SIZE+0x20)        

    %if com_file
        org 0x0100
    %else
        org 0x7c00
    %endif
        mov ax,0x0013   ; Set mode 0x13 (320x200x256 VGA)
        int 0x10        ; Call BIOS
        cld
        mov ax,0xa000
        mov ds,ax
        mov es,ax
        mov ah,0x04
        mov [level],ax  ; Level = 0, Lives = 4
restart_game:
        xor ax,ax
        mov cx,level/2  ; Clear screen and variables (except level/lives)
        xor di,di
        rep
        stosw           ; ch is zero from here

        ;
        ; Setup descend state
        ;
        mov ax,[di]     ; al now contains level, ah contains lives
        cmp al,0x20     ; Maximum level?
        je in36         ; Yes, don't increase
        inc ax          ; Increase by 2 (so invaders descend correctly)
        inc ax
in36:   stosw           ; Advance level
        mov ah,al
        xchg ax,dx      ; Shouldn't damage DX starting here

        ;
        ; Setup the spaceship
        ;
        mov ax,SPACESHIP_COLOR*0x0100+0x00
        stosw
        mov ax,SHIP_ROW+0x4c*2
        stosw
        ;
        ; Setup the invaders
        ;
        mov ax,0x08*OFFSET_X+0x28
        mov bx,START_COLOR*0x0100+0x10
in1:    mov cl,0x0b             ; Eleven invaders per row
in5:    stosw
        add ax,0x0b*2
        xchg ax,bx
        stosw
        inc ah
        xchg ax,bx
        loop in5                ; Loop and also makes sure ch is zero
        add ax,0x09*OFFSET_X-0x000b*0x000b*2    ; Go to next row
        cmp bh,START_COLOR+55   ; Whole board finished?
        jne in1                 ; No, jump

        ; CH is zero

in14:
        mov si,sprites+SPRITE_SIZE

        ;
        ; Game loop
        ;
        ; Globals:
        ; SI = Next invader to animate
        ; DL = state (0=left, 1=right, >=2 down)
        ; DH = nstate (next state)
        ; CH = dead invaders
        ; BP = frame counter
        ;
in46:
        cmp byte [si+2],0x20    ; Is it cosmic debris?
        jc in2                  ; No, jump
        inc ch
        cmp ch,55               ; All invaders defeated?
        je restart_game         ; Yes, jump.
        ;
        ; Yes, invaders speed up
        ;
in6:
        lodsw                   ; Load coordinates in ax
        xchg ax,di
        lodsw                   ; Get type of sprite
        cmp al,0x28             ; Destroyed?
        je in27                 ; Yes, jump
        cmp al,0x20             ; Explosion?
        jne in29                ; No, jump
        mov byte [si-2],0x28    ; Don't draw again
in29:   call draw_sprite
in27:   cmp si,sprites+56*SPRITE_SIZE     ; Whole board revised?
        jne in46                ; No, jump
        mov al,dh
        sub al,2                ; Going down?
        jc in14                 ; No, preserve left/right direction
        xor al,1                ; Switch direction
        mov dl,al
        mov dh,al
        jmp in14

in2:
        xor byte [si+2],8       ; Invader animation (before possible explosion)
        ;
        ; Synchronize game to 18.20648 hz. of BIOS
        ;
        inc bp
        and bp,7                ; Each 8 invaders
    %if pure8088
        push cx
        push dx
        push si
        push bp
    %else
        pusha
    %endif
        jne in12
in22:
        mov ah,0x00           
        int 0x1a                ; BIOS clock read
        cmp dx,[old_time]       ; Wait for change
        je in22
        mov [old_time],dx
in12:
    %if 1
        ;
        ; Handle player bullet
        ;
        mov si,shots                    ; Point to shots list
        mov cx,4                        ; 4 shots at most
        lodsw                           ; Read position (player)
        cmp ax,X_WIDTH                  ; Is it at top of screen?
        xchg ax,di
        jc in31                         ; Erase bullet
                                        ; Doesn't mind doing it all time
        call zero                       ; Remove bullet 
        sub di,X_WIDTH+2
        mov al,[di]                     ; Read pixel
        sub al,0x20                     ; Hits invader?
        jc in30                         ; No, jump
    %if pure8088
        push si
        push di
    %else
        pusha
    %endif
        mov ah,SPRITE_SIZE              ; The pixel indicates the...
        mul ah                          ; ...invader hit.
        add si,ax
        lodsw
        xchg ax,di
        mov byte [si],0x20              ; Erase next time
        mov ax,INVADER_EXPLOSION_COLOR*0x0100+0x08      ; But explosion now
        call draw_sprite
    %if pure8088
        pop di
        pop si
    %else
        popa
    %endif
        jmp in31

        ;
        ; Handle invader bullets
        ;
in24:
        lodsw                           ; Read current coordinate
        or ax,ax                        ; Is it falling?
        je in23                         ; No, jump
        cmp ax,0x60*OFFSET_X            ; Pixel lower than spaceship?
        xchg ax,di
        jnc in31                        ; Yes, remove bullet
        call zero                       ; Remove bullet 
        add di,X_WIDTH-2                ; Bullet falls down

        ; Draw bullet
in30:
        mov ax,BULLET_COLOR*0x0100+BULLET_COLOR
        mov [si-2],di
        jmp in7

        ; Remove bullet
in31:   xor ax,ax
        mov [si-2],ax                   ; AX contains zero

in7:    cmp byte [di],SPACESHIP_COLOR   ; Check for crash against player
        jne in41                        ; No, jump
        mov word [sprites],SHIP_EXPLOSION_COLOR*0x0100+0x38
in41:
        call big_pixel
in23:   loop in24
    %endif

        ;
        ; Spaceship handling
        ;
        mov ah,0x01                     ; BIOS Key available
        int 0x16
        mov ah,0x00                     ; BIOS Read Key
        je in25
        int 0x16
in25:   mov ch,ah                       ; New scancode key in CH or zero.

        mov si,sprites                  ; Point to spaceship
        lodsw
        or al,al                        ; Explosion?
        je in42                         ; No, jump
        add al,0x08                     ; Keep explosion
        jne in42                        ; Finished? No, jump
        mov ah,SPACESHIP_COLOR          ; Restore color (sprite already)
        dec byte [lives]                ; Remove one life
        js in10                         ; Exit if all used
in42:   mov [si-2],ax
        mov di,[si]
        call draw_sprite                ; Draw spaceship
        jne in43                        ; Jump if still explosion

        mov al,[xdir]                   ; Current X-direction

        cmp ch,0x39                     ; Space key?
        jne in35                        ; No, jump
        xor ax,ax
        cmp ax,[shots]                  ; Bullet available?
        jne in35                        ; No, jump
        lea bx,[di+(0x04*2)]            ; Offset from spaceship
        mov [shots],bx                  ; Start bullet
in35:
        cmp ch,0x4b                     ; Left key?
        jne in17                        ; No, jump
        mov al,-2

in17:   cmp ch,0x4d                     ; Right key?
        jne in18                        ; No, jump
        mov al,2
in18:
        mov [xdir],al                   ; Save direction
        cbw
        add ax,di                       ; Move spaceship
        cmp ax,SHIP_ROW-2               ; Update only if not exceeded border
        je in43
        cmp ax,SHIP_ROW+0x0132
        je in43
in19:   mov [si],ax
in43:
    %if pure8088
        pop bp
        pop si
        pop dx
        pop cx
    %else
        popa
    %endif

        mov ax,[si]
        cmp dl,1                ; Going down (state 2)?
        jbe in9                 ; No, jump
        add ax,0x0280
        cmp ax,0x55*0x280       ; Reaches Earth?
        jc in8                  ; No, jump
in10:
    %if com_file
        mov ax, 3
        int 0x10
        int 0x20
    %else
        jmp $
    %endif

in9:    dec ax                  ; Moving to left
        dec ax
        jc in20
        add ax,4                ; Moving to right
in20:   push ax
        shr ax,1
        mov cl,0xa0
        div cl
        dec ah
        cmp ah,0x94             ; Border touched?
        pop ax
        jb in8                  ; No, jump
        or dh,22                ; Goes down by 11 pixels (11 * 2) must be odd
in8:    mov [si],ax
        add ax,0x06*0x280+0x03*2        ; Offset for bullet
        xchg ax,bx

        mov cx,3        ; ch = 0 invader alive
        in al,(0x40)
        cmp al,0xfc
        jc in4
        ;
        ; Doesn't work in my computer:
        ;
        ; mov di,shots+2
        ; xor ax,ax
        ; repne scasw
        ; mov [di-2],bx
        ;
        mov di,shots+2
in45:   cmp word [di],0
        je in44
        scasw
        loop in45
in44:
        mov [di],bx   ; Start invader shot
in4:
        jmp in6

bitmaps:
        db 0x18,0x18,0x3c,0x24,0x3c,0x7e,0xFf,0x24      ; Spaceship
        db 0x00,0x80,0x42,0x18,0x10,0x48,0x82,0x01      ; Explosion
        db 0x00,0xbd,0xdb,0x7e,0x24,0x3c,0x66,0xc3      ; Alien (frame 1)
        db 0x00,0x3c,0x5a,0xff,0xa5,0x3c,0x66,0x66      ; Alien (frame 2)
        db 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00      ; Erase

        ;
        ; Draw 1 pixel inside reg. al (bit 7)
        ;
bit:    mov ax,dx
        jc big_pixel
zero:   xor ax,ax
big_pixel:
        mov [di+X_WIDTH],ax
        stosw
        ret

        ; ah = sprite color
        ; al = sprite (x8)
        ; di = Target address
draw_sprite:
    %if pure8088
        push ax
        push bx
        push cx
        push dx
        push di
        pushf
    %else
        pusha
    %endif
        mov dl,ah
        mov dh,ah
in3:    push ax
        mov bx,bitmaps
        cs xlat                 ; Extract one byte from bitmap
        xchg ax,bx
        mov cx,10               ; Two extra zero pixels at left and right
        clc                     ; Left pixel as zero (clean)
in0:    call bit                ; Draw pixel
        shl bl,1
        loop in0
        add di,OFFSET_X-20      ; Go to next video line
        pop ax
        inc ax                  ; Next bitmap byte
        test al,7               ; Sprite complete?
        jne in3                 ; No, jump
    %if pure8088
        popf
        pop di
        pop dx
        pop cx
        pop bx
        pop ax
    %else
        popa
    %endif
        ret

    %if pure8088
    %else
        db 'OT'			; Rounding
    %endif
    %if com_file
    %else
        db 0x55,0xaa            ; Make it a bootable sector
    %endif
