# Arithmetic-expression

.model small 
.stack 100h 

.data 
MaxLen equ 38          ;максимальная длина строки = 79   
Buffer db MaxLen + 2 dup(?)  ;буфер строки 
BufferC db MaxLen + 4 dup(?)                   
Str1 db 'Virozenie: $'
Str2 db 0dh,0ah,'False!',0dh,0ah ,'$' 
Str3 db 0dh,0ah,'True!',0dh,0ah ,'$' 
Signs db '+-/$' 
ScobO db '($'
ScobZ db ')$'
ErrorNum db 0
;сдвиг для сообщений 32h
Shift db 32h
Error0 db 0dh,0ah,'Error. 2 and more spaces.',0dh,0ah,'$                    ' 
Error1 db 0dh,0ah,'Error. A space in variable.',0dh,0ah,'$                  ' 
Error2 db 0dh,0ah,'Error. The brackets have an invalid position.',0dh,0ah,'$'
Error3 db 0dh,0ah,'Error. A closing brackets without an opening.',0dh,0ah,'$'
Error4 db 0dh,0ah,'Error. A opening brackets without an closing.',0dh,0ah,'$' 
Error5 db 0dh,0ah,'Error. A string has double signs.',0dh,0ah,'$            '
Error6 db 0dh,0ah,'Error. A string has invalid characters.',0dh,0ah,'$      '
Error7 db 0dh,0ah,'Error. An invalid constant.',0dh,0ah,'$                  ' 
Error8 db 0dh,0ah,'Error. An invalid variable.',0dh,0ah,'$                  '
Error9 db 0dh,0ah,'Error. The length variable is greater than 5.',0dh,0ah,'$' 
;-----------------------------------------------------------------;
printDX MACRO     ;вывод строки по адресу DX
 mov ah,9         ;-   
 int 21h          ;-
ENDM              ;-
 
;-----------------------------------------------------------------;
.code 
code1:  
 ;-------------------Настройки------------------------------------;
 mov ax,@data        ;сегмент данных в DS,ES
 mov ds,ax           ;-
 mov es,ax           ;-
 
 ;-------------------Ввод выражения-------------------------------;        
 lea dx, Str1             ;вывод подсказки
 printDX                  ;-
 mov Buffer, MaxLen       ;ввод строки в память по адресу [Buffer + 2]                
 lea dx, Buffer           ;[Buffer] = максимально возможная длина строки
 mov ah, 0ah              ;[Buffer+1] = реальная длина строки
 int 21h                  ;- 
 mov ax, 0600h            ;очистка экрана
 mov bh, 07h              ;
 mov cx, 0000h            ;
 mov dx, 184Fh            ;
 int 10h                  ;
 mov ax,3                 ;курсор в начало
 int 10h                  ;
 
 ;--------Добавление конца строки---------------------------------;
 lea bx, Buffer            ;увелечение реальной длины на 2
 inc bx                    ;-
 mov cl, [bx]              ;-
 add cl, 2                 ;-
 mov byte ptr[bx], cl      ;-
 add bl, cl                ;добавление в конец *$ (конец строки)
 dec bx                    ;-
 mov byte ptr[bx], '*'     ;-
 inc bx                    ;-
 mov byte ptr[bx], '$'     ;указание конца строки
 
 ;--------Копирование выражения----------------------------------;
 cld                       ;сброс флагов          
 lea bx, Buffer            ;счетчик CX = рельная длина строки + 2
 add bx, 1                 ;-
 mov cl, [bx]              ;-
 mov cx, 2                 ;-
 lea si, Buffer            ;SI = откуда  
 lea di, BufferC           ;DI = куда  
 rep movsb                 ;пересылка данных
 mov byte ptr[di],'*'      ;ограничение строки снизу
 inc di                    ;-
 mov cl, [bx]              ;-
 rep movsb                 ;-
 sub si,2                  ;-
 mov byte ptr[si],'$'      ;-
 
 ;--------Замена операций на '*'---------------------------------;
 mov cx, 3                 ;счетчик CX = количество заменяемых знаков
 lea bx, Signs             ;BX = указ. на знаки
 mov dl, '*'               ;DL = конечный знак
 cycle1Repl:               ;цикл по знакам
  lea si, BufferC          ;[SI] = указ. на символ строки
  add si, 2                ;
  cycle2Repl:              ;цикл по строке
   mov al, [bx]            ;сравнение символа со знаком
   cmp [si],al             ;в случае совпадения - 
   jnz passRepl            ;замена на конечный знак
    xchg [si],dl           ;-
    mov dl, '*'            ;-
   passRepl:                ;-
   inc si                  ;-
   cmp WORD PTR[si],'$'    ;-
  jne cycle2Repl            ;-
  inc bx                   ;-
 loop cycle1Repl            ;-

;--------Проверка и удаление пробелов---------------------------;
 lea bx, BufferC           ;счетчик CX = реальная длина строки
 inc bx                    ;-
 mov cl, [bx]              ;-                  
 lea si, BufferC           ;SI = указ. на начальную строку 
 add si, 3                 ;DI = указ. на конечная строку
 mov di, si                ;-
 mov bl, 0                 ;BL = счетчик символов в конечной строке 
 mov bh, 0                 ;BH = колличество вподряд идущих пробелов
 spaceDel:                 ;-     
  cmp byte ptr[si], ' '    ;проверка на пробел
  jne notSpace             ;-
   inc bh                  ;увеличение количества подр. идущих пробелов
   cmp bh, 1               ;проверка на 2 подр. идущих пробела
   jna falseSpace1         ;-
    lea si, ErrorNum       ;вызов ошибки
    mov byte ptr[si], 0    ;-
    jmp error              ;-
   falseSpace1:            ;-
                           ;(пробел в переменной)
   push bx                 ;-
   mov bx, 0               ;BX = проверка лат. симв. 
   mov al, [di - 1]        ;AL = предшествующий символ 
   mov ah, [si + 1]        ;AH = последующий символ
    cmp al, 'a'            ;проверка на лат. символ перед и после
    jb falseSpaceS1        ;-
    cmp al, 'z'            ;-
    ja falseSpaceS1        ;-
    inc bx                 ;есть лат.символ
    jmp falseSpacePre      ;-
    falseSpaceS1:          ;-
     cmp al, 'A'           ;-
     jb falseSpacePre      ;-
     cmp al, 'Z'           ;-
     ja falseSpacePre      ;-
     inc bx                ;есть лат.символ
    falseSpacePre:         ;-
    cmp ah, 'a'            ;-
    jb falseSpaceS2        ;-
    cmp ah, 'z'            ;-
    ja falseSpaceS2        ;-
    inc bx                 ;есть лат.символ
    falseSpaceS2:          ;-
     cmp ah, 'A'           ;-
     jb falseSpaceNex      ;-
     cmp ah, 'Z'           ;-
     ja falseSpaceNex      ;-
     inc bx                ;есть лат.символ
   falseSpaceNex:          ;-
   cmp bx, 2               ;проверка на лат.символ с обеих сторон
   jne falseSpace2         ;-
    lea si, ErrorNum       ;вызов ошибки
    mov byte ptr[si], 1    ;-
    jmp error              ;-
   falseSpace2:            ;-
   pop bx                  ;-
   inc si                  ;переход на след символ(в начальной строке)
   jmp spaceNext           ;-
  notSpace:                ;-
  mov bh, 0                ;обнуление количества подр. идущих пробелов
  cmp si, di               ;проверка на необходимость сдвига символов
  je spaceNotSif           ;-
   mov al, [di]            ;сдвиг символов
   xchg byte ptr[si], al   ;-
   mov byte ptr[di], al    ;-
  spaceNotSif:             ;-
  inc di                   ;переход на след символ(в обеих строках)
  inc si                   ;-
  inc bl                   ;-
  spaceNext:               ;-
 loop spaceDel             ;-
 lea si, BufferC           ;перезапись реальной длины строки
 inc si                    ;-
 mov byte ptr[si], bl      ;-
 
 ;--------Проверка и удаление скобок-----------------------------;
 lea bx, BufferC           ;счетчик CX = реальная длина строки
 inc bx                    ;-
 mov cl, [bx]              ;-                  
 lea si, BufferC           ;SI = указ. на начальную строку 
 add si, 3                 ;DI = указ. на конечная строку
 mov di, si                ;-
 mov bl, 1                 ;BL = счетчик незакрытых скобок 
 mov bh, 0                 ;BH = счетчик символов строке
 bracket:                  ;цикл строки
  cmp byte ptr[si], '('    ;проверка на открывающуюся скобку
  jne bracketNotOpen       ;-
   inc bl                  ;увеличение количества незакрытых скобок
   mov al, [di - 1]        ;проверка предшествуещего символа
   cmp al, "*"             ;-
   jne bracketError1       ;-
   jmp bracket1            ;-
   bracketError1:          ;-
    lea si, ErrorNum       ;вызов ошибки, если он не '*'
    mov byte ptr[si], 2    ;-
    jmp error              ;-
   bracket1:               ;-
   inc si                  ;переход на след символ(в начальной строке)
   jmp bracketNext         ;-
  bracketNotOpen:          ;-
  cmp byte ptr[si], ')'    ;проверка на закрывающуюся скобку
  jne bracketNotClose      ;-
   dec bl                  ;уменьшение количества незакрытых скобок
   mov al, [di - 1]        ;проверка предшествующего и последующих символов
   mov ah, [si + 1]        ;-
   cmp al, "*"             ;-
   je bracketError2        ;-
   cmp ah, ")"             ;-
   je bracket2             ;-
   cmp ah, "*"             ;-
   je bracket2             ;-
    bracketError2:         ;-
    lea si, ErrorNum       ;вызов ошибки, если предшествующий '*'
    mov byte ptr[si], 2    ;или последующий(за всеми закрыющимися скобками)
    jmp error              ;не '*'
   bracket2:               ;-
   inc si                  ;переход на след символ(в начальной строке)
   jmp bracketNext         ;-
  bracketNotClose:         ;-
  cmp si, di               ;проверка на необходимость сдвига символов
   je bracketNotSif        ;-
   mov al, [di]            ;сдвиг символов
   xchg byte ptr[si], al   ;-
   mov byte ptr[di], al    ;-
  bracketNotSif:           ;-
  inc di                   ;переход на след символ(в обеих строках)
  inc si                   ;-
  inc bh                   ;- 
  bracketNext:             ;-
  cmp bl,1                 ;проверка на закрывающую скобку без открывающей
  jnb bracketNext1         ;-
    lea si, ErrorNum       ;вызов ошибки
    mov byte ptr[si], 3    ;-
    jmp error              ;-
  bracketNext1:            ;-
 loop bracket              ;-
 cmp bl,1                  ;проверка на скобки без пары
 je bracket3               ;-
  lea si, ErrorNum         ;вызов ошибки
  mov byte ptr[si], 4      ;-
  jmp error                ;-
 bracket3:                 ;-
 lea si, BufferC           ;перезапись реальной длины строки
 inc si                    ;-
 mov byte ptr[si], bh      ;-
 
 ;--------Проверка на недопустимые символы,------------------------;
 ;--------контанты и переменные------------------------------------;
 lea di, BufferC  ;di - указатель на строку
 inc di     ;
 mov cx, [di]     ;cx - количество символов
 add di, 2
 
check:
 cmp byte ptr[di],'$'
 je good  
 push cx
 push di
 cld
 mov al,'*'
 repne scasb
 pop di
 pop ax
 push ax
 dec ax
 sub ax, cx 
 cmp ax,0
 jne notDoubleSigns
  lea si, ErrorNum         ;вызов ошибки
  mov byte ptr[si], 5      ;-
  jmp error
 notDoubleSigns:
 mov bl,[di]
 cmp bl, '0'
 jb invalidSymbol
 cmp bl, '9'
 jbe checkNum
 cmp bl, 'A'
 jb invalidSymbol
 cmp bl, 'Z'
 jbe checkVar
 cmp bl, 'a'
 jb invalidSymbol
 cmp bl, 'z'
 jbe checkVar 
invalidSymbol:
 lea si, ErrorNum         ;вызов ошибки
 mov byte ptr[si], 6      ;-
 jmp error

good:              ;вывод строки по адресу str1
 lea dx, Buffer 
 add dx,2
 printDX                ;вывод строки по адресу str1
 lea dx, Str3       ;
 printDX           ;вывод текста 
 jmp endProg 
 
checkNum:
 mov cx, ax
 dec cx
 cmp cx,0
 je checkNumCyclePas
 checkNumCycle:
  inc di
  mov bl,[di]
  cmp bl, '0'
  jb invalidNum
  cmp bl, '9'
  ja invalidNum
 loop checkNumCycle
 checkNumCyclePas:
 pop cx
 sub cx, ax
 add di, 2
 jmp check
 
checkVar:
 mov cx, ax
 dec cx
 cmp cx,0
 je checkVarCyclePas
 checkVarCycle:
  inc di
  mov bl,[di]
  cmp bl, 'A'
  jb invalidVar
  cmp bl, 'Z'
  jbe checkVar1
  cmp bl, 'a'
  jb invalidVar
  cmp bl, 'z'
  ja invalidVar
  checkVar1:
 loop checkVarCycle
 cmp ax,5
 ja invalidVarSize
 checkVarCyclePas:
 pop cx
 sub cx, ax
 add di, 2
 jmp check
 

 
invalidNum:
 lea si, ErrorNum         ;вызов ошибки
 mov byte ptr[si], 7      ;-
 jmp error
 
invalidVar: 
lea si, ErrorNum         ;вызов ошибки
 mov byte ptr[si], 8      ;-
 jmp error
 
invalidVarSize:
 lea si, ErrorNum         ;вызов ошибки
 mov byte ptr[si], 9      ;-
 jmp error 
 
error:
 lea dx, Buffer 
 add dx,2
 printDX
 mov ax, 0
 lea di, Shift
 mov al, [di]
 mul ErrorNum
 lea dx, Error0
 add dx, ax
 printDX
 
endProg:
 mov ah,4ch
 int 21h
 
end code1
