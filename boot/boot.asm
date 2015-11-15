;*****************************************************************************;
;   Boot.asm
;       - Motion OS bootloader
;
;   Jack Atkinson
;*****************************************************************************;

;0x00000000 - 0x000003FF - Real Mode Interrupt Vector Table
;0x00000400 - 0x000004FF - BIOS Data Area
;0x00000500 - 0x00007BFF - Unused, where we will write our krnldrImage
;0x00007C00 - 0x00007DFF - Our Bootloader
;0x00007E00 - 0x0009FFFF - Unused
;0x000A0000 - 0x000BFFFF - Video RAM (VRAM) Memory
;0x000B0000 - 0x000B7777 - Monochrome Video Memory
;0x000B8000 - 0x000BFFFF - Color Video Memory
;0x000C0000 - 0x000C7FFF - Video ROM BIOS
;0x000C8000 - 0x000EFFFF - BIOS Shadow Area
;0x000F0000 - 0x000FFFFF - System BIOS

%define ORGSEG   0x0000
%define ORGOFF   0x7C00
%define BASESEG  0x0000
%define BASEOFF  0x0500
%define FATSEG   0x0070
%define STACKOFF 0xFFFF
%define KERNELBASESEG_R 0x0090
%define KERNELBASEOFF_R 0x0900
%define KERNELBASEOFF_P 0x100000 ; 1MiB
%define NULL     0x00
%define CODE     0x08
%define DATA     0x10

bits    16 ; 16 bit real mode
org     0x0500
Entry:  jmp  short Relocate ; jump to main
nop


;*****************************************************************************;
;   BIOS Parameter Block
;       - Contains information about the drive
;*****************************************************************************;
bpbLabel                db "MotionFS"
bpbBytesPerSector       dw 0x0000
bpbSectorsPerCluster    db 0x00
bpbReservedSectors      dw 0x0000
bpbNumberOfFATs         db 0x00
bpbRootEntries          dw 0x0000    ; N/A for FAT32
bpbTotalSectors         dw 0x0000    ; N/A, partition larger than 32MiB
bpbMedia                db 0x00    ; F8 for fixed
bpbSectorsPerFAT        dw 0x0000    ; N/A for FAT32
bpbSectorsPerTrack      dw 0x0000
bpbHeadsPerCylinder     dw 0x0000
bpbHiddenSectors        dd 0x00000000
bpbBigTotalSectors      dd 0x00000000    ; New 32-bit value of opbTotalSectors
bpbBigSectorsPerFAT     dd 0000000000
bpbExtFlags             dw 0x0000
bpbVersion              dw 0x0000
bpbRootCluster          dd 0x00000000
bpbFSInfoSector         dw 0x0000
bpbBackupSector         dw 0x0000
bpbReserved    times 12 db 0x00
bpbDriveNumber          db 0x00
bpbCurrentHead          db 0x00
bpbExtBootSignature     db 0x00
bpbSerialNumber         dd 0xDEADBEEF
bpbVolumeLabel          db "MotionOS   "
bpbFileSystem           db "FAT32   "


;*****************************************************************************;
;   Relocate()
;       - Relocates the boot loader to BASESEG:BASEOFF
;*****************************************************************************;
Relocate:
    cld ; Make sure direction flag is set to increment si and di
    cli

    mov     ax, ORGSEG
    mov     ds, ax
    mov     ax, BASESEG
    mov     es, ax
    mov     si, ORGOFF
    mov     di, BASEOFF
    mov     cx, 0x0100 ; move stage one, 512 bytes
    rep     movsw
    jmp     BASESEG:Start


;*****************************************************************************;
;   ReadSectors()
;       - Reads 
;   BX    - Number of sectors to read
;   DI:DX - Segment:Offset
;   EAX   - Start sector
;*****************************************************************************;
ReadSectors:
    pusha
    mov     cx, 0x0002 ; 2 retries
  .Read:
    mov     WORD[numberofsectors],     bx ; Number of sectors to read
    mov     WORD[pointertobuffer],     dx ; Offset
    mov     WORD[pointertobuffer + 2], di ; Segment
    mov     DWORD[startsector],        eax ; Starting sector
    mov     DWORD[startsector + 4],    0x00000000
    mov     ah, 0x42 ; Extended Read Sectors From Drive
    mov     si, DAP
    mov     dl, [bpbDriveNumber]
    int     0x13
    jnc     ReadSectors.Success
    dec     bx
    loop    ReadSectors.Read
    jmp     Error
  .Success:
    popa
    ret


;*****************************************************************************;
;   LBAAddress()
;       - Converts the FAT cluster number into a sector number
;   AX => Cluster number         
;*****************************************************************************;
LBAAddress:
    sub     ax, 0x0002
    mul     BYTE[bpbSectorsPerCluster]
    add     ax, WORD[rootcluster]
    ret


;*****************************************************************************;
;   Error()
;       - A critical error occured
;*****************************************************************************;
Error:
    cli
    hlt


;*****************************************************************************;
;   Bootloader Entry Point
;*****************************************************************************;
Start:
	mov		ax, BASESEG
    mov     ds, ax
    mov     ss, ax
    mov     sp, STACKOFF

    mov     ax, FATSEG
    mov     es, ax

    sti ; Stack has been setup so we enable interrupts again

    mov     [bpbDriveNumber], dl ; Save drive number

LoadRootCluster:
    mov     ax, WORD[bpbBigSectorsPerFAT]
    mul     WORD[bpbNumberOfFATs]
    add     ax, WORD[bpbReservedSectors]
    mov     WORD[rootcluster], ax ; Calculate the root cluster
    mov     bx, 0x0001
    mov     di, FATSEG
    xor     dx, dx
    movzx   eax, WORD[rootcluster]
    call    ReadSectors

FindKernel:
    mov     cx, 0x0010 ; 16 entries in one cluster,
    mov     di, 0x0000
  .Next:
    push    cx
    push    di
    mov     cx, 0x000B ; 8.3 filename
    mov     si, kernelname
    rep     cmpsb
    pop     di
    pop     cx
    je      FindKernel.Done
    loop    FindKernel.Next
    jmp     Error
  .Done:
    mov     ax, WORD[es:di + 0x1A]
    mov     WORD[currcluster], ax ; Save starting cluster
    mov     ax, WORD[es:di + 0x1C]
    mov     WORD[kernelsize], ax ; Save file size

LoadFAT:
    mov     bx, 0x0001
    mov     di, FATSEG
    xor     dx, dx
    movzx   eax, WORD[bpbReservedSectors] ; FAT starts right after reserved sectors
    call    ReadSectors

LoadKernel:
    mov     ax, 0x0000
  .More:
    push    ax
    mov     cx, 0x0200
    mul     cx
    mov     dx, ax ; increment the offset by 512 bytes each time
    mov     ax, WORD[currcluster]
    call    LBAAddress
    mov     di, KERNELBASESEG_R
    mov     bx, 0x0001
    call    ReadSectors
    mov     ax, WORD[currcluster]
    mov     cx, 0x0004
    mul     cx
    mov     di, ax
    mov     si, EOFmarker
    push    di ; save di since cmpsb increments it every time it runs
    cmpsd
    pop     di
    je      LoadKernel.Done
    mov     ax, WORD[es:di]
    mov     WORD[currcluster], ax ; Was not EOF, save new cluster
    pop     ax
    inc     ax
    jmp     LoadKernel.More
  .Done:
    pop     ax

InstallGDT:
    pusha
    cli
    lgdt    [GDT] ; load GDT into GDTR
    sti
    popa

EnableA20:
    in      al, 0x92
    or      al, 2
    out     0x92, al

EnterPMode:
    cli ; Can't have interrupts in Protected mode
    mov     eax, cr0
    or      al, 1
    mov     cr0, eax
    jmp     CODE:StartKernel


bits 32
;*****************************************************************************;
;   KernelLoader()
;       - Setups the stack for 32-bit mode and calls the kernel
;*****************************************************************************;
StartKernel:
    mov     ax, DATA       ; set data segments to data selector (0x10)
    mov     ds, ax
    mov     es, ax
    mov     ss, ax
    mov     esp, 0x9FFFF ; Top of useable memory

    ; The following taken from broken thorn
    mov     esi, KERNELBASEOFF_R
    mov     edi, KERNELBASEOFF_P
    movzx   ecx, WORD[kernelsize] ; fixed size dependent on manual change depending on kernel size
    rep     movsb                  ; copy image to its protected mode address
    mov     ebx, DWORD[KERNELBASEOFF_P + 60]
    add     ebx, KERNELBASEOFF_P + 24    ; ebx now points to file sig (PE00)
    mov     eax, [ebx]          ; _IMAGE_FILE_HEADER is 20 bytes + size of sig (4 bytes)
    add     ebx, 16           ; address of entry point
    mov     ebp, dword [ebx]        ; get entry point offset in code section    
    add     ebx, 12             ; image base is offset 8 bytes from entry point
    mov     eax, dword [ebx]        ; add image base
    add     ebp, eax

    call    ebp                       ; Execute Kernel

    cli ; kernel returned
    hlt


GDTData:
  .Null:
    dd 0x00
    dd 0x00
 
  .Code:
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 10011010b
    db 11001111b
    db 0x00
 
  .Data:
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 10010010b
    db 11001111b
    db 0x00
GDTEnd:
GDT: 
    dw GDTEnd - GDTData - 1 ; Size of GDT
    dd GDTData ; Base of GDT

;*****************************************************************************;
;   Disk Address Packet structure
;*****************************************************************************;
DAP:
    size               db 0x10
    unused             db 0x00
    numberofsectors    dw 0x0000
    pointertobuffer    dd 0x00000000
    startsector        dq 0x000000000000

FAT:
    rootcluster dw 0x0000
    currcluster dw 0x0000
    EOFmarker   dd 0x0FFFFFFF

kernelname    db "MKRN32  EXE"
kernelsize    dw 0x0000

times 510-($-$$) db 0x00
BootSignature  dw 0xAA55 ; Boot Signiture