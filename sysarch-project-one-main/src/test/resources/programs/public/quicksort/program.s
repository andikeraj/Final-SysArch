addi sp, sp, 252
addi a0, zero, 0
addi a1, zero, 0
addi a2, zero, 6
jal ra, 8
jal ra, 252
addi sp, sp, -20
sw ra, 16(sp)
sw s3, 12(sp)
sw s2, 8(sp)
sw s1, 4(sp)
sw s0, 0(sp)
addi s0, a0, 0
addi s1, a1, 0
addi s2, a2, 0
BLT a2, a1, 44
jal ra, 68
addi s3, a0, 0
addi a0, s0, 0
addi a1, s1, 0
addi a2, s3, -1
jal ra, -60
addi a0, s0, 0
addi a1, s3, 1
addi a2, s2, 0
jal ra, -76
lw s0, 0(sp)
lw s1, 4(sp)
lw s2, 8(sp)
lw s3, 12(sp)
lw ra, 16(sp)
addi sp, sp, 20
jalr zero, ra, 0
addi sp, sp, -4
sw ra, 0(sp)
slli t0, a2, 2
add t0, t0, a0  
lw t0, 0(t0)
addi t1, a1, -1
addi t2, a1, 0
BEQ t2, a2, 56
slli t3, t2, 2
add a6, t3, a0
lw t3, 0(a6)
addi t0, t0, 1
BLT t0, t3, 28
addi t1, t1, 1
slli t5, t1, 2
add a7, t5, a0
lw t5, 0(a7)
sw t5, 0(a6)
sw t3, 0(a7)
addi t2, t2, 1
beq zero, zero, -52
addi t5, t1, 1
addi a5, t5, 0
slli t5, t5, 2
add a7, t5, a0
lw t5, 0(a7)
slli t3, a2, 2
add a6, t3, a0
lw t3, 0(a6)
sw t5, 0(a6)
sw t3, 0(a7)
addi a0, a5, 0
lw ra, 0(sp)
addi sp, sp, 4
jalr zero, ra, 0
nop
beq zero, zero, -4