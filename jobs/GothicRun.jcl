//GOTHIC   JOB (001),'RUN GOTHIC',                                      00010000
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=128K            00020000
//*
//* ¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢
//* Above are cent signs (ascii x'A2')
//* w/ code page 819/1047, you add it in VIM with a <ctrl-K> 'C' 't'
//* ¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢¢
//*
//S1      EXEC PGM=GOTHIC                                               00030000
//SYSPRINT  DD SYSOUT=A
//*
//* parallel lines start 'off'
//* '_' EBCDIC x'6D' turns on parallel lines
//* '^' EBCDIC x'5F' EBCDIC not-sign turns off lines, depends on mapping
//* '/' EBCDIC x'61' changes color
//* '#' EBCDIC x'7B' continue to next card
//*
//SYSIN     DD *                                                        00080000
/A
B
_A
B
/_A
B
A
B
/*
//S2      EXEC PGM=GOTHIC
//SYSPRINT  DD SYSOUT=A
//SYSIN     DD *
a^/ b_c
A#
B_b/C
/*
//S3      EXEC PGM=GOTHIC                                               00030000
//SYSPRINT  DD SYSOUT=A
//SYSIN     DD *                                                        00080000
/¢
/.
/(
/&
/!
/$
/)
/;
/-
/,
/?
/:
/'
/"
/A
/B
/C
/D
/E
/F
/G
/H
/I
/J
/K
/L
/M
/N
/O
/P
/Q
/R
/S
/T
/U
/V
/W
/X
/Y
/Z
/a
/b
/c
/d
/e
/f
/g
/h
/i
/j
/k
/l
/m
/n
/o
/p
/q
/r
/s
/t
/u
/v
/w
/x
/y
/z
/0
/1
/2
/3
/4
/5
/6
/7
/8
/9
¢
.
(
&
!
$
)
;
-
,
?
:
'
"
A
B
C
D
E
F
G
H
I
J
K
L
M
N
O
P
Q
R
S
T
U
V
W
X
Y
Z
a
b
c
d
e
f
g
h
i
j
k
l
m
n
o
p
q
r
s
t
u
v
w
x
y
z
0
1
2
3
4
5
6
7
8
9
/*
