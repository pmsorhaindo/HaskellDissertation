import Data.Graph
import Data.Array

squares = array (1,50) [(i,i*i)| i<- [1..50]]

arr1 = array (1,2)[2,7]
{-
2[1,8,3]
3[1,4,9]
4[3,10,5]
5[4,11,6]
6[5,12]
7[1,8,13]
8[2,7,9,14]
9[3,8,10,15]
10[4,9,11,16]
11[5,10,12,17]
12[6,11,18]
13[7,14,19]
14[8,13,15,20]
15[9,14,16,21]
16[10,15,17,22]
17[11,16,18,23]
18[12,17,24]
19[13,20,25]
20[14,19,21,26]
21[15,20,22,27]
22[16,21,23,28]
23[17,22,24,29]
24[18,23,30]
25[19,26,31]
26[20,25,27,32]
27[21,26,28,33]
28[22,27,29,34]
29[23,28,30,35]
30[24,29,36]
31[25,32]
32[26,31,33]
33[27,32,34]
34[28,33,35]
35[29,34,36]
36[30,35]-}