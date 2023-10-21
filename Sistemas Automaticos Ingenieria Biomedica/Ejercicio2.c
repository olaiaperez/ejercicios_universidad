#include <stdio.h>
#include <imagen.h>
#include <math.h>
typedef struct {
 float xc, yc;
 int area;
 float L;
} Objeto;
Objeto datos[10];
int nObjetos = 0;
int main()
{
 for(int i = 0; i < 640*480; i++)
 if (m[i] < 100)
 m[i] = 0;
 else m[i] = 255;

 for(int f = 1; f < 480; f++)
 for(int c = 0; c < 640; c++) {
 if(m[f * 640 + c] == 0) {
 int ci = c;
 while(m[f * 640 + c] == 0)
 c++;
 int cf = c - 1;
 int minimo = 255;
 for(int j = ci; j <= cf; j++)
 if(m[(f-1)*640+j] < minimo)
 minimo = m[(f-1)*640+j];
 if (minimo == 255) {
 nObjetos ++;
for(int j = ci; j <= cf; j++)
 m[f*640+j] = nObjetos;
 } else {
 for(int j = ci; j <= cf; j++)
 m[f*640+j] = minimo;
 }
 }
 }


 for(int n = 0; n < nObjetos; n++) {

 datos[n].xc = 0;
 datos[n].yc = 0;
 datos[n].area = 0;
 for(int f = 0; f < 480; f++)
 for(int c = 0; c < 640; c++) {
 if(m[f * 640 + c] == n+1) {
 datos[n].xc += c;
 datos[n].yc += f;
 datos[n].area ++;
 }
 }
 datos[n].xc /= datos[n].area;
 datos[n].yc /= datos[n].area;

 datos[n].L = 0;
 for(int f = 0; f < 480; f++)
 for(int c = 0; c < 640; c++) {
 if(m[f * 640 + c] == n+1) {
 float d = sqrt((datos[n].xc - c)*(datos[n].xc - c) +
 (datos[n].yc - f)*(datos[n].yc - f));
 if (d > datos[n].L)
 datos[n].L = d;
 }
 }
 datos[n].L *= 2;
 }

 return 0;
}