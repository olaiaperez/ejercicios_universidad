void autoajustePID(float actuacion, float * Kp, float * Ki, float * Kd) {
 float P[5], A[5], mediaP, mediaA;
 int nPeriodos = 0;

 salidaAnalogica(actuacion);
 while(entradaAnalogica(1) < 0.5);
 salidaAnalogica(-actuacion);
 while(entradaAnalogica(1) > 0);

 int finalizar = 0;
 while(! finalizar) {
 MS = 0;
 salidaAnalogica(actuacion);

 float minimo = 0, y;
 do {
 y = entradaAnalogica(1);
 if (y < minimo)
 minimo = y;
 } while(y < 0);

 salidaAnalogica(-actuacion);

 float maximo = 0;
 do {
 y = entradaAnalogica(1);
 if (y > maximo)
 maximo = y;
 } while(y > 0);

 if (nPeriodos < 5) {
 P[nPeriodos] = MS / 1000.0;
 A[nPeriodos] = maximo - minimo;
 nPeriodos ++;
 } else {
 for(int i = 0; i <= 3 ; i++) {
 P[i] = P[i+1];
 A[i] = A[i+1];
 }
 P[4] = MS / 1000.0;
 A[4] = maximo - minimo;
 }

 if (nPeriodos == 5) {
 mediaP = 0;
 mediaA = 0;
 for(int i = 0; i < 5; i++) {
 mediaP += P[i];
 mediaA += A[i];
 }
 mediaP /= 5;
 mediaA /= 5;
 finalizar = 1;
 for(int i = 0; i < 5; i++) {
 if (fabs(P[i] - mediaP) > 0.02 * mediaP)
 finalizar = 0;
 if (fabs(A[i] - mediaA) > 0.02 * mediaA)
 finalizar = 0;
 }
 }
 }
 float Ku = 4 * 5 / 3.1415 / mediaA;
 *Kp = 0.6 * Ku;
 *Ki = 1.2 * Ku / mediaP;
 *Kd = 0.075 * Ku;
}

int main()
{
 float Kp, Ki, Kd;

 autoajustePID (5,& Kp,& Ki,& Kd);

 return 0;
}