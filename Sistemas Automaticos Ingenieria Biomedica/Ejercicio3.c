void arrancaTemporizador(int, int);
void esperaTemporizador();
float mideSenal();
void borraPantalla();
void dibujaLinea(int x1, int y1, int x2, int y2);
int main() {
 float medida, filtro;
 float u[5] = {0, 0, 0, 0, 0}, y[5] = {0, 0, 0, 0, 0};
 int periodos = 0;
 float m[4000];
 for (int i = 0; i < 4000; i++) m[i] = 0;
 arrancaTemporizador(0, 1000000);
 while(1) {
 esperaTemporizador();
 periodos++;
 medida = mideSenal();
 filtro = 2.9745 * y[0] - 3.8060 * y[1] + 2.5453 * y[2] - 0.8811 * y[3] + 0.1254 * y[4]
 + 0.0013 * medida + 0.0064 * u[0] + 0.0128 * u[1] + 0.0128 * u[2]
 + 0.0064 * u[3] + 0.0013 * u[4];
 for(int i = 0; i <= 3998; i++)
 m[i] = m[i + 1];
 m[3999] = filtro;
 for(int i = 3; i >= 0; i--) {
 y[i+1] = y[i];
 u[i+1] = u[i];
 }
 y[0] = filtro;
 u[0] = medida;
 if (periodos == 2000) {
 borraPantalla();
 for(int i = 0; i <= 3998; i++)
 dibujaLinea(i * 1023 / 3999, 50 + (m[i] + 3) * (717 - 50) / 6,
 (i + 1) * 1023 / 3999, 50 + (m[i + 1] + 3) * (717 - 50) / 6);
 }
 }
 return 0;
}