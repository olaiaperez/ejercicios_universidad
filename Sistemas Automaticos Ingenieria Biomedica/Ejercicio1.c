int main()
{
 int ordenes[] = { 5, 3, 2, 6, 3, 4, 1, 6, 4, 8 }; // Número de órdenes ejecutadas por cada máquina
 int libres[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; // Buleanos que indican qué máquinas están libres
 int nMaquinas = 10; // Número de máquinas existentes
 int i, maxOrdenes, seleccionada;
 maxOrdenes = 1000000;
 seleccionada = -1;
 for(i = 0; i < nMaquinas; i++)
 if (libres[i] && ordenes[i] < maxOrdenes) {
 maxOrdenes = ordenes[i];
 seleccionada = i;
 }
 if (seleccionada >= 0)
 seleccionada ++;

 return 0;
}