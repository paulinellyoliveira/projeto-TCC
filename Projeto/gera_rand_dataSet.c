/*#include <conio.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
    FILE *arq;
    arq = fopen("sample_inpu","w");

    if(arq == NULL){
       printf("Erro na abertura do arquivo!");
       return 1;
    }

    int i, x, y;


    srand((unsigned)time(NULL));

    for(i=1 ; i <= 95 ; i++){
        x = 1 + (rand()%10000);
	y = 1 + (rand()%10000);

	  /*usando fprintf para armazenar a string no arquivo*/
	  fprintf(arq,"%\d %\d %\d\n", i, x, y);
    }
        

    fclose (arq);
    return 0; 
}
