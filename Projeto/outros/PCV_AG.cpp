#include<fstream>
#include<iostream>
#include<cmath>
#include<cstdlib>

using namespace std;

#define TAM_POPULACAO 100

int **geracao;
int **nova_geracao;
float **cidades;
int total_cidades;

void monta_grafo(); // Monta o grafo de cidades

int main() {
	int p=0;

	srand(time(NULL));
	monta_grafo();	


	// Gera a população inicial
	geracao = new int*[TAM_POPULACAO];
	

	for(int i=0;i<TAM_POPULACAO;i++) {
		geracao[i] = new int[total_cidades];

		// Sinaliza que todas as cidades estão disponíveis
		for(int j=0;j<total_cidades;j++) geracao[i][j] = -1;

		// Popula indivíduo
		for(int j=0;j<total_cidades;j++) {
			p = rand() % total_cidades;

			// Aloca a cidade numa posição aleatoriamente
			while(1) {
				if(geracao[i][p] == -1) {
					geracao[i][p] = j;
					break;
				}else{
					p++;
					p %= total_cidades;
				}
			}
		}
	}

	return 0;
}

void monta_grafo() {
	fstream f;
	string buff;
	int **p_cidades;
	int i;

	// Arquivo de cidades
	f.open("cidades.txt");

	// Conta a qtde de cidades no arquivo
	for(total_cidades=0;!f.eof();total_cidades++)
		getline(f,buff);


	// Reseta contador e lê arquivo
	f.clear();
	f.seekg(0,ios::beg);
	p_cidades = new int*[total_cidades];

	for(int i=0;i<total_cidades;i++) {
		p_cidades[i] = new int[2];
		f >> p_cidades[i][0];
		f >> p_cidades[i][1];
		i++;
	}
	

	// Computa a matriz de distancia
	cidades = new float*[total_cidades];

	for(int i=0;i<total_cidades;i++) {
		cidades[i] = new float[total_cidades];

		for(int j=0;j<i;j++) {
			cidades[i][j] = sqrt(pow(p_cidades[i][0]-p_cidades[j][0],2)+pow(p_cidades[i][1]-p_cidades[j][1],2));
		}
	}
}