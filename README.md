# Resumo

Os sistemas embarcados estão cada vez mais presentes em nosso cotidiano. Modelos 
como a Raspberry, Orange Pi, Beaglebone, dentre outros vêm se tornando cada vez mais
populares, principalmente em aplicações domésticas e/ou de baixo custo. Este trabalho
apresenta um comparativo de desempenho entre um computador desktop e uma placa
Raspberry Pi modelo 3B. A fim de avaliar os diferentes tipos de processamento, foi utilizado
para a análise um algoritmo genético, uma vez que seu código possui baixa granularidade,
aplicado na resolução do problema do Caixeiro Viajante. Foram testadas entradas de
5, 10, 15 até 100 cidades, tanto de forma sequencial quanto de forma paralela na placa
e no computador, com a finalidade de verificar o desempenho de cada arquitetura de
acordo com o tamanho da entrada. Uma amostra, de 50 execuções, com entrada fixa de
30 cidades, também foi realizada em cada uma das arquiteturas (sequencial e paralelo)
a fim de verificar aspectos como comportamento ao longo do tempo, entre outros. Os
resultados mostram que o desempenho da Raspberry Pi é inferior ao do computador. Este
já era um resultado esperado visto que se trata de uma arquitetura embarcada que possui
recursos limitados, além de recursos como memória primária e secundária mais lenta que
a de um computador. Entretanto, a placa apresentou melhor eficiência na utilização de
seus núcleos em relação ao computador. Conclui-se portanto, que a Raspberry Pi é uma
alternativa barata e eficiente para sistemas de baixo custo ou que não tenham tempo de
resposta como fator crítico.

## Palavras-chave: Raspberry Pi. Desempenho. Paralelo. Sequencial.
#
#
# Abstract

Embedded systems are increasingly present in our daily lives. Models such as rasp-
berry, orange Pi, Beaglebone, among others have become increasingly popular, especially
in domestic and / or low-cost applications. This work presents a performance comparison
between a desktop computer and a Raspberry Pi model 3B card. In order to evaluate the
different types of processing, a genetic algorithm was used for its analysis, since its code
has low granularity, applied in the problem solving of the Traveling Salesman. Five, 10,
15 to 100 cities entries were tested, both sequentially and in parallel on the board and
without a computer, in order to verify the performance of each architecture according
to the size of the entrance. A sample, of 50 runs, with fixed input of 30 cities, was also
performed in each of the series architectures (sequential and parallel) in order to verify how
behavior over time, among others. The results show that the performance of raspberry Pi
is lower than that of the computer. This was the same expected as an expected problem,
but there are memory resources as well as features such as primary and secondary memory
that are slower than a computer. However, a board had better use of its cores in relation
to the computer. For example, a Raspberry Pi is an inexpensive and efficient alternative
for low-cost systems and has no response time as a critical factor.

## Keywords: Raspberry Pi. Performance. Parallel. Sequential.
