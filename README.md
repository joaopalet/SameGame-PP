# Informação sobre utilização

No ficheiro ```procura.lisp``` estão definidos alguns algoritmos de
procura para serem utilizados na cadeira de Técnicas de Procura.

Estão implementados algoritmos de procura não-informada e algoritmos
de procura informada.

#### Os algoritmos de procura não-informada implementados são os seguintes:
- Procura em largura primeiro
- Procura em profundidade primeiro
- Procura em profundidade iterativa

####  Os algoritmos de procura informada implementados foram:
- Procura A*
- Procura IDA*



### Para poder utilizar os algoritmos implementados precisam de: 

1. Compilar o ficheiro com o código Lisp;

     ```(compile-file "procura.lisp") ```

2. Carregar o ficheiro compilado;

     ```(load "procura") ```

3. Criar um problema, usando para isso a função  ```cria-problema```;


4. Resolver o problema criado de acordo com o ponto 3, usando a função  ```procura```.

#### A função ```procura``` devolve uma lista de quatro elementos, pela ordem seguinte:

- Solução encontrada para o problema (lista de estados desde o estado inicial até ao estado final), ou NIL caso não encontre solução.

- Tempo gasto na procura.  Este valor é obtido a partir da função ```get-internal-run-time```.  Para obter valores em segundos, deve dividir este valor pela constante ```internal-time-units-per-second```.

- Número de nós expandidos durante a procura.

- Número de nós gerados durante a procura.
