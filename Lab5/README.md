# Constraint Programming 

Definir un problema en CP consite en :
- Definir qué variables se van a hacer servir y su significado:

Por ejemplo, supongamos que queremos conseguir una cantidad de dinero usando usando unas monedas de un valor determinado.

```prolog
:- use_module(library(clpfd)).
main:- Coins = [1,2,5,13,17,35,157], Amount=361,
    length (Coins,N),
    length(Vars,N), 
```
- Determinar el dominio de las Variables    
 ```prolog
    Vars ins 0..Amount
```
-  Establecer las restricciones entre las variables
```prolog
    expresionAmount(Vars,Coins,E),  E#= Amount,
```
- Generar soluciones
```prolog
    expresionsuma(Vars,ES),
    labeling([min(ES)],Vars),write(E), nl.

expresionsuma([V],V).
expresionsuma([V|Vars],V+E):- expresionsuma(Vars,E).
```


Es decir todos los programas Prolog en Constraint Programming tienen una estructura similar: 
- Se definen las variables y sus dominios
- Se hacen restricciones acerca de esas variables
- Se generan soluciones

Para **definir un dominio** usamos los predicados **in, ins**:
```prolog
X in -2..4 % X pertenece a [-2,4]
X in -2..4 \/ 5..8 % X pertenece a [-2,4] U [5,8]
[X,Y] ins -2..4 \/ 5..8 % las dos variables tiene el dominio [-2,4] U [5,8]
```

En cuánto **a las restricciones** tenemos:
- **Expresiones aritmèticas**
```prolog
- E
E1+E2
E1-E2
E1*E2
E1 ** E2 % elevado
min(E1,E2)
max(E1,E2)
dist(E1,E2) % |E1 - E2|
E1 // E2 % división entera entre E1 y E2
E1 rem E2 % residuo entre E1 y E2
```
- **Restricciones aritméticas**
```prolog
E1 #= E2 #E1 tiene que ser igual a E2
E1 #\= E2 #E1 tiene que ser diferente de E2
E1 #< E2
E1 #=< E2
E1 #>= E2
```
- **Restricciones booleanas**
```prolog
0 % falso 
1 % cierto
#\ E % not E
E1 #/\ E2 % E1 and E2
E1 #\/ E2 % E1 or E2
E1 #==> E2 % E1 implica E2
E1 #<=> E2 % E1 equivalente a E2
E1 #\<=> E2 E1 diferente de E2
```
-**Altres**
```prolog
all_diferent(list) % todas las variables de List toman valores distintos
element_var(I,L,X) % X toma el mismo valor que el I-éssimo valor de L 
```


> Mirar com funciona exactalemt label o labeling, diferències i què fa que li passis una expressió

### OTROS EJEMPLOS

#### Problema de las 8 reinas

Dado un tablero cuadrado de 8x8 casillas, disponer 8 reinas sin que se maten unas a otras.
A cada columna no puede haber dos reinas así que podemos partir con la suposición de que cada reina está en una columna diferente y solo asignaremos filas a columnas. La variable Xi representa la fila correspondiente a la columna i. 

```prolog
:-use_module(library(clpfd)).
queens:- 
    L = [_,_,_,_,_,_,_,_],
    L  ins 1..8, 
    safe(L),
    label(L),
    write(L),
    nl.

safe([]).
safe(X|L):- check(X,L,1),safe(L).

check(_,[],_).
check(X,[Y|L],I):-
   X #\= Y,
   X #\= Y+I,
   X #\= Y-I,
   I2 is I+1,
   check(X,L,I2).
```


