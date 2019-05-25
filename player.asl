
/*----------------------------------------------------------------------------
	AUTOR: Jose Angel Duran Cerviño
	FECHA DE VERSION: 25/05/2019
	TRABAJO PARA LA ASIGNATURA:
		SISTEMAS INTELIGENTES, UNIVERSIDAD DE VIGO ( CAMPUS DE OURENSE )
----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*---------------------------USO DEL CODIGO-----------------------------------*/
/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------
	La finalidad de este codigo es la implementacion de un agente que pueda
jugar en dos modalidades al cuatro en raya, estas modalidades son jugar a ganar
y jugar a perder, el entorno sobre el que juega ha sido entregado por el profesor
y no admite modificaciones. La plataforma a la que esta dedicada este codigo
es jason.
----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*---------------------------DEFINICIONES INICIALES---------------------------*/
/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------
	Se considera Diagonal descendente la diagonal que va desde la parte
superior izquierda de la ficha hacia la inferior derecha.
	Analogamente se considera Diagonal Ascendente la diagonal que va desde la
parte inferior izquierda hacia la superior derecha.

----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/

/* Initial beliefs and rules */
/*----------------------------------------------------------------------------*/
/*---------------------------REGLAS AUXILIARES--------------------------------*/
/*----------------------------------------------------------------------------*/

/******************************************************************************/
/* Define quien es el jugador y quien[source(self)] es el rival de forma numerica en funcion del nombre, queda comprobado que el posicion solo reconoce a los agentes con
el nombre indicado */
quienSoy(1,2):-
	.my_name(player1).
quienSoy(2,1):-
	.my_name(player2).
/******************************************************************************/
	
/******************************************************************************/	
/* Comprueba si se puede colocar en una posicionn por encima de la fila siete y 
que esté libre */
/*Devuelve true en caso de poder colocar la ficha en la posición*/ 
puedoColocar(jugada(X,Y)):- 
	tablero(X,Y,0) &
    (X >= 0 & X < 8) &
    (Y >= 0 & Y < 8).

puedoColocar(X,Y):-
	tablero(X,Y,0) &
    (X >= 0 & X < 8) &
    (Y >= 0 & Y < 8) .
/******************************************************************************/

/******************************************************************************/
/*Devuelve true cuando no encuentra ningun cuatro en raya, o alguna posibilidad
de victoria inminente*/
cuatroEnRaya(Ficha):- 
	 not cuatroEnRayaHorizontal(Ficha) &
	 not cuatroEnRayaVertical(Ficha) &
	 not cuatroEnRayaDiagonal(Ficha) &
	 not rayable(Ficha,Nulo) &
	 not trio(Ficha,Nulo).
/******************************************************************************/

/******************************************************************************/
/*Reglas que comprueban la existencia de un cuatro en raya en base a una ficha 
dada*/
cuatroEnRayaHorizontal(ficha(X,Y,Jugador)):-
		tablero(X,Y,0) &
		tablero(X-1,Y,Jugador) &
		tablero(X+1,Y,Jugador) &
	    ( tablero(X+2,Y,Jugador) | tablero(X-2,Y,Jugador) ).
	
cuatroEnRayaVertical(ficha(X,Y,Jugador)):-
		tablero(X,Y,0) &
		tablero(X,Y-1,Jugador) &
		tablero(X,Y+1,Jugador) &
	    ( tablero(X,Y+2,Jugador) | tablero(X,Y-2,Jugador) ).	
	
cuatroEnRayaDiagonal(ficha(X,Y,Jugador)):-
		tablero(X,Y,0) &
		tablero(X+1,Y-1,Jugador) &
		tablero(X-1,Y+1,Jugador) &
		(tablero(X-2,Y+2,Jugador) | tablero(X+2,Y-2,Jugador)).

cuatroEnRayaDiagonal(ficha(X,Y,Jugador)):-
		tablero(X,Y,0) &
		tablero(X-1,Y-1,Jugador) &
		tablero(X+1,Y+1,Jugador) &
		(tablero(X-2,Y-2,Jugador) | tablero(X+2,Y+2,Jugador)).
/******************************************************************************/	

/******************************************************************************/
/*Analiza todas las posiciones del tablero y busca que posiciones tiene 
fichas colocadas*/
/*Devuelve true siempre que pueda leer todo el tablero*/
buscar(8,7,[]).
buscar(8,Y,Lista):- buscar(0,Y+1,Lista).
buscar(X,Y,[ficha(X,Y,Ocupante)|Tail]):- tablero(X,Y,Ocupante)   & not (Ocupante=0 | Ocupante = -11)  & buscar(X+1,Y,Tail).
/*En caso de encontrar una celda bloqueada la ignora*/
buscar(X,Y,Lista):- tablero(X,Y,-11) & buscar(X+1,Y,Lista).
/*En caso de encontrar una celda vacía la ignora*/
buscar(X,Y,Lista):- buscar(X+1,Y,Lista). 
/******************************************************************************/

/******************************************************************************/
/*Devuelve true cuando las fichas cercanas a una dada no estén ocupadas por el 
jugador objetivo y haya al menos una casilla libre adyacente*/
aislada(ficha(X,Y,Jugador)):-
		/*Al menos encuentra una casilla cercana libre*/
		( 
			tablero(X+1,Y,0) |
			tablero(X+1,Y+1,0) |
			tablero(X,Y+1,0) |
			tablero(X-1,Y+1,0) |
			tablero(X-1,Y,0) |
			tablero(X-1,Y-1,0) |
			tablero(X,Y-1,0) |
			tablero(X+1,Y-1,0) 
		) &
		
		/*No encuentre ninguna del jugador adyacente*/
		not (
			tablero(X+1,Y,Jugador) |
			tablero(X+1,Y+1,Jugador) |
			tablero(X,Y+1,Jugador) |
			tablero(X-1,Y+1,Jugador) |
			tablero(X-1,Y,Jugador) |
			tablero(X-1,Y-1,Jugador) |
			tablero(X,Y-1,Jugador) |
			tablero(X+1,Y-1,Jugador) 
		).
/******************************************************************************/

/******************************************************************************/
/*Devuelve true cuando las fichas cercanas a una dada no estén ocupadas por el 
jugador objetivo y no haya nignuna casilla libre adyacente*/
solitariaInutil(X,Y,Jugador):-
		/*No encuentre ninguna casilla cercana libre*/
		not ( 
			tablero(X+1,Y,0) |
			tablero(X+1,Y+1,0) |
			tablero(X,Y+1,0) |
			tablero(X-1,Y+1,0) |
			tablero(X-1,Y,0) |
			tablero(X-1,Y-1,0) |
			tablero(X,Y-1,0) |
			tablero(X+1,Y-1,0) 
		) &
		
		/*No encuentre ninguna del jugador adyacente*/
		not (
			tablero(X+1,Y,Jugador) |
			tablero(X+1,Y+1,Jugador) |
			tablero(X,Y+1,Jugador) |
			tablero(X-1,Y+1,Jugador) |
			tablero(X-1,Y,Jugador) |
			tablero(X-1,Y-1,Jugador) |
			tablero(X,Y-1,Jugador) |
			tablero(X+1,Y-1,Jugador) 
		).
/******************************************************************************/

/*----------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------*/
/*---------------------------PAREJAS------------------------------------------*/
/*----------------------------------------------------------------------------*/

/*Busca la existencia de una ficha que puede generar una pareja que podría
crear un cuarteto*/
/*Se contemplan todas las posibilidades de ubicación de los espacios libres 
junto a la ficha objetivo*/

/******************************************************************************/
/*Horizontales*/
parejaViable(ficha(X,Y,Jugador),jugada(X+1,Y)):- 
			tablero(X+1,Y,0) & tablero(X+2,Y,0) & tablero(X+3,Y,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X+1,Y)):- 
			tablero(X+1,Y,Jugador) & tablero(X+2,Y,0) &	tablero(X-1,Y,0).	
			
parejaViable(ficha(X,Y,Jugador),jugada(X-1,Y)):- 
			tablero(X-1,Y,0) & tablero(X-2,Y,0) & tablero(X-3,Y,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X-1,Y)):- 
			tablero(X+1,Y,Jugador) & tablero(X-2,Y,0) &	tablero(X-1,Y,0).	
/******************************************************************************/

/******************************************************************************/
/*Verticales*/
parejaViable(ficha(X,Y,Jugador),jugada(X,Y+1)):- 
			tablero(X,Y+1,0) & tablero(X,Y+2,0) & tablero(X,Y+3,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X,Y+1)):- 
			tablero(X,Y+1,Jugador) & tablero(X,Y+2,0) &	tablero(X,Y-1,0).	
			
parejaViable(ficha(X,Y,Jugador),jugada(X,Y-1)):- 
			tablero(X,Y-1,0) & tablero(X,Y-2,0) & tablero(X,Y-3,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X,Y-1)):- 
			tablero(X,Y-1,Jugador) & tablero(X,Y-2,0) &	tablero(X,Y+1,0).
/******************************************************************************/

/******************************************************************************/
/*Diagonal Descendente*/

parejaViable(ficha(X,Y,Jugador),jugada(X+1,Y+1)):- 
			tablero(X+1,Y+1,0) & tablero(X+2,Y+2,0) & tablero(X+3,Y+3,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X+1,Y+1)):- 
			tablero(X+1,Y+1,Jugador) & tablero(X+2,Y+2,0) &	tablero(X-1,Y-1,0).	
			
parejaViable(ficha(X,Y,Jugador),jugada(X-1,Y-1)):- 
			tablero(X-1,Y-1,0) & tablero(X-2,Y-2,0) & tablero(X-3,Y-3,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X-1,Y-1)):- 
			tablero(X-1,Y-1,Jugador) & tablero(X-2,Y-2,0) &	tablero(X+1,Y+1,0).
/******************************************************************************/

/******************************************************************************/
/*Diagonal Ascendente*/

parejaViable(ficha(X,Y,Jugador),jugada(X+1,Y-1)):- 
			tablero(X+1,Y-1,0) & tablero(X+2,Y-2,0) & tablero(X+3,Y-3,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X+1,Y-1)):- 
			tablero(X+1,Y-1,Jugador) & tablero(X+2,Y-2,0) &	tablero(X-1,Y+1,0).	
			
parejaViable(ficha(X,Y,Jugador),jugada(X-1,Y+1)):- 
			tablero(X-1,Y+1,0) & tablero(X-2,Y+2,0) & tablero(X-3,Y+3,0).
			
parejaViable(ficha(X,Y,Jugador),jugada(X-1,Y+1)):- 
			tablero(X-1,Y+1,Jugador) & tablero(X-2,Y+2,0) &	tablero(X+1,Y-1,0).
/******************************************************************************/

/******************************************************************************/
/*Comprueba que una pareja puede generar un trio que pueda generar un 
cuatro en raya, esto es que haya al menos 2 casillas consecutivas libres a un 
lado de la pareja o que haya una casilla libre a ambos lados de la pareja*/

/******************************************************************************/
/*Crear trio Vertical*/
triable(ficha(X,Y,Jugador),jugada(X,Y-1)):- 	
	tablero(X,Y+1,Jugador) &	
	tablero(X,Y-1,0) & (tablero(X,Y-2,0) | tablero(X,Y+2,0)).
	
triable(ficha(X,Y,Jugador),jugada(X,Y+1)):- 	
	tablero(X,Y-1,Jugador) &	
	tablero(X,Y+1,0) & (tablero(X,Y+2,0) | tablero(X,Y-2,0)).	
/******************************************************************************/

/******************************************************************************/
/*Crear trio Horizontal*/

triable(ficha(X,Y,Jugador),jugada(X+1,Y)):- 	
	tablero(X-1,Y,Jugador) &	
	tablero(X+1,Y,0) & (tablero(X+2,Y,0) | tablero(X-2,Y,0)).

triable(ficha(X,Y,Jugador),jugada(X-1,Y)):- 	
	tablero(X+1,Y,Jugador) &	
	tablero(X-1,Y,0) &  (tablero(X-2,Y,0) | tablero(X+2,Y,0)).
/******************************************************************************/

/******************************************************************************/
/*Crear trio Diagonal Descendente*/
triable(ficha(X,Y,Jugador),jugada(X-1,Y-1)):- 	
	tablero(X+1,Y+1,Jugador) &	
	((tablero(X-1,Y-1,0) & tablero(X-2,Y-2,0)) | (tablero(X-1,Y-1,0) & tablero(X+2,Y+2,0))).
	
triable(ficha(X,Y,Jugador),jugada(X+1,Y+1)):- 	
	tablero(X-1,Y-1,Jugador) &	
	tablero(X+1,Y+1,0) & (tablero(X+2,Y+2,0) |tablero(X-2,Y-2,0)).
/******************************************************************************/

/******************************************************************************/
/*Crear trio Diagonal Ascendente*/	
triable(ficha(X,Y,Jugador),jugada(X-1,Y+1)):- 	
	tablero(X+1,Y-1,Jugador) &	
	tablero(X-1,Y+1,0) & (tablero(X-2,Y+2,0) | tablero(X+2,Y-2,0)).
	
triable(ficha(X,Y,Jugador),jugada(X+1,Y-1)):- 	
	tablero(X-1,Y+1,Jugador) &	
	tablero(X+1,Y-1,0) & ( tablero(X+2,Y-2,0) | tablero(X-2,Y+2,0)).
/******************************************************************************/

/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*---------------------------TRIOS--------------------------------------------*/
/*----------------------------------------------------------------------------*/

/*Se analizan los trios con una posicion adyacente libre para completar el 
cuatro en raya*/

/******************************************************************************/
/*Ascendentes*/
trio(ficha(X,Y,Jugador),jugada(X-2,Y+2)):-
		tablero(X+1,Y-1,Jugador) &
		tablero(X-1,Y+1,Jugador) &
		tablero(X-2,Y+2,0) &
		puedoColocar(X-2,Y+2).
		
trio(ficha(X,Y,Jugador),jugada(X+2,Y-2)):-
		tablero(X+1,Y-1,Jugador) &
		tablero(X-1,Y+1,Jugador) &
		tablero(X+2,Y-2,0) &
		puedoColocar(X+2,Y-2).
/******************************************************************************/

/******************************************************************************/
/*Descendentes*/
trio(ficha(X,Y,Jugador),jugada(X+2,Y+2)):-
		tablero(X+1,Y+1,Jugador) &
		tablero(X-1,Y-1,Jugador) &
		tablero(X+2,Y+2,0).
trio(ficha(X,Y,Jugador),jugada(X+2,Y+2)):-
		tablero(X+1,Y+1,Jugador) &
		tablero(X-1,Y-1,Jugador) &
		tablero(X-2,Y-2,0).
/******************************************************************************/		
		
/******************************************************************************/
/*Horizontales*/
trio(ficha(X,Y,Jugador),jugada(X+2,Y)):-
		tablero(X-1,Y,Jugador) &
		tablero(X+1,Y,Jugador) &
		tablero(X+2,Y,0).
		
trio(ficha(X,Y,Jugador),jugada(X-2,Y)):-
		tablero(X-1,Y,Jugador) &
		tablero(X+1,Y,Jugador) &
		tablero(X-2,Y,0).
/******************************************************************************/

/******************************************************************************/		
/*Verticales*/
trio(ficha(X,Y,Jugador),jugada(X,Y-2)):-
		tablero(X,Y+1,Jugador) &
		tablero(X,Y-1,Jugador) &
		tablero(X-2,Y,0).
		
trio(ficha(X,Y,Jugador),jugada(X,Y+2)):-
		tablero(X,Y+1,Jugador) &
		tablero(X,Y-1,Jugador) &
		tablero(X,Y+2,0).
/******************************************************************************/

/*----------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------*/
/*----------------COMBINACIONES QUE HACEN CUATRO EN RAYA----------------------*/
/*----------------------------------------------------------------------------*/
/*Combinaciones de ficha + blanco + 2fichas*/ 

/******************************************************************************/
/*Horizontal*/
rayable(ficha(X,Y,Jugador),jugada(X+1,Y)):- 
	tablero(X+1,Y,0) & 
	tablero(X+2,Y,Jugador) & 
	tablero(X+3,Y,Jugador).
	
rayable(ficha(X,Y,Jugador),jugada(X-1,Y)):- 
	tablero(X-1,Y,0) & 
	tablero(X-2,Y,Jugador) & 
	tablero(X-3,Y,Jugador).
/******************************************************************************/

/******************************************************************************/
/*Vertical*/
rayable(ficha(X,Y,Jugador),jugada(X,Y-1)):- 
	tablero(X,Y-1,0) & 
	tablero(X,Y-2,Jugador) & 
	tablero(X,Y-3,Jugador).
	
rayable(ficha(X,Y,Jugador),jugada(X,Y+1)):- 
	tablero(X,Y+1,0) & 
	tablero(X,Y+2,Jugador) & 
	tablero(X,Y+3,Jugador).
/******************************************************************************/

/******************************************************************************/
/*Ascendente*/
rayable(ficha(X,Y,Jugador),jugada(X+1,Y-1)):- 
	tablero(X+1,Y-1,0) & 
	tablero(X+2,Y-2,Jugador) & 
	tablero(X+3,Y-3,Jugador).
	
rayable(ficha(X,Y,Jugador),jugada(X-1,Y+1)):- 
	tablero(X-1,Y+1,0) & 
	tablero(X-2,Y+2,Jugador) & 
	tablero(X-3,Y+3,Jugador).
/******************************************************************************/

/******************************************************************************/
/*Descendente*/
rayable(ficha(X,Y,Jugador),jugada(X-1,Y-1)):- 
	tablero(X-1,Y-1,0) & 
	tablero(X-2,Y-2,Jugador) & 
	tablero(X-3,Y-3,Jugador).
	
rayable(ficha(X,Y,Jugador),jugada(X+1,Y+1)):- 
	tablero(X+1,Y+1,0) & 
	tablero(X+2,Y+2,Jugador) & 
	tablero(X+3,Y+3,Jugador).
/******************************************************************************/

/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*----------------------OPCIONES DE MOVIMIENTO--------------------------------*/
/*----------------------------------------------------------------------------*/

/******************************************************************************/
hagoCuatroEnRaya(Jugador,[ficha(X,Y,Jugador)|Tail],Resultado):- 
	rayable(ficha(X,Y,Jugador),Resultado) & puedoColocar(Resultado).
hagoCuatroEnRaya(Jugador,[ficha(X,Y,Jugador)|Tail],Resultado):- 
	trio(ficha(X,Y,Jugador),Resultado) & puedoColocar(Resultado).
hagoCuatroEnRaya(Jugador,[Head|Tail],Resultado):- 
	not .empty([Head|Tail])  & hagoCuatroEnRaya(Jugador,Tail,Resultado).
/******************************************************************************/

/******************************************************************************/
evitoCuatroEnRaya(Rival,[ficha(X,Y,Rival)|Tail],Resultado):- 
	rayable(ficha(X,Y,Rival),Resultado) & puedoColocar(Resultado).
evitoCuatroEnRaya(Rival,[ficha(X,Y,Rival)|Tail],Resultado):-
	trio(ficha(X,Y,Rival),Resultado)  & puedoColocar(Resultado).
evitoCuatroEnRaya(Rival,[Head|Tail],Resultado):-  
	not .empty(Tail)  & evitoCuatroEnRaya(Rival,Tail,Resultado).
/******************************************************************************/

/******************************************************************************/
hagoTrioGanador(Jugador,[ficha(X,Y,Jugador)|Tail],Resultado):- 
	triable(ficha(X,Y,Jugador),Resultado) & puedoColocar(Resultado).
hagoTrioGanador(Jugador,[Head|Tail],Resultado):- 
	not .empty(Tail) & hagoTrioGanador(Jugador,Tail,Resultado).
/******************************************************************************/

/******************************************************************************/
evitoTrioGanador(Jugador,[ficha(X,Y,Jugador)|Tail],Resultado):- 
	triable(ficha(X,Y,Jugador),Resultado) & puedoColocar(Resultado).
evitoTrioGanador(Jugador,[Head|Tail],Resultado):- 
	not .empty(Tail) & evitoTrioGanador(Jugador,Tail,Resultado).
/******************************************************************************/

/******************************************************************************/
hagoPareja(Jugador,[ficha(X,Y,Jugador)|Tail],Resultado):- 
	parejaViable(ficha(X,Y,Jugador),Resultado) &  puedoColocar(Resultado).
hagoPareja(Jugador,[Head|Tail],Resultado):- 
	not .empty(Tail) & hagoPareja(Jugador,Tail,Resultado).
/******************************************************************************/

/******************************************************************************/
dominoEsquinas(Jugador,jugada(2,2)):- tablero(2,2,0).
dominoEsquinas(Jugador,jugada(2,5)):- tablero(2,5,0).
dominoEsquinas(Jugador,jugada(5,2)):- tablero(5,2,0).
dominoEsquinas(Jugador,jugada(5,5)):- tablero(5,5,0).
/******************************************************************************/

/******************************************************************************/
dominoBordesBloqueo(Jugador,jugada(2,3)):- tablero(2,3,0).
dominoBordesBloqueo(Jugador,jugada(2,4)):- tablero(2,4,0).
dominoBordesBloqueo(Jugador,jugada(5,3)):- tablero(5,3,0).
dominoBordesBloqueo(Jugador,jugada(5,4)):- tablero(5,4,0).
dominoBordesBloqueo(Jugador,jugada(3,2)):- tablero(3,2,0).
dominoBordesBloqueo(Jugador,jugada(4,2)):- tablero(4,2,0).
dominoBordesBloqueo(Jugador,jugada(3,5)):- tablero(3,5,0).
dominoBordesBloqueo(Jugador,jugada(4,5)):- tablero(4,5,0).
/******************************************************************************/

/******************************************************************************/
colocoSolitaria(Jugador,jugada(X,Y)):- 
	tablero(X,Y,0) & not cuatroEnRaya(ficha(X,Y,Jugador))& oponente(Rival) & cuatroEnRaya(ficha(X,Y,Rival)).
colocoSolitaria(Jugador,jugada(X,Y)):- 
	tablero(X,Y,0) & not cuatroEnRaya(ficha(X,Y,Jugador)).
colocoSolitaria(Jugador,jugada(X,Y)):- tablero(X,Y,0).
/******************************************************************************/

/******************************************************************************/
marginoEsquinas(jugada(0,0)):- tablero(0,0,0) & oponente(Rival) & cuatroEnRaya(ficha(0,0,Rival)).
marginoEsquinas(jugada(0,7)):- tablero(0,7,0) & oponente(Rival) & cuatroEnRaya(ficha(0,7,Rival)).
marginoEsquinas(jugada(7,0)):- tablero(7,0,0) & oponente(Rival) & cuatroEnRaya(ficha(7,0,Rival)).
marginoEsquinas(jugada(7,7)):- tablero(7,7,0) & oponente(Rival) & cuatroEnRaya(ficha(7,7,Rival)).
/******************************************************************************/

/******************************************************************************/
marginoBorde(Jugador,jugada(0,Y)) :- 
	oponente(Rival) & tablero(0,Y,0) &  cuatroEnRaya(ficha(0,Y,Jugador)) & cuatroEnRaya(ficha(0,Y,Rival)).
marginoBorde(Jugador,jugada(7,Y)) :- 
	oponente(Rival) & tablero(7,Y,0) &  cuatroEnRaya(ficha(7,Y,Jugador)) & cuatroEnRaya(ficha(7,Y,Rival)).
marginoBorde(Jugador,jugada(X,0)) :- 
	oponente(Rival) & tablero(X,0,0) &  cuatroEnRaya(ficha(X,0,Jugador)) & cuatroEnRaya(ficha(X,0,Rival)).
marginoBorde(Jugador,jugada(X,7)) :- 
	oponente(Rival) & tablero(X,7,0) &  cuatroEnRaya(ficha(X,7,Jugador)) & cuatroEnRaya(ficha(X,7,Rival)).
/******************************************************************************/

/******************************************************************************/
marginoCuadradoInterno(Jugador,jugada(2,Y)) :- 
	tablero(2,Y,0)  &  cuatroEnRaya(ficha(2,Y,Jugador)) & oponente(Rival) & cuatroEnRaya(ficha(2,Y,Rival)).
marginoCuadradoInterno(Jugador,jugada(5,Y)) :- 
	tablero(5,Y,0)  &  cuatroEnRaya(ficha(5,Y,Jugador)) & oponente(Rival) & cuatroEnRaya(ficha(5,Y,Rival)).
marginoCuadradoInterno(Jugador,jugada(X,2)) :- 
	tablero(X,2,0)  &  cuatroEnRaya(ficha(X,2,Jugador)) & oponente(Rival) & cuatroEnRaya(ficha(X,2,Rival)).
marginoCuadradoInterno(Jugador,jugada(X,5)) :- 
	tablero(X,5,0)  &  cuatroEnRaya(ficha(X,5,Jugador)) & oponente(Rival) & cuatroEnRaya(ficha(X,5,Rival)).
/******************************************************************************/

/******************************************************************************/
colocoSolitariaDondePueda(Jugador,jugada(X,Y)):- 
	tablero(X,Y,0) & cuatroEnRaya(ficha(X,Y,Jugador))& oponente(Rival) & cuatroEnRaya(ficha(X,Y,Rival)).
colocoSolitariaDondePueda(Jugador,jugada(X,Y)):- 
	tablero(X,Y,0) & cuatroEnRaya(ficha(X,Y,Jugador)).
colocoSolitariaDondePueda(Jugador,jugada(X,Y)):- tablero(X,Y,0).
/******************************************************************************/

/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*-------------------------MOVIMIENTO JUGAR A GANAR---------------------------*/
/*----------------------------------------------------------------------------*/
/*Primer movimiento*/
movimiento(Jugador,Lista,Resultado):- estrategia(jugarAGanar)[source(Percept)] &
		oponente(Rival) & .empty(Lista) &
		( 		
			dominoEsquinas(Jugador,Resultado) |
			dominoBordesBloqueo(Jugador,Resultado) |
			colocoSolitaria(Jugador,Resultado)
		).
		
/*Movimientos normales*/
/*Están ordenados según pienso que son importantes*/
movimiento(Jugador,Lista,Resultado):- estrategia(jugarAGanar)[source(Percept)] &
		oponente(Rival) & not .empty(Lista) &
		( 
			hagoCuatroEnRaya(Jugador,Lista,Resultado)|
			evitoCuatroEnRaya(Rival,Lista,Resultado) |		
			hagoTrioGanador(Jugador,Lista,Resultado) |
			evitoTrioGanador(Rival,Lista,Resultado) |
			hagoPareja(Jugador,Lista,Resultado) |
			dominoEsquinas(Jugador,Resultado) |
			dominoBordesBloqueo(Jugador,Resultado)|
			colocoSolitaria(Jugador,Resultado)
		).

/*----------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------*/
/*-------------------------MOVIMIENTO JUGAR A PERDER--------------------------*/
/*----------------------------------------------------------------------------*/
/*Primer movimiento*/
movimiento(Jugador,Lista,Resultado):- estrategia(jugarAPerder)[source(Percept)]&
		oponente(Rival) & .empty(Lista) & marginoEsquinas(Resultado).
		
/*Movimientos normales*/
/*Están ordenados según pienso que son importantes*/
movimiento(Jugador,Lista,Resultado):- estrategia(jugarAPerder)[source(Percept)]&
		oponente(Rival) & not .empty(Lista) &
		( 
			marginoEsquinas(Resultado) |
			marginoBorde(Jugador,Resultado) |
			marginoCuadradoInterno(Jugador,Resultado) |
			colocoSolitariaDondePueda(Jugador,Resultado)
		).

/*----------------------------------------------------------------------------*/



/* Initial goals */
!comienzo.

/* Plans */
/*----------------------------------------------------------------------------*/
/*---------------------------------TURNOS-------------------------------------*/
/*----------------------------------------------------------------------------*/
/* Define elementos iniciales tales como el primer turno */
+!comienzo[source(self)]:
	turno(X)[source(percept)] &
	.my_name(X) &
	quienSoy(X1,Y1) <- 
		+jugador(X1);
		+oponente(Y1);
		!jugar.
+!comienzo[source(self)]:
	turno(X)[source(percept)] &
	not .my_name(X) &
	quienSoy(X1,Y1) <- 
		+jugador(X1);
		+oponente(Y1);
		!jugar.
+!comienzo[source(self)] <- 
		.wait(50); 
		!comienzo.
/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*---------------------------------JUGAR--------------------------------------*/
/*----------------------------------------------------------------------------*/
/* Gestiona el turno*/
/*Comprueba que es su turno, observa las fichas en el tablero y comprueba que 
no hay 4 en raya, después elige un movimiento y lo ejecuta*/

+!jugar[source(self)]:
	turno(X)[source(percept)] & .my_name(X) &
	jugador(J) & buscar(0,0,Lista) & .print("Me toca") & movimiento(J,Lista,jugada(A,B))<-
								.wait(500);
								put(A,B);
								!jugar.
				
/* Si es el turno del oponente no [source(self)]hace nada */
+!jugar[source(self)] <-
	.print("Espero a mi turno...");
	.wait(500);
	!jugar. 
	

/*------------------------------EVITAR TRAMPAS--------------------------------*/
+!A[source(Agente)] <-
	Agente \== percept.