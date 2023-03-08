schedule(canakkale, erzincan, 6).
schedule(erzincan, canakkale, 6).
schedule(erzincan, antalya, 3).
schedule(antalya, erzincan, 3).
schedule(antalya, izmir, 2).
schedule(izmir, antalya, 2).
schedule(antalya, diyarbakir, 4).
schedule(diyarbakir, antalya, 4).
schedule(izmir, istanbul, 2).
schedule(istanbul, izmir, 2).
schedule(izmir, ankara, 6).
schedule(ankara, izmir, 6).
schedule(diyarbakir, ankara, 8).
schedule(ankara, diyarbakir, 8).
schedule(rize, istanbul, 4).
schedule(istanbul, rize, 4).
schedule(ankara, istanbul, 1).
schedule(istanbul, ankara, 1).
schedule(rize, ankara, 5).
schedule(ankara, rize, 5).
schedule(ankara, van, 4).
schedule(van, ankara, 4).
schedule(gaziantep, van, 3).
schedule(van, gaziantep, 3).

connection(X, Y, C) :- schedule(X, Y, C).										
connection(X, Y, C) :- new_connection(X, Y, C, []).					 				
new_connection(X, Y, C, _) :- schedule(X, Y, C).									
new_connection(X, Z, C, Visited) :- 
	(X \== Z,
	\+ member(X, Visited),  									
	schedule(X, Y, C1), 										
	new_connection(Y, Z, C2, [X | Visited]), 						
	C is C1 + C2).									
			
