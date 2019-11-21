
-- Question 1:

SELECT f.TITLE, SUM(t.PRICE) AS REVENUE 
	FROM TICKET t inner join Film f on (t.fkFilm = f.idFilm) 
	GROUP BY f.TITLE 
	ORDER BY REVENUE DESC;


-- Question 2:

SELECT 
	(SELECT COUNT(1) * 1.00
	FROM ticket f inner JOIN Shop s ON (f.idTicket = s.fkTicket)
		inner join food_beverages fb ON (s.fkTicket = fb.idProduct) 
	WHERE Category_ IN ('Popcorns', 'Soft-drinks')) / count(*) * 100.00 As Percent 
	FROM Ticket;
	

-- Question 3:

SELECT s.idSession as SESSION_ID,
	   s.Room as THEATRE,
	   s.Time_ as TIME_SLOT,
	   count(*) as NUMBER_OF_PEOPLE
	FROM TICKET t inner join SESSIONS s on (t.fkSession = s.idSession)
	GROUP BY s.idSession, s.TIME_, s.ROOM
	ORDER BY count(*) DESC LIMIT 1;
	
	
-- Question 4:

SELECT s.idSESSION as SESSION_ID,
	   s.ROOM as THEATRE,
	   s.TIME_ as TIME_SLOT,
	   SUM(t.Price) as TOTAL_SESSION_REVENUE 
	FROM TICKET t inner join SESSIONS s on (t.fkSession = s.idSession)
	GROUP BY s.idSession, s.ROOM, s.TIME_;
	

-- Question 5:

SELECT 
  SUM(Fb.Price) AS REVENUE_SHOP,
  (SELECT SUM(Price)​ FROM Ticket) AS REVENUE_TICKET​
  FROM TICKET t inner join SHOP s ON t.idTicket = s.fkTicket inner join FOOD_BEVERAGES fb ON s.fkProduct = fb.idProduct;