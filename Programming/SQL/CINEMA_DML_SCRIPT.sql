DELETE * FROM FILM;

DELETE * FROM FOOD_BEVERAGES;

DELETE * FROM SESSIONS;

DELETE * FROM TICKET;

DELETE * FROM SHOP;


INSERT INTO FILM 
	VALUES 
	(1,'Titanic',2000,'Romance'),
	(2,'The Godfather',2001,'Mafia'),
	(3,'Star Wars',2002,'Science Fiction'),
	(4,'The Dark Night',2004,'Thriller'),
	(5,'Jaws',2000,'Horror'),
	(6,'The Lord of the Rings',2001,'Adventure'),
	(7,'The Matrix',2002,'Action'),
	(8,'Saving Private Ryan',2004,'Action'),
	(9,'Forrest Gump',2000,'Drama'),
	(10,'Game of Thrones (the film)',2019,'Action');

INSERT INTO FOOD_BEVERAGES VALUES
	(1,'Popcorns','Popcorns','S',2.00),
	(2,'Popcorns','Popcorns','M',4.00),
	(3,'Popcorns','Popcorns','L',6.00),
	(4,'Soda','Soft-drinks','S',1.00),
	(5,'Soda','Soft-drinks','M',2.00),
	(6,'Soda','Soft-drinks','L',3.00),
	(7,'Nachos','Food','S',2.00),
	(8,'Nachos','Food','M',3.00),
	(9,'Nachos','Food','L',4.00);
	
INSERT INTO SESSIONS VALUES
	(1,1,'13:00:00',200),
	(2,2,'13:00:00',150),
	(3,3,'13:00:00',150),
	(4,4,'13:00:00',100),
	(5,1,'15:00:00',200),
	(6,2,'15:00:00',150),
	(7,3,'15:00:00',150),
	(8,4,'15:00:00',100),
	(9,1,'17:00:00',200),
	(10,2,'17:00:00',150),
	(11,3,'17:00:00',150),
	(12,4,'17:00:00',100),
	(13,1,'19:00:00',200),
	(14,2,'19:00:00',150),
	(15,3,'19:00:00',150),
	(16,4,'19:00:00',100),
	(17,1,'21:00:00',200),
	(18,2,'21:00:00',150),
	(19,3,'21:00:00',150),
	(20,4,'21:00:00',100),
	(21,1,'23:00:00',200),
	(22,2,'23:00:00',150),
	(23,3,'23:00:00',150),
	(24,4,'23:00:00',100);
	
INSERT INTO TICKET 
	VALUES
	(1,6,3,5.00,'2019-01-02 00:00:00', 98),
	(2,21,4,6.00,'2019-01-02 00:00:00', 94),
	(3,16,1,8.00,'2019-01-02 00:00:00', 39),
	(4,11,9,8.00,'2019-01-02 00:00:00', 97),
	(5,17,2,9.00,'2019-01-02 00:00:00', 94),
	(6,13,8,7.00,'2019-01-02 00:00:00', 41),
	(7,14,5,5.00,'2019-01-02 00:00:00', 83),
	(8,6,3,6.00,'2019-01-02 00:00:00', 9),
	(9,23,6,7.00,'2019-01-02 00:00:00', 77),
	(10,8,5,5.00,'2019-01-02 00:00:00', 24),
	(11,4,2,9.00,'2019-01-02 00:00:00', 8),
	(12,11,9,7.00,'2019-01-02 00:00:00', 31),
	(13,1,10,9.00,'2019-01-02 00:00:00', 56),
	(14,10,7,7.00,'2019-01-02 00:00:00', 24),
	(15,23,6,6.00,'2019-01-02 00:00:00', 6),
	(16,14,5,9.00,'2019-01-02 00:00:00', 96),
	(17,8,5,6.00,'2019-01-02 00:00:00', 89),
	(18,24,8,9.00,'2019-01-02 00:00:00', 60),
	(19,11,5,7.00,'2019-01-03 00:00:00', 46),
	(20,17,6,9.00,'2019-01-03 00:00:00', 1),
	(21,14,9,5.00,'2019-01-03 00:00:00', 84),
	(22,18,4,8.00,'2019-01-03 00:00:00', 67),
	(23,10,2,6.00,'2019-01-03 00:00:00', 39),
	(24,7,8,7.00,'2019-01-03 00:00:00', 4),
	(25,13,4,8.00,'2019-01-03 00:00:00', 8),
	(26,24,1,8.00,'2019-01-03 00:00:00', 48),
	(27,8,7,7.00,'2019-01-03 00:00:00', 79),
	(28,2,10,8.00,'2019-01-03 00:00:00', 23),
	(29,6,9,6.00,'2019-01-03 00:00:00', 14),
	(30,16,8,8.00,'2019-01-03 00:00:00', 87),
	(31,24,1,5.00,'2019-01-03 00:00:00', 98),
	(32,21,3,8.00,'2019-01-03 00:00:00', 3),
	(33,3,5,5.00,'2019-01-03 00:00:00', 12),
	(34,16,8,9.00,'2019-01-03 00:00:00', 61),
	(35,24,1,8.00,'2019-01-03 00:00:00', 45),
	(36,7,8,7.00,'2019-01-03 00:00:00', 51),
	(37,2,10,5.00,'2019-01-03 00:00:00', 83),
	(38,2,10,6.00,'2019-01-03 00:00:00', 53),
	(39,9,6,6.00,'2019-01-03 00:00:00', 59),
	(40,19,4,8.00,'2019-01-03 00:00:00', 65),
	(41,10,2,7.00,'2019-01-03 00:00:00', 23),
	(42,5,8,5.00,'2019-01-03 00:00:00', 27),
	(43,15,7,8.00,'2019-01-03 00:00:00', 95),
	(44,20,4,7.00,'2019-01-03 00:00:00', 92),
	(45,14,9,6.00,'2019-01-03 00:00:00', 38),
	(46,14,9,9.00,'2019-01-03 00:00:00', 25),
	(47,1,9,7.00,'2019-01-04 00:00:00', 49),
	(48,21,5,9.00,'2019-01-04 00:00:00', 25),
	(49,19,4,5.00,'2019-01-04 00:00:00', 32),
	(50,20,6,8.00,'2019-01-04 00:00:00', 72),
	(51,5,1,7.00,'2019-01-04 00:00:00', 52),
	(52,4,7,8.00,'2019-01-04 00:00:00', 38),
	(53,1,9,6.00,'2019-01-04 00:00:00', 78),
	(54,11,2,9.00,'2019-01-04 00:00:00', 14),
	(55,16,8,8.00,'2019-01-04 00:00:00', 10),
	(56,6,10,5.00,'2019-01-04 00:00:00', 47),
	(57,24,4,7.00,'2019-01-04 00:00:00', 70),
	(58,4,7,7.00,'2019-01-04 00:00:00', 65),
	(59,2,3,7.00,'2019-01-04 00:00:00', 90),
	(60,14,5,7.00,'2019-01-04 00:00:00', 79),
	(61,5,1,7.00,'2019-01-04 00:00:00', 51),
	(62,15,3,7.00,'2019-01-04 00:00:00', 51),
	(63,23,10,9.00,'2019-01-04 00:00:00', 34), 
	(64,4,7,5.00,'2019-01-04 00:00:00', 52),
	(65,17,2,7.00,'2019-01-04 00:00:00', 34),
	(66,23,10,9.00, '2019-01-04 00:00:00', 89),
	(67,22,9,9.00, '2019-01-04 00:00:00', 61),
	(68,17,2,6.00, '2019-01-04 00:00:00', 53),
	(69,16,8,9.00, '2019-01-04 00:00:00', 69),
	(70,16,8,9.00, '2019-01-04 00:00:00', 51),
	(71,3,6,8.00, '2019-01-04 00:00:00', 73),
	(72,18,8,5.00, '2019-01-04 00:00:00', 39),
	(73,17,2,5.00, '2019-01-04 00:00:00', 89),
	(74,5,1,5.00, '2019-01-04 00:00:00', 13),
	(75,11,2,8.00, '2019-01-04 00:00:00', 53),
	(76,23,10,8.00, '2019-01-04 00:00:00', 33),
	(77,16,8,6.00, '2019-01-04 00:00:00', 47),
	(78,9,7,5.00, '2019-01-04 00:00:00', 72),
	(79,10,6,5.00, '2019-01-04 00:00:00', 90),
	(80,9,7,7.00, '2019-01-04 00:00:00', 53),
	(81,15,4,7.00, '2019-01-05 00:00:00', 39),
	(82,10,10,9.00, '2019-01-05 00:00:00', 30),
	(83,11,6,9.00, '2019-01-05 00:00:00', 70),
	(84,13,2,9.00, '2019-01-05 00:00:00', 27),
	(85,17,1,6.00, '2019-01-05 00:00:00', 83),
	(86,22,8,8.00, '2019-01-05 00:00:00', 90),
	(87,18,9,5.00, '2019-01-05 00:00:00', 39),
	(88,1,3,5.00, '2019-01-05 00:00:00', 23),
	(89,9,5,8.00, '2019-01-05 00:00:00', 37),
	(90,1,3,8.00, '2019-01-05 00:00:00', 6),
	(91,14,7,5.00, '2019-01-05 00:00:00', 57),
	(92,1,3,7.00, '2019-01-05 00:00:00', 7),
	(93,11,6,7.00, '2019-01-05 00:00:00', 88),
	(94,18,9,6.00, '2019-01-05 00:00:00', 3),
	(95,15,4,8.00, '2019-01-05 00:00:00', 2),
	(96,14,7,8.00, '2019-01-05 00:00:00', 54),
	(97,22,8,6.00, '2019-01-05 00:00:00', 35),
	(98,12,3,7.00, '2019-01-05 00:00:00', 84),
	(99,17,1,5.00, '2019-01-05 00:00:00', 51),
	(100,13,2,8.00, '2019-01-05 00:00:00', 92);
	
	
INSERT INTO SHOP 
	VALUES
	(6,1),
	(5,2),
	(7,3),
	(5,6),
	(9,7),
	(9,8),
	(7,9),
	(1,10),
	(2,12),
	(5,13),
	(9,14),
	(2,15),
	(4,16),
	(8,17),
	(6,18),
	(1,19),
	(8,20),
	(4,21),
	(8,22),
	(5,23),
	(7,24),
	(9,27),
	(4,28),
	(1,29),
	(5,30),
	(2,31),
	(7,32),
	(3,40),
	(5,41),
	(6,42),
	(2,43),
	(1,44),
	(7,48),
	(2,49),
	(4,50),
	(7,51),
	(1,52),
	(4,53),
	(9,66),
	(9,67),
	(9,68),
	(8,69),
	(6,70),
	(3,71),
	(7,72),
	(3,73),
	(7,74),
	(9,75),
	(3,77),
	(4,78),
	(6,79),
	(3,80),
	(2,81),
	(1,82),
	(1,83),
	(6,84),
	(1,85),
	(3,86),
	(9,87),
	(5,88),
	(3,89),
	(9,90),
	(3,1),
	(8,2),
	(3,3),
	(5,4),
	(7,5),
	(7,6),
	(2,7),
	(3,8),
	(8,9),
	(7,10),
	(4,11),
	(2,12),
	(1,13),
	(6,15),
	(4,16),
	(5,17),
	(3,18),
	(5,19),
	(2,20),
	(9,21),
	(2,22),
	(4,23),
	(8,24),
	(2,25),
	(1,26),
	(1,27),	
	(1,34),
	(3,35),
	(3,36),
	(4,37),
	(2,38),
	(3,39),
	(4,40),
	(5,41),
	(2,42),
	(9,49),
	(8,50),
	(6,51),
	(9,52),
	(7,53),
	(2,54),
	(6,55),
	(9,56),
	(3,57),
	(8,58);