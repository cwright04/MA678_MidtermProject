/*Investigate Business Data*/

/*What is the distribution by state? Subset down to open businesses*/
/* Note: MA has the largest count*/
SELECT count(*), state from Business where is_open = '1' group by state ;

/*Look at the type of categories are included*/
SELECT count(*), categories from Business where is_open = '1' ;

/*Look at by state counts once we have subset down to open restaurants/food establishments*/
SELECT count(*), state from Business where is_open = '1' AND categories LIKE '%food%'
   OR categories LIKE '%FOOD%'
   OR categories LIKE '%Food%'
   OR categories LIKE '%RESTAURANT%' 
   OR categories LIKE '%Restaurant%' 
   OR categories LIKE '%restaurant%' group by state;

SELECT * from Business where is_open = '1' AND state = 'MA' AND (categories LIKE '%food%'
   OR categories LIKE '%FOOD%'
   OR categories LIKE '%Food%'
   OR categories LIKE '%RESTAURANT%' 
   OR categories LIKE '%Restaurant%' 
   OR categories LIKE '%restaurant%') 
    ;

/*Look at by city counts once we have subset down to open restaurants/food establishments*/
SELECT count(*), upper(city) from Business where is_open = '1' AND state = 'MA' AND (categories LIKE '%food%'
   OR categories LIKE '%FOOD%'
   OR categories LIKE '%Food%'
   OR categories LIKE '%RESTAURANT%' 
   OR categories LIKE '%Restaurant%' 
   OR categories LIKE '%restaurant%') 
    group by city;
   
/*Create a table of just open MA restaurants*/
CREATE TABLE MA_RESTAURANT AS 
SELECT * from Business 
where is_open = '1' 
AND state = 'MA'
AND (categories LIKE '%RESTAURANT%' 
   OR categories LIKE '%Restaurant%' 
   OR categories LIKE '%restaurant%')
   ;
   

/*Merge Review Data onto MA_RESTAURANT Data*/
CREATE TABLE Review1 AS 
SELECT CAST(substr(date, 1, 4) AS INTEGER) AS year, 
business_id, 
review_id, 
stars, 
useful, 
funny, 
text, 
cool, 
date FROM Review;


CREATE TABLE MA_RESTAURANT_REVIEW AS SELECT r.business_id, r.review_id, r.stars, r.useful, r.funny, r.text, r.cool, r.date, r.year,
b.business_id, b.name, b.address, b.city, b.state, b.postal_code, b.latitude, b.longitude,
b.stars as bus_stars,b.attributes, b.categories,b.hours
FROM Review1 r
INNER JOIN MA_RESTAURANT b
ON r.business_id = b.business_id where r.year >2015;

select count(*) from COVID;

CREATE TABLE COVID_MA AS SELECT c.*, d.business_id 
FROM COVID c 
INNER_JOIN MA_RESTAURANT_REVIEW d
on c.business_id = d.business_id;