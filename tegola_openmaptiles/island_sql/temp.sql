select *, ST_ASTEXT(geometry) from osm_island_polygon 
select ST_ASTEXT(geometry) from osm_island_polygon where name LIKE '%Đá Tây%'
select *,ST_AsText(geometry) from osm_island_point --where name LIKE '%Trường%'

SELECT 
    ST_X(ST_GeomFromText('POINT(12507102.926173223 998296.3584159157)', 3857)) AS longitude,
    ST_Y(ST_GeomFromText('POINT(12507102.926173223 998296.3584159157)', 3857)) AS latitude;

update osm_island_polygon
set rank =1