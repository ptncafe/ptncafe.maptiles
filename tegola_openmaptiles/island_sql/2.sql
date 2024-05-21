DELETE FROM public.osm_island_point
WHERE ID = 9999
INSERT INTO public.osm_island_point(
	id, osm_id, name, name_en, name_de, tags, rank, geometry)
	VALUES (9999,
			-99999999, 
			'Quẩn đảo Trường Sa', 
			'Quẩn đảo Trường Sa', 
						'Quẩn đảo Trường Sa', 

			'"name"=>"Quần đảo Hoàng Sa", "place"=>"island", "natural"=>"coastline", "name_int"=>"Quần đảo Hoàng Sa","name:latin"=>"Quần đảo Hoàng Sa"', 
			 1, 	

			ST_AsText(ST_Transform(ST_GeomFromText('POINT(111.208676975 15.7776774770001)', 4326), 3857))
		   );