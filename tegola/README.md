# Demo tegola with OSM data

tegola-osm<https://github.com/go-spatial/tegola-osm>

## Install extension

In osm_db
```sql
CREATE EXTENSION postgis;
CREATE EXTENSION hstore;
```

## Imposm3

In docker-imposm3  folder

```bash
./docker_run.sh  
```


## Import the OSM Land and Natural Earth dataset

```bash
./natural_earth.sh && ./osm_land.sh
```

## postgis_helpers.sql

In osm_db
```sql
BEGIN;

 -- Inspired by http://stackoverflow.com/questions/16195986/isnumeric-with-postgresql/16206123#16206123
CREATE OR REPLACE FUNCTION as_numeric(text) RETURNS NUMERIC AS $$
DECLARE test NUMERIC;
BEGIN
     test = $1::NUMERIC;
     RETURN test;
EXCEPTION WHEN others THEN
     RETURN -1;
END;
$$ STRICT
LANGUAGE plpgsql IMMUTABLE;

COMMIT;
```

## postgis_index.sql

In osm_db
```sql
BEGIN;
	CREATE INDEX ON osm_transport_lines_gen0 (type);
	CREATE INDEX ON osm_transport_lines_gen1 (type);
	CREATE INDEX ON osm_transport_lines (type);
	CREATE INDEX ON osm_admin_areas (admin_level);
	CREATE INDEX ON osm_landuse_areas_gen0 (type);
	CREATE INDEX ON osm_water_lines (type);
	CREATE INDEX ON osm_water_lines_gen0 (type);
	CREATE INDEX ON osm_water_lines_gen1 (type);
	CREATE INDEX ON osm_water_areas (type);
	CREATE INDEX ON osm_water_areas_gen0 (type);	
	CREATE INDEX ON osm_water_areas_gen1 (type);
COMMIT;
```

## Run tegola

```bash
./tegola serve --config tegola.toml
```
