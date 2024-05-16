# Demo tegola with OSM data

Tegola-omt <https://github.com/dechristopher/tegola-omt>

This example shows how to use `tegola` with `OSM` data using `OpenMapTiles tools`.

## Install extension database

In osm database, execute below script

```sql
CREATE EXTENSION postgis;
CREATE EXTENSION hstore;
```

## Import Open Street Map Data by Imposm3

Download [asia.pbf](https://download.geofabrik.de/asia-latest.osm.pbf) to `file` folder

Run  `import-osm` shell script

```bash
./import-osm
```

## Import Natural Earth Data

Download natural-earth data and relocate file/folder like below folder structure

[natural_earth_vector.sqlite_v5.1.2.zip](https://dev.maptiler.download/geodata/omt/natural_earth_vector.sqlite_v5.1.2.zip)

[water-polygons-split-3857.zip](http://osmdata.openstreetmap.de/download/water-polygons-split-3857.zip)

[lake_centerline.geojson](https://dev.maptiler.download/geodata/omt/lake_centerline.geojson)

```
.
└── import-data/
    └── import/
        ├── lake_centerline/
        │   └── lake_centerline.geojson
        ├── natural_earth/
        │   └── natural_earth_vector.sqlite
        └── water_polygons/
            ├── water_polygons.shp
            └── water_polygons.shx
```

Run ./clean-natural-earth.sh to clear data

```bash
cd import-data
./clean-natural-earth.sh importnatural_earth//natural_earth_vector.sqlite 

```
And
```bash
./import_data.sh
```


## Import sql 

Download [asia.pbf](https://download.geofabrik.de/asia-latest.osm.pbf) to `file` folder

Run  `import-osm` shell script

```bash
cd ..
./import-sql
```

## Run tegola

```bash
./tegola serve --config tegola.toml
```

## Update data

```bash
cd ..
./import-update
```