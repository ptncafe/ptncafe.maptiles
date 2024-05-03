#docker run -v ${PWD}/import:/import -v ${PWD}/mapping:/mapping  -e PGHOST="host.docker.internal" -e PGPORT=5432 -e PGDATABASE="osm_db?sslmode=require" -e PGUSER="postgres" -e PGPASSWORD="CAQ4LujGV1pI1k1a" -e IMPOSM_MAPPING_FILE=/mapping/imposm3-mapping.json --add-host=host.docker.internal:host-gateway openmaptiles/openmaptiles-tools import-osm

docker run -v ${PWD}/import:/import -v ${PWD}/mapping:/mapping \
    -e PGHOST="127.0.0.1" -e PGPORT=25432 -e PGDATABASE="osm_db" -e PGUSER="docker" -e PGPASSWORD="docker" \
    -e IMPOSM_MAPPING_FILE=/mapping/imposm3-mapping.json \
    --net=host \
    openmaptiles/openmaptiles-tools import-osm